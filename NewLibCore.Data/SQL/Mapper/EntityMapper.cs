﻿using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.Cache;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Data.SQL.Mapper.MapperExtension;
using NewLibCore.Logger;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 将对应的操作翻译为sql并执行
    /// </summary>
    public sealed class EntityMapper : IDisposable
    {
        private readonly IMapperDbContext _mapperDbContext;
        private readonly IServiceScope _serviceScope;

        private EntityMapper()
        {
            var services = new ServiceCollection()
               .AddTransient<ResultCache, ExecutionResultCache>()
               .AddTransient<StatementStore>()
               .AddScoped<IMapperDbContext, MapperDbContext>()
               .AddSingleton<ILogger, ConsoleLogger>();

            if (MapperConfig.MapperType == MapperType.MSSQL)
            {
                services = services.AddTransient<InstanceConfig, MsSqlInstanceConfig>();
            }
            else if (MapperConfig.MapperType == MapperType.MYSQL)
            {
                services = services.AddTransient<InstanceConfig, MySqlInstanceConfig>();
            }
            var serviceProvider = services.BuildServiceProvider();
            _serviceScope = serviceProvider.CreateScope();
            MapperConfig.ServiceProvider = _serviceScope.ServiceProvider;
        } 

        public static EntityMapper CreateMapper()
        {
            return new EntityMapper();
        }

        /// <summary>
        /// 添加一個TModel
        /// </summary>
        /// <param name="model">要新增的对象</param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public TModel Add<TModel>(TModel model) where TModel : EntityBase, new()
        {
            Parameter.Validate(model);

            return RunDiagnosis.Watch(() =>
            {
                Handler handler = new InsertHandler<TModel>(model);
                model.Id = handler.Execute().FirstOrDefault<Int32>();
                return model;
            });
        }

        /// <summary>
        /// 修改一個TModel
        /// </summary>
        /// <param name="model">要修改的对象</param>
        /// <param name="expression">查询条件</param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public Boolean Update<TModel>(TModel model, Expression<Func<TModel, Boolean>> expression) where TModel : EntityBase, new()
        {
            Parameter.Validate(model);
            Parameter.Validate(expression);

            return RunDiagnosis.Watch(() =>
            {
                var segmentManager = MapperConfig.ServiceProvider.GetService<StatementStore>();
                segmentManager.Add(expression);
                Handler handler = new UpdateHandler<TModel>(model, segmentManager);
                return handler.Execute().FirstOrDefault<Int32>() > 0;
            });
        }

        /// <summary>
        /// 查询一個TModel
        /// </summary>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public IJoin<TModel> Query<TModel>() where TModel : new()
        {
            var segmentManager = MapperConfig.ServiceProvider.GetService<StatementStore>();
            segmentManager.Add<TModel>();
            return new Join<TModel>(segmentManager);
        }

        /// <summary>
        /// 执行一個返回列表的sql语句
        /// </summary>
        /// <param name="sql">sql语句</param>
        /// <param name="parameters">实体参数</param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public RawResult SqlQuery(String sql, IEnumerable<EntityParameter> parameters = null)
        {
            Parameter.Validate(sql);

            return RunDiagnosis.Watch(() =>
            {
                var sqlResult = TranslateResult.CreateResult();
                sqlResult.Append(sql, parameters);
                return sqlResult.Execute();
            });
        }

        public void OpenTransaction()
        {
            _mapperDbContext.OpenTransaction();
        }

        public void Commit()
        {
            _mapperDbContext.Commit();
        }

        public void Rollback()
        {
            _mapperDbContext.Rollback();
        }

        public void Dispose()
        {
            _mapperDbContext.Dispose();
            _serviceScope.Dispose();
        }
    }
}

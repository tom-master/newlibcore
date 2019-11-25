using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.DbContext;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 将对应的操作翻译为sql并执行
    /// </summary>
    public sealed class EntityMapper : IDisposable
    {
        private readonly IServiceScope _serviceScope;

        private EntityMapper()
        {
            var services = new ServiceCollection().AddScoped<MapperDbContextBase, MapperDbContext>();

            if (MapperConfig.MapperType == MapperType.MSSQL)
            {
                services = services.AddScoped<TemplateBase, MsSqlTemplate>();
            }
            else if (MapperConfig.MapperType == MapperType.MYSQL)
            {
                services = services.AddScoped<TemplateBase, MySqlTemplate>();
            }
            var serviceProvider = services.BuildServiceProvider();
            _serviceScope = serviceProvider.CreateScope();
        }

        /// <summary>
        /// 初始化EntityMapper类的新实例
        /// </summary>
        /// <returns></returns>
        public static EntityMapper CreateMapper()
        {
            return new EntityMapper();
        }

        /// <summary>
        /// 添加
        /// </summary>
        /// <param name="model">要新增的对象</param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public TModel Add<TModel>(TModel model) where TModel : EntityBase, new()
        {
            Parameter.Validate(model);

            return RunDiagnosis.Watch(() =>
            {
                Handler handler = new InsertHandler<TModel>(model, _serviceScope.ServiceProvider);
                model.Id = handler.Execute().FirstOrDefault<Int32>();
                return model;
            });
        }

        /// <summary>
        /// 修改
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
                Handler handler = new UpdateHandler<TModel>(model, expression, _serviceScope.ServiceProvider);
                return handler.Execute().FirstOrDefault<Int32>() > 0;
            });
        }

        /// <summary>
        /// 查询
        /// </summary>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public QueryWrapper<TModel> Query<TModel>() where TModel : new()
        {
            var expressionStore = new ExpressionStore();
            expressionStore.AddFrom<TModel>();
            return new QueryWrapper<TModel>(expressionStore, _serviceScope.ServiceProvider);
        }

        /// <summary>
        /// 执行原生的SQL语句
        /// </summary>
        /// <param name="sql">sql语句</param>
        /// <param name="parameters">实体参数</param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public RawResult SqlQuery(String sql, IEnumerable<MapperParameter> parameters = null)
        {
            Parameter.Validate(sql);

            return RunDiagnosis.Watch(() =>
            {
                Handler handler = new DirectSqlHandler(sql, parameters, _serviceScope.ServiceProvider);
                return handler.Execute();
            });
        }

        public void Commit()
        {
            _serviceScope.ServiceProvider.GetService<MapperDbContextBase>().Commit();
        }

        public void Rollback()
        {
            _serviceScope.ServiceProvider.GetService<MapperDbContextBase>().Rollback();
        }

        public void OpenTransaction()
        {
            _serviceScope.ServiceProvider.GetService<MapperDbContextBase>().UseTransaction = true;
        }

        /// <summary>
        /// 释放资源
        /// </summary>
        public void Dispose()
        {
            _serviceScope.Dispose();
        }
    }
}

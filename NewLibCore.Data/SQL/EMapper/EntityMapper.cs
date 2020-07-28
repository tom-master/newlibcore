﻿using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Linq.Expressions;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Component.Cache;
using NewLibCore.Data.SQL.Extension;
using NewLibCore.Data.SQL.ProcessorFactory;
using NewLibCore.Data.SQL.Store;
using NewLibCore.Data.SQL.Template;
using NewLibCore.Logger;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL
{
    /// <summary>
    /// 将对应的操作翻译为sql并执行
    /// </summary>
    public sealed class EntityMapper : IDisposable
    {
        private static ILogger _logger;

        private static QueryCacheBase _queryCacheBase;

        private IServiceProvider _serviceProvider;

        /// <summary>
        /// 连接字符串名称
        /// </summary>
        /// <value></value>
        public static String ConnectionStringName { get; set; }

        /// <summary>
        /// 是否在出现异常时抛出异常
        /// </summary>
        /// <value></value>
        public static Boolean ThrowException { get; set; } = true;

        /// <summary>
        /// 启用模型验证
        /// </summary> 
        /// <value></value>
        public static Boolean EnableModelValidate { get; set; }

        /// <summary>
        /// 事务隔离级别
        /// </summary>
        internal static IsolationLevel TransactionLevel { get; set; }

        /// <summary>
        /// 映射的数据库类型
        /// </summary>
        internal static MapperType MapperType { get; set; }

        /// <summary>
        /// mssql的版本
        /// </summary>
        internal static MsSqlPaginationVersion MsSqlPaginationVersion { get; set; } = MsSqlPaginationVersion.NONE;

        /// <summary>
        /// 初始化默认配置
        /// </summary>
        public static void InitDefaultSetting()
        {
            UseMySql();
            SetTransactionLevel(IsolationLevel.Unspecified);
            EnableModelValidate = true;
        }

        /// <summary>
        /// 初始化依赖注入
        /// </summary>
        private void InitDependency()
        {
            IServiceCollection services = new ServiceCollection();

            #region scoped

            if (MapperType == MapperType.MSSQL)
            {
                services = services.AddScoped<TemplateBase, MsSqlTemplate>();
            }
            else if (MapperType == MapperType.MYSQL)
            {
                services = services.AddScoped<TemplateBase, MySqlTemplate>();
            }

            services = services.AddScoped<MapperDbContextBase, MapperDbContext>();

            #endregion

            #region singleton

            if (_queryCacheBase == null)
            {
                services = services.AddSingleton<QueryCacheBase, DefaultQueryCache>();
            }
            else
            {
                services = services.AddSingleton(_queryCacheBase.GetType());
            }

            RunDiagnosis.SetLoggerInstance(_logger ?? new DefaultLogger());
            #endregion

            #region transient

            services = services.AddTransient<ParserExecutor, DefaultParserExecutor>();
            services = services.AddTransient<ParserResult>();

            services = services.AddTransient<Processor, RawSqlProcessor>();
            services = services.AddTransient<Processor, QueryProcessor>();
            services = services.AddTransient<Processor, UpdateProcessor>();
            services = services.AddTransient<Processor, InsertProcessor>();

            #endregion

            _serviceProvider = services.BuildServiceProvider();
        }


        /// <summary>
        /// 切换为mysql
        /// </summary>
        public static void UseMySql()
        {
            MapperType = MapperType.MYSQL;
        }

        /// <summary>
        /// 切换为mssql
        /// </summary>
        public static void UseMsSql()
        {
            MapperType = MapperType.MSSQL;
        }

        /// <summary>
        /// 设置事务隔离级别
        /// </summary>
        /// <param name="isolationLevel"></param>
        public static void SetTransactionLevel(IsolationLevel isolationLevel)
        {
            TransactionLevel = isolationLevel;
        }

        /// <summary>
        /// 设置自定义日志记录组件
        /// </summary>
        /// <param name="logger"></param>
        public static void SetLogger(ILogger logger)
        {
            Parameter.Validate(logger);
            _logger = logger;
        }

        /// <summary>
        /// 设置自定义查询缓存组件
        /// </summary>
        /// <param name="cache"></param>
        public static void SetCache(QueryCacheBase cache)
        {
            Parameter.Validate(cache);
            _queryCacheBase = cache;
        }

        private EntityMapper()
        {
            InitDependency();
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
                var store = new ExpressionStore();
                store.AddModel(model);

                var processor = FindProcessor(_serviceProvider.GetServices<Processor>(), nameof(InsertProcessor));
                model.Id = processor.Process(store).GetModifyRowCount();
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
                var store = new ExpressionStore();
                store.AddWhere(expression);
                store.AddModel(model);
                var processor = FindProcessor(_serviceProvider.GetServices<Processor>(), nameof(UpdateProcessor));
                return processor.Process(store).GetModifyRowCount() > 0;
            });
        }

        /// <summary>
        /// 查询
        /// </summary>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public QueryWrapper<TModel> Query<TModel>() where TModel : EntityBase, new()
        {
            var expressionStore = new ExpressionStore();
            expressionStore.AddFrom<TModel>();

            var processor = FindProcessor(_serviceProvider.GetServices<Processor>(), nameof(QueryProcessor));
            return new QueryWrapper<TModel>(expressionStore, processor);
        }

        /// <summary>
        /// 执行原生的SQL语句
        /// </summary>
        /// <param name="sql">sql语句</param>
        /// <param name="parameters">实体参数</param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public ExecuteResult SqlQuery(String sql, params MapperParameter[] parameters)
        {
            Parameter.Validate(sql);

            return RunDiagnosis.Watch(() =>
            {
                var store = new ExpressionStore();
                store.AddDirectSql(sql, parameters);
                var processor = FindProcessor(_serviceProvider.GetServices<Processor>(), nameof(RawSqlProcessor));
                return processor.Process(store);
            });
        }

        public void Commit()
        {
            _serviceProvider.GetService<MapperDbContextBase>().Commit();
        }

        public void Rollback()
        {
            _serviceProvider.GetService<MapperDbContextBase>().Rollback();
        }

        public void OpenTransaction()
        {
            _serviceProvider.GetService<MapperDbContextBase>().UseTransaction = true;
        }

        /// <summary>
        /// 释放资源
        /// </summary>
        public void Dispose()
        {
            (_serviceProvider as ServiceProvider).Dispose();
        }

        private Processor FindProcessor(IEnumerable<Processor> source, String target)
        {
            Parameter.Validate(source);
            Parameter.Validate(target);
            var result = source.FirstOrDefault(w => w.CurrentId == target);
            if (result != null)
            {
                return result;
            }
            throw new ArgumentException($@"没有找到{target}所注册的实现类");
        }
    }
}
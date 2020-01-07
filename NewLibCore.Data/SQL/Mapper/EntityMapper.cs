using System;
using System.Data;
using System.Linq.Expressions;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.Component.Cache;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Handler;
using NewLibCore.Data.SQL.Mapper.Store;
using NewLibCore.Data.SQL.Mapper.Template;
using NewLibCore.Logger;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 将对应的操作翻译为sql并执行
    /// </summary>
    public sealed class EntityMapper : IDisposable
    {

        private static ILogger _logger;

        private static QueryCacheBase _queryCacheBase;

        private IServiceScope _serviceScope;

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
        private void InitDi()
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
            services = services.AddScoped<DirectSqlHandler>();
            services = services.AddScoped<QueryHandler>();
            services = services.AddScoped<UpdateHandler>();
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

            if (_logger == null)
            {
                services = services.AddSingleton<ILogger, DefaultLogger>();
            }
            else
            {
                services = services.AddSingleton(_logger.GetType());
            }

            services = services.AddSingleton<RunDiagnosis>();
            #endregion

            #region transient

            services = services.AddTransient<ParserExecutor, DefaultParserExecutor>();
            services = services.AddTransient<ParserResult>();
            #endregion


            _serviceScope = services.BuildServiceProvider().CreateScope();
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
            InitDi();
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

            return _serviceScope.ServiceProvider.GetService<RunDiagnosis>().Watch(() =>
            {
                var handler = _serviceScope.ServiceProvider.GetService<InsertHandler>();
                model.Id = handler.Execute(model).FirstOrDefault<Int32>();
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

            return _serviceScope.ServiceProvider.GetService<RunDiagnosis>().Watch(() =>
            {
                var expressionStore = new ExpressionStore();
                expressionStore.AddWhere(expression);
                var handler = _serviceScope.ServiceProvider.GetService<UpdateHandler>();
                return handler.Execute(model, expressionStore).FirstOrDefault<Int32>() > 0;
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

            var diagnosis = _serviceScope.ServiceProvider.GetService<RunDiagnosis>();
            var queryCacheBase = _serviceScope.ServiceProvider.GetService<QueryCacheBase>();
            var queryHandler = _serviceScope.ServiceProvider.GetService<QueryHandler>();

            return new QueryWrapper<TModel>(expressionStore, diagnosis, queryCacheBase, queryHandler);
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

            return _serviceScope.ServiceProvider.GetService<RunDiagnosis>().Watch(() =>
            {
                var handler = _serviceScope.ServiceProvider.GetService<DirectSqlHandler>();
                return handler.Execute(sql, parameters);
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

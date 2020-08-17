using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.EMapper;
using NewLibCore.Data.SQL.Extension;
using NewLibCore.Data.SQL.ProcessorFactory;
using NewLibCore.Data.SQL.Store;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL
{
    /// <summary>
    /// 将对应的操作翻译为sql并执行
    /// </summary>
    public sealed class EntityMapper : IDisposable
    {

        private EntityMapper()
        {
            EntityMapperConfig.InitDependency();
        }

        static EntityMapper()
        {
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
            Parameter.IfNullOrZero(model);

            return RunDiagnosis.Watch(() =>
            {
                var store = new ExpressionStore();
                store.AddModel(model);

                var processor = FindProcessor(nameof(InsertProcessor));
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
            Parameter.IfNullOrZero(model);
            Parameter.IfNullOrZero(expression);

            return RunDiagnosis.Watch(() =>
            {
                var store = new ExpressionStore();
                store.AddWhere(expression);
                store.AddModel(model);
                var processor = FindProcessor(nameof(UpdateProcessor));
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

            var processor = FindProcessor(nameof(QueryProcessor));
            return new QueryWrapper<TModel>(expressionStore, processor);
        }

        /// <summary>
        /// 执行原生的SQL语句
        /// </summary>
        /// <param name="sql">sql语句</param>
        /// <param name="parameters">实体参数</param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public SqlExecuteResultConvert SqlQuery(String sql, params MapperParameter[] parameters)
        {
            Parameter.IfNullOrZero(sql);

            return RunDiagnosis.Watch(() =>
            {
                var store = new ExpressionStore();
                store.AddDirectSql(sql, parameters);
                var processor = FindProcessor(nameof(RawSqlProcessor));
                return processor.Process(store);
            });
        }

        public void Commit()
        {
            EntityMapperConfig.Provider.GetService<MapperDbContextBase>().Commit();
        }

        public void Rollback()
        {
            EntityMapperConfig.Provider.GetService<MapperDbContextBase>().Rollback();
        }

        public void OpenTransaction()
        {
            EntityMapperConfig.Provider.GetService<MapperDbContextBase>().UseTransaction = true;
        }

        /// <summary>
        /// 释放资源
        /// </summary>
        public void Dispose()
        {
            (EntityMapperConfig.Provider as ServiceProvider).Dispose();
        }

        private Processor FindProcessor(String target)
        {
            Parameter.IfNullOrZero(target);
            var result = EntityMapperConfig.Provider.GetServices<Processor>().FirstOrDefault(w => w.CurrentId == target);
            if (result != null)
            {
                return result;
            }
            throw new ArgumentException($@"没有找到{target}所注册的实现类");
        }
    }
}

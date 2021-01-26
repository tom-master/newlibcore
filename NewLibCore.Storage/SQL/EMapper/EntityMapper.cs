using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Storage.SQL.ProcessorFactory;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL
{
    /// <summary>
    /// 将对应的操作翻译为sql并执行
    /// </summary>
    public sealed class EntityMapper
    {
        private readonly MapperDbContextBase _dbContextBase;
        public EntityMapper(MapperDbContextBase mapperDbContextBase)
        {
            _dbContextBase = mapperDbContextBase;
        }

        /// <summary>
        /// 添加
        /// </summary>
        /// <param name="model">要新增的对象</param>
        /// <typeparam name="TModel"></typeparam>
        /// <returns></returns>
        public TModel Add<TModel>(TModel model) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(model);

            return RunDiagnosis.Watch(() =>
            {
                var sqlComponent = new SqlComponent();
                sqlComponent.AddModel(model);

                var processor = FindProcessor(nameof(InsertProcessor));
                model.Id = processor.Process(sqlComponent).GetModifyRowCount();
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
            Check.IfNullOrZero(model);
            Check.IfNullOrZero(expression);

            return RunDiagnosis.Watch(() =>
            {
                var sqlComponent = new SqlComponent();
                sqlComponent.AddWhere(expression);
                sqlComponent.AddModel(model);
                var processor = FindProcessor(nameof(UpdateProcessor));
                return processor.Process(sqlComponent).GetModifyRowCount() > 0;
            });
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
            Check.IfNullOrZero(sql);

            return RunDiagnosis.Watch(() =>
            {
                var sqlComponent = new SqlComponent();
                sqlComponent.AddDirectSql(sql, parameters);
                var processor = FindProcessor(nameof(RawSqlProcessor));
                return processor.Process(sqlComponent);
            });
        }

        public void Commit()
        {
            _dbContextBase.Commit();
        }

        public void Rollback()
        {
            _dbContextBase.Rollback();
        }

        public void OpenTransaction()
        {
            _dbContextBase.UseTransaction = true;
        }

        private Processor FindProcessor(String target)
        {
            Check.IfNullOrZero(target);
            var result = _serviceProvider.GetServices<Processor>().FirstOrDefault(w => w.CurrentId == target);
            if (result != null)
            {
                return result;
            }
            throw new ArgumentException($@"没有找到{target}所注册的实现类");
        }
    }
}

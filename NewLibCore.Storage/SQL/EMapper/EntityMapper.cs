using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Storage.SQL.Component.Sql;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Validate;
using System;
using System.Linq;
using System.Linq.Expressions;

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
                var insertComponent = new InsertComponent<TModel>(model);
                var processor = FindProcessor(nameof(InsertProcessor));
                model.Id = processor.Process(insertComponent).GetModifyRowCount();
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
                var updateComponent = new UpdateComponent<TModel>(model, new WhereComponent(expression));
                var processor = FindProcessor(nameof(UpdateProcessor));
                return processor.Process(updateComponent).GetModifyRowCount() > 0;
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

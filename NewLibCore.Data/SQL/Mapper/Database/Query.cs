using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.MapperExtension
{
    public interface IQuery<TModel> where TModel : new()
    {
        IQuery<TModel> Where(Expression<Func<TModel, Boolean>> expression);

        IQuery<TModel> Where<T>(Expression<Func<T, Boolean>> expression) where T : new();

        IQuery<TModel> Where<T>(Expression<Func<TModel, T, Boolean>> expression) where T : new();

        IQuery<TModel> Select(Expression<Func<TModel, dynamic>> fields = null);

        IQuery<TModel> Page(Int32 pageIndex, Int32 pageSize);

        TModel FirstOrDefault();

        T FirstOrDefault<T>() where T : new();

        List<TModel> ToList();

        List<T> ToList<T>() where T : new();
    }

    public class Query<TModel> : IQuery<TModel> where TModel : new()
    {
        private readonly SegmentManager _segmentManager;
        private readonly IMapperDbContext _mapperDbContext;

        internal Query(SegmentManager segmentManager, IMapperDbContext mapperDbContext)
        {
            _segmentManager = segmentManager;
            _mapperDbContext = mapperDbContext;
        }

        public IQuery<TModel> Page(Int32 pageIndex, Int32 pageSize)
        {
            Parameter.Validate(pageIndex);
            Parameter.Validate(pageSize);
            _segmentManager.AddPage(pageIndex, pageSize);

            return this;
        }

        public IQuery<TModel> Select(Expression<Func<TModel, dynamic>> fields = null)
        {
            if (fields != null)
            {
                _segmentManager.Add(fields);
            }

            return this;
        }

        public IQuery<TModel> Where(Expression<Func<TModel, Boolean>> expression)
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression);
            return this;
        }

        public IQuery<TModel> Where<T>(Expression<Func<T, Boolean>> expression) where T : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression);

            return this;
        }

        public IQuery<TModel> Where<T>(Expression<Func<TModel, T, Boolean>> expression) where T : new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression);

            return this;
        }

        public TModel FirstOrDefault()
        {
            return RunDiagnosis.Watch(() =>
            {
                var executeResult = InternalExecuteSql();
                return executeResult.ToSingle<TModel>();
            });
        }

        public T FirstOrDefault<T>() where T : new()
        {
            return RunDiagnosis.Watch(() =>
            {
                var executeResult = InternalExecuteSql();
                return executeResult.ToSingle<T>();
            });
        }

        public List<TModel> ToList()
        {
            return RunDiagnosis.Watch(() =>
            {
                var executeResult = InternalExecuteSql();
                return executeResult.ToList<TModel>();
            });
        }

        public List<T> ToList<T>() where T : new()
        {
            return RunDiagnosis.Watch(() =>
            {
                var executeResult = InternalExecuteSql();
                return executeResult.ToList<T>();
            });
        }

        private RawExecuteResult InternalExecuteSql()
        {
            Handler handler = new QueryHandler<TModel>(_segmentManager, _mapperDbContext);
            return handler.Execute();
        }
    }
}

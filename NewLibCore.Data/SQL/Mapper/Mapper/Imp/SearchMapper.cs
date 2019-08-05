using System;
using System.Collections.Generic;
using System.Data;
using System.Diagnostics;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Builder;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;
using Newtonsoft.Json;

namespace NewLibCore.Data.SQL.Mapper.Mapper.Imp
{
    internal class SearchMapper<TModel> : ISearchMapper<TModel> where TModel : EntityBase, new()
    {
        private readonly SegmentManager _segmentManager = new SegmentManager();

        public Boolean Exist()
        {
            return Count() > 0;
        }

        public Int32 Count()
        {
            return Watch<Int32>(() =>
            {
                Select(s => "COUNT(*)");
                var executeResult = InternalExecuteSql();
                Int32.TryParse(executeResult.Value.ToString(), out var count);
                return count;
            });
        }

        public TModel FirstOrDefault()
        {
            return Watch<TModel>(() =>
            {
                var executeResult = InternalExecuteSql();
                var dataTable = executeResult.Value as DataTable;
                return dataTable.ToSingle<TModel>();
            });
        }

        public List<TModel> ToList()
        {
            return Watch<List<TModel>>(() =>
            {
                var executeResult = InternalExecuteSql();
                var dataTable = executeResult.Value as DataTable;
                return dataTable.ToList<TModel>();
            });
        }

        public ISearchMapper<TModel> Select<T>(Expression<Func<TModel, T, dynamic>> fields = null) where T : EntityBase, new()
        {
            if (fields != null)
            {
                _segmentManager.Add(fields);
            }

            return this;
        }

        public ISearchMapper<TModel> Select(Expression<Func<TModel, dynamic>> fields = null)
        {
            if (fields != null)
            {
                _segmentManager.Add(fields);
            }

            return this;
        }

        public ISearchMapper<TModel> Where(Expression<Func<TModel, Boolean>> expression)
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression);

            return this;
        }

        public ISearchMapper<TModel> Where<T>(Expression<Func<T, Boolean>> expression) where T : EntityBase, new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression);

            return this;
        }

        public ISearchMapper<TModel> Where<T>(Expression<Func<TModel, T, Boolean>> expression) where T : EntityBase, new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression);

            return this;
        }

        public ISearchMapper<TModel> Page(Int32 pageIndex, Int32 pageSize)
        {
            Parameter.Validate(pageIndex);
            Parameter.Validate(pageSize);
            _segmentManager.AddPage(pageIndex, pageSize);
            return this;
        }

        public ISearchMapper<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.LEFT);

            return this;
        }

        public ISearchMapper<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.RIGHT);

            return this;
        }

        public ISearchMapper<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.INNER);

            return this;
        }

        public ISearchMapper<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression)
          where TLeft : EntityBase, new()
          where TRight : EntityBase, new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.LEFT);

            return this;
        }

        public ISearchMapper<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression)
            where TLeft : EntityBase, new()
            where TRight : EntityBase, new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.RIGHT);

            return this;
        }

        public ISearchMapper<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression)
            where TLeft : EntityBase, new()
            where TRight : EntityBase, new()
        {
            Parameter.Validate(expression);
            _segmentManager.Add(expression, JoinType.INNER);

            return this;
        }

        public ISearchMapper<TModel> OrderByDesc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : EntityBase, new()
        {
            Parameter.Validate(order);
            _segmentManager.AddOrderBy(order, OrderByType.DESC);

            return this;
        }

        public ISearchMapper<TModel> OrderByAsc<TOrder, TKey>(Expression<Func<TOrder, TKey>> order) where TOrder : EntityBase, new()
        {
            Parameter.Validate(order);
            _segmentManager.AddOrderBy(order, OrderByType.ASC);

            return this;
        }

        private RawExecuteResult InternalExecuteSql()
        {
            Handler<TModel> builder = new SelectHandler<TModel>(_segmentManager);
            var segmentResult = builder.GetSegmentResult();
            var executeResult = segmentResult.GetExecuteResult();
            MapperConfig.DatabaseConfig.Logger.Info($@"查询后的结果:{JsonConvert.SerializeObject(executeResult.Value)}");
            return executeResult;
        }

        private T Watch<T>(Func<Object> func)
        {
            var sw = new Stopwatch();
            sw.Start();
            var returnValue = (T)func();
            sw.Stop();
            MapperConfig.DatabaseConfig.Logger.Info($@"共花费{Math.Round(sw.Elapsed.TotalSeconds, 2)}s");

            return returnValue;
        }
    }
}
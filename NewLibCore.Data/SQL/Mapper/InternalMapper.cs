using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Builder;
using NewLibCore.Data.SQL.Mapper.Execute;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Translation;
using NewLibCore.InternalExtension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{

    internal class SqlExecutor : ISqlExecutor
    {
        private ExecuteCore _executeCore;

        public SqlExecutor(ExecuteCore executeCore)
        {
            _executeCore = executeCore;
        }

        public DataTable Execute(String sql, IEnumerable<EntityParameter> parameters = null)
        {
            ExecuteCoreResult executeResult;
            executeResult = _executeCore.Execute(ExecuteType.SELECT, sql, parameters, CommandType.Text);
            var dataTable = (DataTable)executeResult.Value;
            return dataTable;
        }

    }

    internal class SelectEntityMapper<TModel> : ISelectEntityMapper<TModel> where TModel : EntityBase, new()
    {
        private ExecuteCore _execute;
        private StatementStore _statementStore;

        public SelectEntityMapper(ExecuteCore executeCore)
        {
            _execute = executeCore;
            _statementStore = new StatementStore();
        }

        public Boolean Exist()
        {
            return Count() > 0;
        }

        public Int32 Count()
        {
            Select(s => "COUNT(*)");
            var executeResult = InternalExecuteSql(ExecuteType.SELECT_SINGLE);
            Int32.TryParse(executeResult.Value.ToString(), out var count);
            return count;
        }

        public TModel FirstOrDefault()
        {
            var executeResult = InternalExecuteSql(ExecuteType.SELECT);
            var dataTable = executeResult.Value as DataTable;
            return dataTable.ToSingle<TModel>();
        }

        public List<TModel> ToList()
        {
            var executeResult = InternalExecuteSql(ExecuteType.SELECT);
            var dataTable = executeResult.Value as DataTable;
            return dataTable.ToList<TModel>().ToList();
        }

        public ISelectEntityMapper<TModel> Select<T>(Expression<Func<TModel, T, dynamic>> fields = null) where T : EntityBase, new()
        {
            if (fields != null)
            {
                _statementStore.Add(fields);
            }
            return this;
        }

        public ISelectEntityMapper<TModel> Select(Expression<Func<TModel, dynamic>> fields = null)
        {
            if (fields != null)
            {
                _statementStore.Add(fields);
            }
            return this;
        }

        public ISelectEntityMapper<TModel> Where(Expression<Func<TModel, Boolean>> expression = null)
        {
            return Where<TModel>(expression);
        }

        public ISelectEntityMapper<TModel> Where<T>(Expression<Func<T, Boolean>> expression = null) where T : EntityBase, new()
        {
            if (expression != null)
            {
                _statementStore.Add(expression);
            }
            return this;
        }

        public ISelectEntityMapper<TModel> Where<T>(Expression<Func<TModel, T, Boolean>> expression = null) where T : EntityBase, new()
        {
            if (expression != null)
            {
                _statementStore.Add(expression);
            }
            return this;
        }

        public ISelectEntityMapper<TModel> Page(Int32 pageIndex, Int32 pageSize)
        {
            _statementStore.AddPage(pageIndex, pageSize);
            return this;
        }

        public ISelectEntityMapper<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new()
        {
            Parameter.Validate(expression);
            _statementStore.Add(expression, JoinType.LEFT);
            return this;
        }

        public ISelectEntityMapper<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new()
        {
            Parameter.Validate(expression);
            _statementStore.Add(expression, JoinType.RIGHT);
            return this;
        }

        public ISelectEntityMapper<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : EntityBase, new()
        {
            Parameter.Validate(expression);
            _statementStore.Add(expression, JoinType.INNER);
            return this;
        }

        public ISelectEntityMapper<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression)
          where TLeft : EntityBase, new()
          where TRight : EntityBase, new()
        {
            Parameter.Validate(expression);
            _statementStore.Add(expression, JoinType.LEFT);
            return this;
        }

        public ISelectEntityMapper<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression)
            where TLeft : EntityBase, new()
            where TRight : EntityBase, new()
        {
            Parameter.Validate(expression);
            _statementStore.Add(expression, JoinType.RIGHT);
            return this;
        }

        public ISelectEntityMapper<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression)
            where TLeft : EntityBase, new()
            where TRight : EntityBase, new()
        {
            Parameter.Validate(expression);
            _statementStore.Add(expression, JoinType.INNER);
            return this;
        }

        public ISelectEntityMapper<TModel> OrderBy<TOrder, TKey>(Expression<Func<TOrder, TKey>> order, OrderByType orderBy = OrderByType.DESC) where TOrder : EntityBase, new()
        {
            Parameter.Validate(order);
            _statementStore.AddOrderBy(order, orderBy);
            return this;
        }

        private ExecuteCoreResult InternalExecuteSql(ExecuteType executeType)
        {
            IBuilder<TModel> builder = new SelectBuilder<TModel>(_statementStore);
            _statementStore.ExecuteType = executeType;
            var executeResult = _execute.Execute(executeType, builder.Build());
            return executeResult;
        }
    }

    internal class UpdateEntityMapper<TModel> : IUpdateEntityMapper<TModel> where TModel : EntityBase, new()
    {
        private ExecuteCore _execute;
        private StatementStore _statementStore;

        public UpdateEntityMapper(ExecuteCore executeCore)
        {
            _execute = executeCore;
            _statementStore = new StatementStore();
        }

        public Boolean Update(TModel model, Expression<Func<TModel, Boolean>> expression)
        {
            _statementStore.Add(expression);
            _statementStore.ExecuteType = ExecuteType.UPDATE;
            IBuilder<TModel> builder = new ModifyBuilder<TModel>(model, _statementStore, true);
            var executeResult = _execute.Execute(ExecuteType.UPDATE, builder.Build());
            return (Int32)executeResult.Value > 0;
        }
    }

    internal class AddEntityMapper<TModel> : IAddEntityMapper<TModel> where TModel : EntityBase, new()
    {
        private readonly ExecuteCore _executeCore;

        public AddEntityMapper(ExecuteCore executeCore)
        {
            _executeCore = executeCore;
        }

        public TModel Add(TModel model)
        {
            IBuilder<TModel> builder = new AddBuilder<TModel>(model, true);
            var executeResult = _executeCore.Execute(ExecuteType.INSERT, builder.Build());
            model.Id = Int32.Parse(executeResult.Value.ToString());
            return model;
        }
    }

}

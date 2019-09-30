using System;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.MapperExtension
{

    public interface IJoin<TModel> : IQuery<TModel> where TModel : new()
    {
        IJoin<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new();

        IJoin<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : new() where TRight : new();

        IJoin<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new();

        IJoin<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : new() where TRight : new();

        IJoin<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new();

        IJoin<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : new() where TRight : new();
    }

    public class Join<TModel> : Query<TModel>, IJoin<TModel> where TModel : new()
    {
        private readonly StatementStore _statementStore;
        internal Join(StatementStore statementStore, IMapperDbContext mapperDbContext) : base(statementStore, mapperDbContext)
        {
            _statementStore = statementStore;
        }

        public IJoin<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            _statementStore.Add(expression, JoinRelation.LEFT);

            return this;
        }

        public IJoin<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : new() where TRight : new()
        {
            Parameter.Validate(expression);
            _statementStore.Add(expression, JoinRelation.LEFT);

            return this;
        }

        public IJoin<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            _statementStore.Add(expression, JoinRelation.RIGHT);

            return this;
        }

        public IJoin<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : new() where TRight : new()
        {
            Parameter.Validate(expression);
            _statementStore.Add(expression, JoinRelation.RIGHT);

            return this;
        }

        public IJoin<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> expression) where TRight : new()
        {
            Parameter.Validate(expression);
            _statementStore.Add(expression, JoinRelation.INNER);

            return this;
        }

        public IJoin<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> expression) where TLeft : new() where TRight : new()
        {
            Parameter.Validate(expression);
            _statementStore.Add(expression, JoinRelation.INNER);

            return this;
        }
    }
}

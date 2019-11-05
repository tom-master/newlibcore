using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.MapperExtension
{

    public static class EntityMapperExtension
    {
        private static readonly ExpressionStore _expressionStore;

        static EntityMapperExtension()
        {
            _expressionStore = new ExpressionStore();
        }

        public static EntityMapper LeftJoin<TLeft, TRight>(this EntityMapper entityMapper, Expression<Func<TLeft, TRight, Boolean>> expression)
        where TLeft : new()
         where TRight : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression, JoinRelation.LEFT);

            return entityMapper;
        }



        public static EntityMapper RightJoin<TLeft, TRight>(this EntityMapper entityMapper, Expression<Func<TLeft, TRight, Boolean>> expression)
        where TLeft : new()
        where TRight : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression, JoinRelation.RIGHT);

            return entityMapper;
        }


        public static EntityMapper InnerJoin<TLeft, TRight>(this EntityMapper entityMapper, Expression<Func<TLeft, TRight, Boolean>> expression)
        where TLeft : new()
        where TRight : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression, JoinRelation.INNER);

            return entityMapper;
        }


        public static EntityMapper Page(this EntityMapper entityMapper, Int32 pageIndex, Int32 pageSize)
        {
            Parameter.Validate(pageIndex);
            Parameter.Validate(pageSize);
            _expressionStore.AddPage(pageIndex, pageSize);
            return entityMapper;
        }

        public static EntityMapper Select<TModel>(this EntityMapper entityMapper, Expression<Func<TModel, dynamic>> fields = null)
        where TModel : new()
        {
            if (fields != null)
            {
                _expressionStore.Add(fields);
            }

            return entityMapper;
        }

        public static EntityMapper Select<TModel1, TModel2>(this EntityMapper entityMapper, Expression<Func<TModel1, TModel2, dynamic>> fields = null)
        where TModel1 : new()
        where TModel2 : new()
        {
            if (fields != null)
            {
                _expressionStore.Add(fields);
            }
            return entityMapper;
        }
        public static EntityMapper Select<TModel1, TModel2, TModel3>(this EntityMapper entityMapper, Expression<Func<TModel1, TModel2, TModel3, dynamic>> fields = null)
        where TModel1 : new()
        where TModel2 : new()
        where TModel3 : new()
        {
            if (fields != null)
            {
                _expressionStore.Add(fields);
            }
            return entityMapper;
        }

        public static EntityMapper Select<TModel1, TModel2, TModel3, TModel4>(this EntityMapper entityMapper, Expression<Func<TModel1, TModel2, TModel3, TModel4, dynamic>> fields = null)
        where TModel1 : new()
        where TModel2 : new()
        where TModel3 : new()
        where TModel4 : new()
        {
            if (fields != null)
            {
                _expressionStore.Add(fields);
            }
            return entityMapper;
        }

        public static EntityMapper Select<TModel1, TModel2, TModel3, TModel4, TModel5>(this EntityMapper entityMapper, Expression<Func<TModel1, TModel2, TModel3, TModel4, TModel5, dynamic>> fields = null)
        where TModel1 : new()
        where TModel2 : new()
        where TModel3 : new()
        where TModel4 : new()
        where TModel5 : new()
        {
            if (fields != null)
            {
                _expressionStore.Add(fields);
            }
            return entityMapper;
        }


        public static EntityMapper Where<TModel1>(this EntityMapper entityMapper, Expression<Func<TModel1, Boolean>> expression)
        where TModel1 : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression);
            return entityMapper;
        }

        public static EntityMapper Where<TModel1, TModel2>(this EntityMapper entityMapper, Expression<Func<TModel1, TModel2, Boolean>> expression)
       where TModel1 : new()
       where TModel2 : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression);
            return entityMapper;
        }


        public static EntityMapper Where<TModel1, TModel2, TModel3>(this EntityMapper entityMapper, Expression<Func<TModel1, TModel2, TModel3, Boolean>> expression)
        where TModel1 : new()
        where TModel2 : new()
        where TModel3 : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression);
            return entityMapper;
        }


        public static EntityMapper Where<TModel1, TModel2, TModel3, TModel4>(this EntityMapper entityMapper, Expression<Func<TModel1, TModel2, TModel3, TModel4, Boolean>> expression)
        where TModel1 : new()
        where TModel2 : new()
        where TModel3 : new()
        where TModel4 : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression);
            return entityMapper;
        }


        public static EntityMapper Where<TModel1, TModel2, TModel3, TModel4, TModel5>(this EntityMapper entityMapper, Expression<Func<TModel1, TModel2, TModel3, TModel4, TModel5, Boolean>> expression)
        where TModel1 : new()
        where TModel2 : new()
        where TModel3 : new()
        where TModel4 : new()
        {
            Parameter.Validate(expression);
            _expressionStore.Add(expression);
            return entityMapper;
        }

        public static EntityMapper ThenByDesc<TModel, TKey>(this EntityMapper entityMapper, Expression<Func<TModel, TKey>> order)
        where TModel : new()
        {
            Parameter.Validate(order);
            _expressionStore.AddOrderBy(order, OrderByType.DESC);
            return entityMapper;
        }

        public static EntityMapper ThenByAsc<TModel, TKey>(this EntityMapper entityMapper, Expression<Func<TModel, TKey>> order)
        where TModel : new()
        {
            Parameter.Validate(order);
            _expressionStore.AddOrderBy(order, OrderByType.ASC);
            return entityMapper;
        }



        public static TModel FirstOrDefault<TModel>(this EntityMapper entityMapper)
        where TModel : new()
        {
            return RunDiagnosis.Watch(() =>
            {
                var executeResult = entityMapper.Query<TModel>();
                return executeResult.FirstOrDefault<TModel>();
            });
        }

        public static List<TModel> ToList<TModel>(this EntityMapper entityMapper)
        where TModel : new()
        {
            return RunDiagnosis.Watch(() =>
            {
                var executeResult = entityMapper.Query<TModel>();
                return executeResult.ToList<TModel>();
            });
        }
    }
}

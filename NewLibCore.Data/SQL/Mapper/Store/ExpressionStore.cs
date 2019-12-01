using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Store
{
    /// <summary>
    /// 将外部传入的表达式树保存起来
    /// </summary>
    internal class ExpressionStore
    {
        /// <summary>
        /// 排序语句对象
        /// </summary>
        /// <value></value>
        internal OrderExpressionMapper Order { get; private set; }

        /// <summary>
        /// 字段语句对象
        /// </summary>
        /// <value></value>
        internal SimpleExpressionMapper Select { get; private set; }

        /// <summary>
        /// Where语句对象
        /// </summary>
        /// <value></value>
        internal SimpleExpressionMapper Where { get; private set; }

        /// <summary>
        /// from语句 对象
        /// </summary>
        internal SimpleExpressionMapper From { get; private set; }

        /// <summary>
        /// 包含
        /// </summary>
        internal SimpleExpressionMapper Include { get; private set; }

        /// <summary>
        /// 分页语句对象
        /// </summary>
        /// <value></value>
        internal PaginationExpressionMapper Pagination { get; private set; }

        /// <summary>
        /// 连接语句对象列表
        /// </summary>
        internal IList<JoinExpressionMapper> Joins { get; private set; } = new List<JoinExpressionMapper>();

        internal void AddFrom<TModel>() where TModel : EntityBase, new()
        {
            var modelType = typeof(TModel);
            Expression<Func<TModel, TModel>> expression = (a) => a;

            From = new SimpleExpressionMapper
            {
                Expression = expression,
                AliaNameMapper = new List<KeyValuePair<String, String>>
                {
                   new KeyValuePair<String, String>(modelType.GetTableName().TableName,modelType.GetTableName().AliasName)
                }
            };
        }

        /// <summary>
        /// 添加排序表达式
        /// </summary>
        /// <param name="order">排序条件</param>
        /// <param name="orderByType">排序方向</param>
        /// <typeparam name="TModel">表</typeparam>
        /// <typeparam name="TKey">排序键</typeparam>
        /// <returns></returns>
        internal void AddOrderBy<TModel, TKey>(Expression<Func<TModel, TKey>> order, OrderByType orderByType)
        where TModel : EntityBase, new()
        {
            Parameter.Validate(order);
            Order = new OrderExpressionMapper
            {
                Expression = order,
                OrderBy = orderByType
            };
        }

        internal void AddInclude<TModel, TModel1>(Expression<Func<TModel, TModel1>> include)
        where TModel : EntityBase, new()
        where TModel1 : EntityBase, new()
        {
            Parameter.Validate(include);

            var parameterType = include.Parameters[0].Type;
            var foreignKeyType = include.Body.Type;

            var foreignKeyPropertyInfo = parameterType.GetProperties().FirstOrDefault(w => w.GetCustomAttributes<ForeignKeyAttribute>().Any() && w.GetCustomAttributes<ForeignKeyAttribute>().FirstOrDefault(f => f.ForeignType == foreignKeyType) != null);
            if (foreignKeyPropertyInfo == null)
            {
                throw new ArgumentException($@"{parameterType.Name}中没有用{nameof(ForeignKeyAttribute)}修饰的属性");
            }
            var foreignPropertyInfo = foreignKeyType.GetProperties().FirstOrDefault(w => w.GetCustomAttributes<PrimaryKeyAttribute>().Any());
            if (foreignPropertyInfo == null)
            {
                throw new ArgumentException($@"{foreignKeyType.Name}中没有用{nameof(PrimaryKeyAttribute)}修饰的属性");
            }

            var leftParameter = Expression.Parameter(parameterType, parameterType.GetTableName().AliasName);
            var rightParameter = Expression.Parameter(foreignKeyType, foreignKeyType.GetTableName().AliasName);

            var left = Expression.Property(leftParameter, foreignKeyPropertyInfo);
            var right = Expression.Property(rightParameter, foreignPropertyInfo);
            var r = Expression.Lambda<Func<TModel, TModel1, Boolean>>(Expression.Equal(left, right), leftParameter, rightParameter);

            AddJoin(r, JoinRelation.INNER);
        }

        /// <summary>
        /// 添加条件表达式
        /// </summary>
        /// <typeparam name="TModel1"></typeparam>
        /// <param name="filter"></param>
        internal void AddWhere<TModel1>(Expression<Func<TModel1, Boolean>> filter) where TModel1 : EntityBase, new()
        {
            Parameter.Validate(filter);
            Where = new SimpleExpressionMapper
            {
                Expression = filter,
                AliaNameMapper = ParseToAliasNames(filter)
            };
        }

        /// <summary>
        /// 添加条件表达式
        /// </summary>
        /// <typeparam name="TModel1"></typeparam>
        /// <typeparam name="TModel2"></typeparam>
        /// <param name="filter"></param>
        internal void AddWhere<TModel1, TModel2>(Expression<Func<TModel1, TModel2, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        {
            Parameter.Validate(filter);
            Where = new SimpleExpressionMapper
            {
                Expression = filter,
                AliaNameMapper = ParseToAliasNames(filter)
            };
        }

        /// <summary>
        /// 添加条件表达式
        /// </summary>
        /// <typeparam name="TModel1"></typeparam>
        /// <typeparam name="TModel2"></typeparam>
        /// <typeparam name="TModel3"></typeparam>
        /// <param name="filter"></param>
        internal void AddWhere<TModel1, TModel2, TModel3>(Expression<Func<TModel1, TModel2, TModel3, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        {
            Parameter.Validate(filter);
            Where = new SimpleExpressionMapper
            {
                Expression = filter,
                AliaNameMapper = ParseToAliasNames(filter)
            };
        }

        /// <summary>
        /// 添加条件表达式
        /// </summary>
        /// <typeparam name="TModel1"></typeparam>
        /// <typeparam name="TModel2"></typeparam>
        /// <typeparam name="TModel3"></typeparam>
        /// <typeparam name="TModel4"></typeparam>
        /// <param name="filter"></param>
        internal void AddWhere<TModel1, TModel2, TModel3, TModel4>(Expression<Func<TModel1, TModel2, TModel3, TModel4, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        {
            Parameter.Validate(filter);
            Where = new SimpleExpressionMapper
            {
                Expression = filter,
                AliaNameMapper = ParseToAliasNames(filter)
            };
        }

        /// <summary>
        /// 添加条件表达式
        /// </summary>
        /// <typeparam name="TModel1"></typeparam>
        /// <typeparam name="TModel2"></typeparam>
        /// <typeparam name="TModel3"></typeparam>
        /// <typeparam name="TModel4"></typeparam>
        /// <typeparam name="TModel5"></typeparam>
        /// <param name="filter"></param>
        internal void AddWhere<TModel1, TModel2, TModel3, TModel4, TModel5>(Expression<Func<TModel1, TModel2, TModel3, TModel4, TModel5, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        where TModel5 : EntityBase, new()
        {
            Parameter.Validate(filter);
            Where = new SimpleExpressionMapper
            {
                Expression = filter,
                AliaNameMapper = ParseToAliasNames(filter)
            };
        }

        /// <summary>
        /// 添加条件表达式
        /// </summary>
        /// <typeparam name="TModel1"></typeparam>
        /// <typeparam name="TModel2"></typeparam>
        /// <typeparam name="TModel3"></typeparam>
        /// <typeparam name="TModel4"></typeparam>
        /// <typeparam name="TModel5"></typeparam>
        /// <typeparam name="TModel6"></typeparam>
        /// <param name="filter"></param>
        internal void AddWhere<TModel1, TModel2, TModel3, TModel4, TModel5, TModel6>(Expression<Func<TModel1, TModel2, TModel3, TModel4, TModel5, TModel6, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        where TModel5 : EntityBase, new()
        where TModel6 : EntityBase, new()
        {
            Parameter.Validate(filter);
            Where = new SimpleExpressionMapper
            {
                Expression = filter,
                AliaNameMapper = ParseToAliasNames(filter)
            };
        }

        /// <summary>
        /// 添加查询列表达式
        /// </summary>
        /// <typeparam name="TModel1"></typeparam>
        /// <param name="selector"></param>
        internal void AddSelect<TModel1>(Expression<Func<TModel1, dynamic>> selector)
        where TModel1 : EntityBase, new()
        {
            Parameter.Validate(selector);
            Select = new SimpleExpressionMapper
            {
                Expression = selector
            };
        }

        /// <summary>
        /// 添加查询列表达式
        /// </summary>
        /// <typeparam name="TModel1"></typeparam>
        /// <typeparam name="TModel2"></typeparam>
        /// <param name="selector"></param>
        internal void AddSelect<TModel1, TModel2>(Expression<Func<TModel1, TModel2, dynamic>> selector)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        {
            Parameter.Validate(selector);
            Select = new SimpleExpressionMapper
            {
                Expression = selector
            };
        }

        /// <summary>
        /// 添加查询列表达式
        /// </summary>
        /// <typeparam name="TModel1"></typeparam>
        /// <typeparam name="TModel2"></typeparam>
        /// <typeparam name="TModel3"></typeparam>
        /// <param name="selector"></param>
        internal void AddSelect<TModel1, TModel2, TModel3>(Expression<Func<TModel1, TModel2, TModel3, dynamic>> selector)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        {
            Parameter.Validate(selector);
            Select = new SimpleExpressionMapper
            {
                Expression = selector
            };
        }

        /// <summary>
        /// 添加查询列表达式
        /// </summary>
        /// <typeparam name="TModel1"></typeparam>
        /// <typeparam name="TModel2"></typeparam>
        /// <typeparam name="TModel3"></typeparam>
        /// <typeparam name="TModel4"></typeparam>
        /// <param name="selector"></param>
        internal void AddSelect<TModel1, TModel2, TModel3, TModel4>(Expression<Func<TModel1, TModel2, TModel3, TModel4, dynamic>> selector)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        {
            Parameter.Validate(selector);
            Select = new SimpleExpressionMapper
            {
                Expression = selector
            };
        }

        /// <summary>
        /// 添加查询列表达式
        /// </summary>
        /// <typeparam name="TModel1"></typeparam>
        /// <typeparam name="TModel2"></typeparam>
        /// <typeparam name="TModel3"></typeparam>
        /// <typeparam name="TModel4"></typeparam>
        /// <typeparam name="TModel5"></typeparam>
        /// <param name="selector"></param>
        internal void AddSelect<TModel1, TModel2, TModel3, TModel4, TModel5>(Expression<Func<TModel1, TModel2, TModel3, TModel4, TModel5, dynamic>> selector)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        where TModel5 : EntityBase, new()
        {
            Parameter.Validate(selector);
            Select = new SimpleExpressionMapper
            {
                Expression = selector
            };
        }

        /// <summary>
        /// 添加连接表达式
        /// </summary>
        /// <typeparam name="TModel"></typeparam>
        /// <typeparam name="TJoin"></typeparam>
        /// <param name="expression"></param>
        /// <param name="joinRelation"></param>
        internal void AddJoin<TModel, TJoin>(Expression<Func<TModel, TJoin, Boolean>> expression, JoinRelation joinRelation)
        where TModel : EntityBase, new()
        where TJoin : EntityBase, new()
        {
            Parameter.Validate(expression);
            Joins.Add(new JoinExpressionMapper
            {
                Expression = expression,
                JoinRelation = joinRelation,
                AliaNameMapper = ParseToAliasNames(expression),
                MainTable = typeof(TModel).GetTableName().TableName
            });
        }

        /// <summary>
        /// 添加分页
        /// </summary>
        /// <param name="pageIndex">页索引</param>
        /// <param name="pageSize">页大小</param>
        internal void AddPage(Int32 pageIndex, Int32 pageSize)
        {
            Parameter.Validate(pageIndex);
            Parameter.Validate(pageSize);
            Pagination = new PaginationExpressionMapper
            {
                Index = pageIndex,
                Size = pageSize
            };
        }

        /// <summary>
        /// 合并查询列表达式与条件表达式的别名
        /// </summary>
        /// <returns></returns>
        internal IReadOnlyList<KeyValuePair<String, String>> MergeAliasMapper()
        {
            var newAliasMapper = new List<KeyValuePair<String, String>>();
            if (Where != null)
            {
                newAliasMapper.AddRange(Where.AliaNameMapper);
            }
            if (Joins.Any())
            {
                newAliasMapper.AddRange(Joins.SelectMany(s => s.AliaNameMapper));
            }
            newAliasMapper = newAliasMapper.Select(s => s).Distinct().ToList();

            var sameGroup = newAliasMapper.GroupBy(a => a.Value);
            foreach (var groupItem in sameGroup)
            {
                if (groupItem.Count() > 1)
                {
                    throw new ArgumentException($@"表:{String.Join(",", groupItem.Select(s => s.Key))}指定了相同别名:{groupItem.Key}");
                }
            }

            return newAliasMapper;
        }

        internal IReadOnlyList<Type> MergeTypes()
        {
            var types = new List<Type>();
            if (From != null)
            {
                var r = (From.Expression as LambdaExpression).Parameters[0].Type;
                types.Add(r);
            }

            if (Joins.Any())
            {
                foreach (var item in Joins)
                {
                    foreach (var parameter in (item.Expression as LambdaExpression).Parameters)
                    {
                        types.Add(parameter.Type);
                    }
                }
            }
            return types.Distinct().ToList();
        }

        /// <summary>
        /// 将表达式对象转换为表别名列表
        /// </summary>
        /// <param name="expression">表达式</param>
        /// <returns></returns>
        private IReadOnlyList<KeyValuePair<String, String>> ParseToAliasNames(Expression expression)
        {
            return ((LambdaExpression)expression).Parameters.Select(s => new KeyValuePair<String, String>(s.Type.GetTableName().TableName, s.Type.GetTableName().AliasName)).ToList();
        }
    }
}

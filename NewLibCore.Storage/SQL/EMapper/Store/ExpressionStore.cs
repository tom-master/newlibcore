using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Storage.SQL.Validate;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Store
{

    internal static class ExpressionStoreExtension
    {
        /// <summary>
        /// 添加一个sql语句
        /// </summary>
        /// <param name="sql"></param>
        /// <param name="parameters"></param>
        internal static void AddDirectSql(this ExpressionStore store, String sql, params MapperParameter[] parameters)
        {
            Check.IfNullOrZero(sql);
            store.RawSql = new RawSqlComponent
            {
                Sql = sql,
                Parameters = parameters
            };
        }

        internal static void AddModel<TModel>(this ExpressionStore store, TModel model) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(model);
            store.Model = model;
        }
    }

    /// <summary>
    /// 将外部传入的表达式树保存起来
    /// </summary>
    internal class ExpressionStore
    {
        /// <summary>
        /// 排序语句对象
        /// </summary>
        /// <value></value>
        internal OrderComponent Order { get; private set; }

        /// <summary>
        /// 字段语句对象
        /// </summary>
        /// <value></value>
        internal ComponentBase Select { get; private set; }

        /// <summary>
        /// Where语句对象
        /// </summary>
        /// <value></value>
        internal SimpleComponent Where { get; private set; }

        /// <summary>
        /// from语句 对象
        /// </summary>
        internal SimpleComponent From { get; private set; }

        /// <summary>
        /// 分页语句对象
        /// </summary>
        /// <value></value>
        internal PaginationComponent Pagination { get; private set; }

        /// <summary>
        /// sql语句对象
        /// </summary>
        /// <value></value>
        internal RawSqlComponent RawSql { get; set; }

        /// <summary>
        /// 存储单个模型实例
        /// </summary>
        /// <value></value>
        internal EntityBase Model { get; set; }

        /// <summary>
        /// 连接语句对象列表
        /// </summary>
        internal IList<JoinComponent> JoinComponents { get; private set; } = new List<JoinComponent>();

        internal void AddFrom<TModel>() where TModel : EntityBase, new()
        {
            var modelType = typeof(TModel);
            Expression<Func<TModel, TModel>> expression = (a) => a;

            From = new SimpleComponent
            {
                Expression = expression,
                AliasNameMapper = new List<KeyValuePair<String, String>>
                {
                    new KeyValuePair<String, String>(modelType.GetEntityBaseAliasName().TableName, modelType.GetEntityBaseAliasName().AliasName)
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
            Check.IfNullOrZero(order);
            Order = new OrderComponent
            {
                Expression = order,
                OrderBy = orderByType
            };
        }

        internal void AddInclude<TModel, TModel1>(Expression<Func<TModel, TModel1>> include)
        where TModel : EntityBase, new()
        where TModel1 : EntityBase, new()
        {
            Check.IfNullOrZero(include);

            var parameterType = include.Parameters[0].Type;
            var foreignKeyType = include.Body.Type;

            //找到模型中用ForeignKeyAttribute修饰的外键
            var foreignKeyPropertyInfo = parameterType.GetProperties(BindingFlags.Instance | BindingFlags.Public)
                .FirstOrDefault(w => w.GetAttributes<ForeignKeyAttribute>().Any(f => f.ForeignType == foreignKeyType));
            if (foreignKeyPropertyInfo == null)
            {
                throw new ArgumentException($@"{parameterType.Name}中没有用{nameof(ForeignKeyAttribute)}修饰的属性");
            }

            //找到对应外键的表用PrimaryKeyAttribute修饰的主键
            var foreignPropertyInfo = foreignKeyType.GetProperties(BindingFlags.Instance | BindingFlags.Public)
            .FirstOrDefault(w => w.GetAttributes<PrimaryKeyAttribute>().Any());
            if (foreignPropertyInfo == null)
            {
                throw new ArgumentException($@"{foreignKeyType.Name}中没有用{nameof(PrimaryKeyAttribute)}修饰的属性");
            }

            var leftParameter = Expression.Parameter(parameterType, parameterType.GetEntityBaseAliasName().AliasName);
            var rightParameter = Expression.Parameter(foreignKeyType, foreignKeyType.GetEntityBaseAliasName().AliasName);

            var left = Expression.Property(leftParameter, foreignKeyPropertyInfo);
            var right = Expression.Property(rightParameter, foreignPropertyInfo);

            var includeExpression = Expression.Lambda<Func<TModel, TModel1, Boolean>>(Expression.Equal(left, right), leftParameter, rightParameter);
            AddJoin(includeExpression, JoinRelation.LEFT);
        }

        /// <summary>
        /// 添加条件表达式
        /// </summary>
        /// <typeparam name="TModel1"></typeparam>
        /// <param name="filter"></param>
        internal void AddWhere<TModel1>(Expression<Func<TModel1, Boolean>> filter) where TModel1 : EntityBase, new()
        {
            Check.IfNullOrZero(filter);
            Where = new SimpleComponent
            {
                Expression = filter,
                AliasNameMapper = ParseToAliasNames(((LambdaExpression)filter).Parameters)
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
            Check.IfNullOrZero(filter);
            Where = new SimpleComponent
            {
                Expression = filter,
                AliasNameMapper = ParseToAliasNames(((LambdaExpression)filter).Parameters)
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
            Check.IfNullOrZero(filter);
            Where = new SimpleComponent
            {
                Expression = filter,
                AliasNameMapper = ParseToAliasNames(((LambdaExpression)filter).Parameters)
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
            Check.IfNullOrZero(filter);
            Where = new SimpleComponent
            {
                Expression = filter,
                AliasNameMapper = ParseToAliasNames(((LambdaExpression)filter).Parameters)
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
            Check.IfNullOrZero(filter);
            Where = new SimpleComponent
            {
                Expression = filter,
                AliasNameMapper = ParseToAliasNames(((LambdaExpression)filter).Parameters)
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
            Check.IfNullOrZero(filter);
            Where = new SimpleComponent
            {
                Expression = filter,
                AliasNameMapper = ParseToAliasNames(((LambdaExpression)filter).Parameters)
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
            Check.IfNullOrZero(selector);
            Select = new ComponentBase
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
            Check.IfNullOrZero(selector);
            Select = new ComponentBase
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
            Check.IfNullOrZero(selector);
            Select = new ComponentBase
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
            Check.IfNullOrZero(selector);
            Select = new ComponentBase
            {
                Expression = selector,
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
            Check.IfNullOrZero(selector);
            Select = new ComponentBase
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
            Check.IfNullOrZero(expression);
            JoinComponents.Add(new JoinComponent
            {
                Expression = expression,
                JoinRelation = joinRelation,
                AliasNameMapper = ParseToAliasNames(((LambdaExpression)expression).Parameters),
                MainTable = typeof(TModel).GetEntityBaseAliasName().TableName
            });
        }

        /// <summary>
        /// 添加分页
        /// </summary>
        /// <param name="pageIndex">页索引</param>
        /// <param name="pageSize">页大小</param>
        internal void AddPage(Int32 pageIndex, Int32 pageSize, Int32 maxKey = 0)
        {
            Check.IfNullOrZero(pageIndex);
            Check.IfNullOrZero(pageSize);

            Pagination = new PaginationComponent
            {
                Index = pageIndex,
                Size = pageSize,
                MaxKey = maxKey,
                AliasNameMapper = From.AliasNameMapper
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
                newAliasMapper.AddRange(Where.AliasNameMapper);
            }
            if (JoinComponents.Any())
            {
                newAliasMapper.AddRange(JoinComponents.SelectMany(s => s.AliasNameMapper));
            }
            if (From != null)
            {
                newAliasMapper.AddRange(From.AliasNameMapper);
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

        /// <summary>
        /// 合并返回表达式的参数类型
        /// </summary>
        /// <returns></returns>
        internal IReadOnlyList<Type> MergeParameterTypes()
        {
            var types = new List<Type>();
            if (From != null)
            {
                var r = (From.Expression as LambdaExpression).Parameters[0].Type;
                types.Add(r);
            }

            if (JoinComponents.Any())
            {
                foreach (var item in JoinComponents)
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
        private IReadOnlyList<KeyValuePair<String, String>> ParseToAliasNames(IReadOnlyList<ParameterExpression> parameters)
        {
            var list = new List<KeyValuePair<String, String>>();
            foreach (var item in parameters)
            {
                var (TableName, AliasName) = item.Type.GetEntityBaseAliasName();
                list.Add(new KeyValuePair<String, String>(TableName, AliasName));
            }
            return list.ToList();
        }
    }
}

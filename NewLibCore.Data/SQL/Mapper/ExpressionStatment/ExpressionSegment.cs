using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.ExpressionStatment
{
    /// <summary>
    /// 分解的表达式段
    /// </summary>
    internal class SegmentManager
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
        internal SimpleExpressionMapper SelectField { get; private set; }

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
        /// 分页语句对象
        /// </summary>
        /// <value></value>
        internal PaginationExpressionMapper Pagination { get; private set; }

        /// <summary>
        /// 连接语句对象列表
        /// </summary>
        internal IList<JoinExpressionMapper> Joins { get; private set; } = new List<JoinExpressionMapper>();

        /// <summary>
        /// 将表达式拆分出相应的排序对象
        /// </summary>
        /// <param name="order">排序条件</param>
        /// <param name="orderByType">排序方向</param>
        /// <typeparam name="TModel">表</typeparam>
        /// <typeparam name="TKey">排序键</typeparam>
        /// <returns></returns>
        internal void AddOrderBy<TModel, TKey>(Expression<Func<TModel, TKey>> order, OrderByType orderByType)
        {
            Parameter.Validate(order);
            Order = new OrderExpressionMapper
            {
                Expression = order,
                OrderBy = orderByType
            };
        }

        /// <summary>
        /// 将表达式拆分出相应的连接对象
        /// </summary>
        /// <param name="expression">连接表达式</param>
        /// <param name="joinType">连接类型</param>
        /// <typeparam name="TModel">主表</typeparam>
        /// <typeparam name="TJoin">子表</typeparam>
        /// <returns></returns>
        internal void Add<TModel, TJoin>(Expression<Func<TModel, TJoin, Boolean>> expression, JoinType joinType) where TModel : new()
            where TJoin : new()
        {
            Parameter.Validate(expression);
            Joins.Add(new JoinExpressionMapper
            {
                Expression = expression,
                JoinType = joinType,
                AliaNameMapper = ParseToAliasNames(expression),
                MainTable = typeof(TModel).GetTableName().TableName
            });
        }

        /// <summary>
        /// 将表达式拆分出相应的Where对象
        /// </summary>
        /// <param name="expression">连接表达式</param>
        /// <typeparam name="TModel">主表</typeparam>
        /// <typeparam name="TJoin">连接类型</typeparam>
        /// <returns></returns>
        internal void Add<TModel, TJoin>(Expression<Func<TModel, TJoin, Boolean>> expression) where TModel : new()
        {
            Parameter.Validate(expression);
            Where = new SimpleExpressionMapper
            {
                Expression = expression,
                AliaNameMapper = ParseToAliasNames(expression)
            };
        }

        /// <summary>
        /// 将表达式拆分出相应的Where对象
        /// </summary>
        /// <param name="expression">表达式</param>
        /// <typeparam name="TModel">主表</typeparam>
        internal void Add<TModel>(Expression<Func<TModel, Boolean>> expression) where TModel : new()
        {
            Parameter.Validate(expression);
            Where = new SimpleExpressionMapper
            {
                Expression = expression,
                AliaNameMapper = ParseToAliasNames(expression)
            };
        }

        /// <summary>
        /// 将表达式拆分出相应的字段对象
        /// </summary>
        /// <param name="expression">表达式</param>
        /// <typeparam name="TModel">主表</typeparam>
        internal void Add<TModel>(Expression<Func<TModel, dynamic>> expression) where TModel : new()
        {
            Parameter.Validate(expression);
            SelectField = new SimpleExpressionMapper
            {
                Expression = expression
            };
        }

        internal void Add<TModel>() where TModel : new()
        {
            Expression<Func<Type>> expression = () => typeof(TModel);
            From = new SimpleExpressionMapper
            {
                Expression = expression,
                AliaNameMapper = new List<KeyValuePair<String, String>>
                {
                   new KeyValuePair<String, String>(typeof(TModel).GetTableName().TableName,typeof(TModel).GetTableName().AliasName)
                }
            };
        }

        /// <summary>
        /// 将表达式拆分出相应的字段对象
        /// </summary>
        /// <param name="expression">表达式</param>
        /// <typeparam name="TModel">主表</typeparam>
        /// <typeparam name="T">子表</typeparam>
        /// <returns></returns>
        internal void Add<TModel, T>(Expression<Func<TModel, T, dynamic>> expression) where TModel : new()
        where T : EntityBase, new()
        {
            Parameter.Validate(expression);
            SelectField = new SimpleExpressionMapper
            {
                Expression = expression
            };
        }

        /// <summary>
        /// 将表达式拆分出相应的分页对象
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
        /// 合并相应的实体别名
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
            return newAliasMapper;
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

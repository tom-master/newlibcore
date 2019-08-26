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
        internal OrderStatement Order { get; private set; }

        /// <summary>
        /// 字段语句对象
        /// </summary>
        /// <value></value>
        internal SimpleStatement Field { get; private set; }

        /// <summary>
        /// Where语句对象
        /// </summary>
        /// <value></value>
        internal SimpleStatement Where { get; private set; }

        /// <summary>
        /// 分页语句对象
        /// </summary>
        /// <value></value>
        internal PageSegment Page { get; private set; }

        /// <summary>
        /// 连接语句对象列表
        /// </summary>
        /// <typeparam name="JoinStatement"></typeparam>
        /// <returns></returns>
        internal IList<JoinSegment> Joins { get; private set; } = new List<JoinSegment>();

        /// <summary>
        /// 将表达式拆分出相应的排序对象
        /// </summary>
        /// <param name="order"></param>
        /// <param name="orderByType"></param>
        /// <typeparam name="TModel"></typeparam>
        /// <typeparam name="TKey"></typeparam>
        /// <returns></returns>
        internal void AddOrderBy<TModel, TKey>(Expression<Func<TModel, TKey>> order, OrderByType orderByType)
        {
            Parameter.Validate(order);
            Order = new OrderStatement
            {
                Expression = order,
                OrderBy = orderByType
            };
        }

        /// <summary>
        /// 将表达式拆分出相应的连接对象
        /// </summary>
        /// <param name="expression"></param>
        /// <param name="joinType"></param>
        /// <typeparam name="TModel"></typeparam>
        /// <typeparam name="TJoin"></typeparam>
        /// <returns></returns>
        internal void Add<TModel, TJoin>(Expression<Func<TModel, TJoin, Boolean>> expression, JoinType joinType) where TModel : EntityBase, new()
            where TJoin : EntityBase, new()
        {
            Parameter.Validate(expression);
            Joins.Add(new JoinSegment
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
        /// <param name="expression"></param>
        /// <typeparam name="TModel"></typeparam>
        /// <typeparam name="TJoin"></typeparam>
        /// <returns></returns>
        internal void Add<TModel, TJoin>(Expression<Func<TModel, TJoin, Boolean>> expression) where TModel : EntityBase, new()
        {
            Parameter.Validate(expression);
            Where = new SimpleStatement
            {
                Expression = expression,
                AliaNameMapper = ParseToAliasNames(expression)
            };
        }

        /// <summary>
        /// 将表达式拆分出相应的Where对象
        /// </summary>
        /// <param name="expression"></param>
        /// <typeparam name="TModel"></typeparam>
        internal void Add<TModel>(Expression<Func<TModel, Boolean>> expression) where TModel : EntityBase, new()
        {
            Parameter.Validate(expression);
            Where = new SimpleStatement
            {
                Expression = expression,
                AliaNameMapper = ParseToAliasNames(expression)
            };
        }

        /// <summary>
        /// 将表达式拆分出相应的字段对象
        /// </summary>
        /// <param name="expression"></param>
        /// <typeparam name="TModel"></typeparam>
        internal void Add<TModel>(Expression<Func<TModel, dynamic>> expression) where TModel : EntityBase, new()
        {
            Parameter.Validate(expression);
            Field = new SimpleStatement
            {
                Expression = expression
            };
        }

        /// <summary>
        /// 将表达式拆分出相应的字段对象
        /// </summary>
        /// <param name="expression"></param>
        /// <typeparam name="TModel"></typeparam>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        internal void Add<TModel, T>(Expression<Func<TModel, T, dynamic>> expression) where TModel : EntityBase, new()
        where T : EntityBase, new()
        {
            Parameter.Validate(expression);
            Field = new SimpleStatement
            {
                Expression = expression
            };
        }

        /// <summary>
        /// 将表达式拆分出相应的分页对象
        /// </summary>
        /// <param name="pageIndex"></param>
        /// <param name="pageSize"></param>
        internal void AddPage(Int32 pageIndex, Int32 pageSize)
        {
            Parameter.Validate(pageIndex);
            Parameter.Validate(pageSize);
            Page = new PageSegment
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
        /// <param name="expression"></param>
        /// <returns></returns>
        private IReadOnlyList<KeyValuePair<String, String>> ParseToAliasNames(Expression expression)
        {
            return ((LambdaExpression)expression).Parameters.Select(s => new KeyValuePair<String, String>(s.Type.GetTableName().TableName, s.Type.GetTableName().AliasName)).ToList();
        }
    }
}

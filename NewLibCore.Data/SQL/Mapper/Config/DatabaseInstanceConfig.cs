using System;
using System.Collections.Generic;
using System.Data.Common;
using NewLibCore.Data.SQL.Mapper.Cache;

namespace NewLibCore.Data.SQL.Mapper.Config
{
    /// <summary>
    /// 数据库实例配置
    /// </summary>
    internal abstract class DatabaseInstanceConfig
    {
        private readonly ILogger _logger;

        protected static IDictionary<RelationType, String> RelationMapper = new Dictionary<RelationType, String>();

        protected static IDictionary<JoinType, String> JoinTypeMapper = new Dictionary<JoinType, String>();

        protected static IDictionary<OrderByType, String> OrderTypeMapper = new Dictionary<OrderByType, String>();

        protected String ConnectionString { get { return Host.GetHostVar("NewCrmDatabase"); } }

        public virtual String UnionPlaceHolder { get { return Guid.NewGuid().ToString().Replace("-", ""); } }

        internal ResultCache Cache { get; set; }

        protected DatabaseInstanceConfig(ILogger logger)
        {
            _logger = logger ?? new ConsoleLogger();

            RelationMapper.Clear();
            JoinTypeMapper.Clear();
            OrderTypeMapper.Clear();

            InitRelationType();
            InitJoinType();
            InitOrderType();
        }

        /// <summary>
        /// 追加关系类型
        /// </summary>

        protected abstract void AppendRelationType();

        /// <summary>
        /// 实例扩展
        /// </summary>
        /// <value></value>
        internal virtual InstanceExtension Extension { get; }

        /// <summary>
        /// 日志
        /// </summary>
        /// <value></value>
        internal ILogger Logger
        {
            get { return _logger; }
        }

        /// <summary>
        /// 获取数据库连接对象实例
        /// </summary>
        /// <returns></returns>
        internal abstract DbConnection GetConnectionInstance();

        /// <summary>
        /// 获取SQL语句参数对象实例
        /// </summary>
        /// <returns></returns>
        internal abstract DbParameter GetParameterInstance();

        /// <summary>
        /// 逻辑关系构建
        /// </summary>
        /// <param name="relationType"></param>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <returns></returns>
        internal abstract String RelationBuilder(RelationType relationType, String left, Object right);

        /// <summary>
        /// 连接语句构建
        /// </summary>
        /// <param name="joinType"></param>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <returns></returns>
        internal String JoinBuilder(JoinType joinType, String left, String right)
        {
            return String.Format(JoinTypeMapper[joinType], left, right);
        }

        /// <summary>
        /// 排序语句构建
        /// </summary>
        /// <param name="orderByType"></param>
        /// <param name="left"></param>
        /// <returns></returns>
        internal String OrderByBuilder(OrderByType orderByType, String left)
        {
            return String.Format(OrderTypeMapper[orderByType], left);
        }

        /// <summary>
        /// 初始化默认逻辑关系
        /// </summary>
        private void InitRelationType()
        {
            RelationMapper.Add(RelationType.AND, "{0} AND {1}");
            RelationMapper.Add(RelationType.OR, "{0} OR {1}");
            RelationMapper.Add(RelationType.EQ, "{0} = {1}");
            RelationMapper.Add(RelationType.NQ, "{0} <> {1}");
            RelationMapper.Add(RelationType.GT, "{0} < {1}");
            RelationMapper.Add(RelationType.LT, "{0} > {1}");
            RelationMapper.Add(RelationType.GE, "{0} <= {1}");
            RelationMapper.Add(RelationType.LE, "{0} >= {1}");

            AppendRelationType();
        }

        /// <summary>
        /// 初始化默认连接语句
        /// </summary>
        private void InitJoinType()
        {
            JoinTypeMapper.Add(JoinType.NONE, "");
            JoinTypeMapper.Add(JoinType.INNER, "INNER JOIN {0} AS {1} ON");
            JoinTypeMapper.Add(JoinType.LEFT, "LEFT JOIN {0} AS {1} ON");
            JoinTypeMapper.Add(JoinType.RIGHT, "RIGHT JOIN {0} AS {1} ON");
        }

        /// <summary>
        /// 初始化排序类型
        /// </summary>
        private void InitOrderType()
        {
            OrderTypeMapper.Add(OrderByType.ASC, "ORDER BY {0} ASC");
            OrderTypeMapper.Add(OrderByType.DESC, "ORDER BY {0} DESC");
        }
    }

    /// <summary>
    /// 数据库实例扩展
    /// </summary>
    internal class InstanceExtension
    {
        /// <summary>
        /// 自增主键
        /// </summary>
        /// <value></value>
        public String Identity { get; internal set; }

        /// <summary>
        /// 执行受影响的行数
        /// </summary>
        /// <value></value>
        public String RowCount { get; internal set; }

        /// <summary>
        /// 分页 每页条数与多少页
        /// </summary>
        /// <value></value>
        public String Page { get; internal set; }
    }
}

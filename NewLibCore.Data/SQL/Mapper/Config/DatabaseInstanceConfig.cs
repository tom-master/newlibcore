using System;
using System.Collections.Generic;
using System.Data.Common;
using NewLibCore.Data.SQL.Mapper.Cache;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.Mapper.Config
{
    internal abstract class DatabaseInstanceConfig
    {
        private readonly ILogger _logger;

        protected static IDictionary<RelationType, String> RelationMapper = new Dictionary<RelationType, String>();

        protected static IDictionary<JoinType, String> JoinTypeMapper = new Dictionary<JoinType, String>();

        protected static IDictionary<OrderByType, String> OrderTypeMapper = new Dictionary<OrderByType, String>();

        protected String ConnectionString { get { return Host.GetHostVar("NewCrmDatabase"); } }

        public virtual String UnionPlaceHolder { get { return Guid.NewGuid().ToString().Replace("-", ""); } }

        internal MapperCache Cache { get; set; }

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

        protected internal abstract void AppendRelationType();

        internal virtual InstanceExtension Extension { get; }

        internal abstract DbConnection GetConnectionInstance();

        internal abstract DbParameter GetParameterInstance();

        internal abstract String RelationBuilder(RelationType relationType, String left, Object right);

        internal virtual ILogger Logger
        {
            get { return _logger; }
        }

        internal String JoinBuilder(JoinType joinType, String left, String right)
        {
            return String.Format(JoinTypeMapper[joinType], left, right);
        }

        internal String OrderByBuilder(OrderByType orderByType, String left)
        {
            return String.Format(OrderTypeMapper[orderByType], left);
        }

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

        private void InitJoinType()
        {
            JoinTypeMapper.Add(JoinType.NONE, "");
            JoinTypeMapper.Add(JoinType.INNER, "INNER JOIN {0} AS {1} ON");
            JoinTypeMapper.Add(JoinType.LEFT, "LEFT JOIN {0} AS {1} ON");
            JoinTypeMapper.Add(JoinType.RIGHT, "RIGHT JOIN {0} AS {1} ON");
        }

        private void InitOrderType()
        {
            OrderTypeMapper.Add(OrderByType.ASC, "ORDER BY {0} ASC");
            OrderTypeMapper.Add(OrderByType.DESC, "ORDER BY {0} DESC");
        }
    }


    internal class InstanceExtension
    {
        public String Identity { get; internal set; }

        public String RowCount { get; internal set; }

        public String Page { get; internal set; }
    }
}

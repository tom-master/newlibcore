using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Data.SqlClient;
using MySql.Data.MySqlClient;
using NewLibCore.Data.SQL.Mapper.Extension;

namespace NewLibCore.Data.SQL.Mapper.Config
{
	internal abstract class MapperInstance
	{
		protected static IDictionary<RelationType, String> RelationMapper = new Dictionary<RelationType, String>();

		protected static IDictionary<JoinType, String> JoinTypeMapper = new Dictionary<JoinType, String>();

		protected static IDictionary<OrderByType, String> OrderTypeMapper = new Dictionary<OrderByType, String>();

		protected String ConnectionString { get { return Host.GetHostVar("NewCrmDatabase"); } }

		protected MapperInstance()
		{
			RelationMapper.Clear();
			JoinTypeMapper.Clear();
			OrderTypeMapper.Clear();

			InitRelationType();
			InitJoinType();
			InitOrderType();
		}

		protected virtual void AppendRelationType() { }

		internal virtual InstanceExtension Extension { get; }

		internal abstract DbConnection GetConnectionInstance();

		internal abstract DbParameter GetParameterInstance();

		internal abstract String RelationBuilder(RelationType relationType, String left, Object right);

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

	internal class MsSqlInstance : MapperInstance
	{
		protected override void AppendRelationType()
		{
			RelationMapper.Add(RelationType.FULL_LIKE, "{0} LIKE '%{1}%'");
			RelationMapper.Add(RelationType.START_LIKE, "{0} LIKE '{1}%'");
			RelationMapper.Add(RelationType.END_LIKE, "{0} LIKE '%{1}' ");
			RelationMapper.Add(RelationType.IN, "{0} IN ({1})");
		}

		internal override DbConnection GetConnectionInstance()
		{
			return new SqlConnection(ConnectionString);
		}

		internal override DbParameter GetParameterInstance()
		{
			return new SqlParameter();
		}

		internal override String RelationBuilder(RelationType relationType, String left, Object right)
		{
			return String.Format(RelationMapper[relationType], left, right);
		}

		internal override InstanceExtension Extension
		{
			get
			{
				return new InstanceExtension
				{
					Identity = " SELECT @@IDENTITY",
					RowCount = " SELECT @@ROWCOUNT",
					Page = " OFFSET ({value}) ROWS FETCH NEXT {pageSize} ROWS ONLY"
				};
			}
		}
	}

	internal class MySqlInstance : MapperInstance
	{
		protected override void AppendRelationType()
		{
			RelationMapper.Add(RelationType.FULL_LIKE, "{0} LIKE CONCAT('%',{1},'%')");
			RelationMapper.Add(RelationType.START_LIKE, "{0} LIKE CONCAT('',{1},'%')");
			RelationMapper.Add(RelationType.END_LIKE, "{0} LIKE CONCAT('%',{1},'')");
			RelationMapper.Add(RelationType.IN, "FIND_IN_SET({0},{1})");
		}

		internal override DbConnection GetConnectionInstance()
		{
			return new MySqlConnection(ConnectionString);
		}

		internal override DbParameter GetParameterInstance()
		{
			return new MySqlParameter();
		}

		internal override String RelationBuilder(RelationType relationType, String left, Object right)
		{
			return String.Format(RelationMapper[relationType], left, right);
		}

		internal override InstanceExtension Extension
		{
			get
			{
				return new InstanceExtension
				{
					Identity = " ; SELECT CAST(@@IDENTITY AS SIGNED) AS c ",
					RowCount = " ; SELECT CAST(ROW_COUNT() AS SIGNED) AS c",
					Page = " LIMIT {value},{pageSize}"
				};
			} 
		}
	}

	internal class InstanceExtension
	{
		public String Identity { get; internal set; }

		public String RowCount { get; internal set; }

 		public String Page { get; internal set; }
	}
}

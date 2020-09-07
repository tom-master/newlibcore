using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;
using System.Reflection;
using NewLibCore.Data.SQL.Extension;
using NewLibCore.Data.SQL.Store;
using NewLibCore.Data.SQL.Validate;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Template
{
    /// <summary>
    /// 为相应的数据库实例提供对应的模板化SQL
    /// </summary>
    internal abstract class TemplateBase
    {

        private static String _primaryKeyName;

        /// <summary>
        /// 谓词关系映射
        /// </summary>
        protected readonly IDictionary<PredicateType, String> PredicateMapper = new Dictionary<PredicateType, String>();

        /// <summary>
        /// 连接关系映射
        /// </summary>
        protected readonly IDictionary<JoinRelation, String> JoinMapper = new Dictionary<JoinRelation, String>();

        /// <summary>
        /// 排序方式映射
        /// </summary>
        protected readonly IDictionary<OrderByType, String> OrderTypeMapper = new Dictionary<OrderByType, String>();


        /// <summary>
        /// 初始化TemplateBase类的新实例
        /// </summary>
        protected TemplateBase()
        {
            JoinMapper.Clear();
            PredicateMapper.Clear();
            OrderTypeMapper.Clear();

            InitPredicateType();
            InitJoinType();
            InitOrderType();
        }

        /// <summary>
        /// 查询模板
        /// </summary>
        internal virtual String CreateSelect(String field, String tableName, String aliasName)
        {
            return String.Format("SELECT {0} FROM {1} AS {2} ", field, tableName, aliasName);
        }

        /// <summary>
        /// 添加模板
        /// </summary>
        internal virtual String CreateInsert<TModel>(TModel model) where TModel : EntityBase
        {
            return $@"INSERT {model.GetTableName().TableName} ({model.SqlPart.Fields}) VALUES ({model.SqlPart.InsertPlaceHolders}) {Identity} ";
        }

        /// <summary>
        /// 更新模板
        /// </summary>
        internal abstract String CreateUpdate<TModel>(TModel model) where TModel : EntityBase;

        /// <summary>
        /// 追加关系类型
        /// </summary>
        protected abstract void AppendPredicateType();

        /// <summary>
        /// 返回新增主键
        /// </summary>
        /// <value></value>
        internal abstract String Identity { get; }

        /// <summary>
        /// 执行受影响的行数
        /// </summary>
        /// <value></value>
        internal abstract String AffectedRows { get; }

        /// <summary>
        /// 获取主键名称(默认为Id)
        /// </summary>
        internal String PrimaryKeyName
        {
            get
            {

                if (!String.IsNullOrEmpty(_primaryKeyName))
                {
                    return _primaryKeyName;
                }

                var identityProperty = typeof(EntityBase)
                 .GetProperties(BindingFlags.Instance | BindingFlags.Public)
                 .ToList().FirstOrDefault(w => w.GetAttributes<PrimaryKeyAttribute>().Any());
                if (identityProperty == null)
                {
                    throw new ArgumentNullException($@"未找到使用{nameof(PrimaryKeyAttribute)}修饰的主键");
                }
                var name = identityProperty.Name;
                _primaryKeyName = name;
                return name;
            }
        }

        /// <summary>
        /// 追加分页语句
        /// </summary>
        /// <param name="pageIndex"></param>
        /// <param name="pageSize"></param>
        /// <param name="orderBy"></param>
        /// <param name="rawSql"></param>
        /// <returns></returns>
        internal abstract String CreatePagination(PaginationExpressionMapper pagination, String orderBy, String rawSql);

        /// <summary>
        /// 创建谓词关系
        /// </summary>
        /// <param name="predicateType">关系的类型</param>
        /// <param name="left">左语句</param>
        /// <param name="right">右语句</param>
        /// <returns></returns>
        internal abstract String CreatePredicate(PredicateType predicateType, String left, String right);

        /// <summary>
        /// 创建参数
        /// </summary>
        /// <returns></returns>
        internal abstract DbParameter CreateParameter(String key, Object value, Type dataType);

        /// <summary>
        /// 创建连接
        /// </summary>
        /// <returns></returns>
        internal abstract DbConnection CreateDbConnection();

        /// <summary>
        /// 创建连接关系
        /// </summary>
        /// <param name="joinRelation">连接类型</param>
        /// <param name="left">左语句</param>
        /// <param name="right">右语句</param>
        /// <returns></returns>
        internal String CreateJoin(JoinRelation joinRelation, String left, String right)
        {
            Parameter.IfNullOrZero(joinRelation);
            Parameter.IfNullOrZero(left);
            Parameter.IfNullOrZero(right);

            if (!JoinMapper.ContainsKey(joinRelation))
            {
                throw new ArgumentNullException($@"{joinRelation}不存在");
            }

            return String.Format(JoinMapper[joinRelation], left, right);
        }

        /// <summary>
        /// 排序语句构建
        /// </summary>
        /// <param name="orderByType">排序方向</param>
        /// <param name="left">左语句</param>
        /// <returns></returns>
        internal String CreateOrderBy(OrderByType orderByType, String left)
        {
            Parameter.IfNullOrZero(orderByType);
            Parameter.IfNullOrZero(left);

            if (!OrderTypeMapper.ContainsKey(orderByType))
            {
                throw new ArgumentNullException($@"{orderByType}不存在");
            }
            return String.Format(OrderTypeMapper[orderByType], left);
        }

        protected DbType ConvertToDatabaseDataType(Type dataType)
        {

            switch (Type.GetTypeCode(dataType))
            {
                case TypeCode.Boolean:
                    return DbType.Boolean;
                case TypeCode.Byte:
                    return DbType.Byte;
                case TypeCode.DateTime:
                    return DbType.DateTime;
                case TypeCode.Decimal:
                    return DbType.Decimal;
                case TypeCode.Double:
                    return DbType.Double;
                case TypeCode.Int16:
                    return DbType.Int16;
                case TypeCode.Int32:
                    return DbType.Int32;
                case TypeCode.Int64:
                    return DbType.Int64;
                case TypeCode.SByte:
                    return DbType.SByte;
                case TypeCode.Single:
                    return DbType.Single;
                case TypeCode.String:
                    return DbType.String;
                case TypeCode.UInt16:
                    return DbType.UInt16;
                case TypeCode.UInt32:
                    return DbType.UInt32;
                case TypeCode.UInt64:
                    return DbType.UInt64;
                case TypeCode.Object:
                    return DbType.Object;
                default:
                    throw new InvalidCastException();
            }
        }

        /// <summary>
        /// 初始化默认逻辑关系
        /// </summary>
        private void InitPredicateType()
        {
            PredicateMapper.Add(PredicateType.AND, " {0} AND {1} ");
            PredicateMapper.Add(PredicateType.OR, " {0} OR {1} ");
            PredicateMapper.Add(PredicateType.EQ, " {0} = {1} ");
            PredicateMapper.Add(PredicateType.NQ, " {0} <> {1} ");
            PredicateMapper.Add(PredicateType.GT, " {0} < {1} ");
            PredicateMapper.Add(PredicateType.LT, " {0} > {1} ");
            PredicateMapper.Add(PredicateType.GE, " {0} <= {1} ");
            PredicateMapper.Add(PredicateType.LE, " {0} >= {1} ");

            AppendPredicateType();
        }

        /// <summary>
        /// 初始化默认连接语句
        /// </summary>
        private void InitJoinType()
        {
            JoinMapper.Add(JoinRelation.NONE, "");
            JoinMapper.Add(JoinRelation.INNER, " INNER JOIN {0} AS {1} ON ");
            JoinMapper.Add(JoinRelation.LEFT, " LEFT JOIN {0} AS {1} ON ");
            JoinMapper.Add(JoinRelation.RIGHT, " RIGHT JOIN {0} AS {1} ON ");
        }

        /// <summary>
        /// 初始化排序类型
        /// </summary>
        private void InitOrderType()
        {
            OrderTypeMapper.Add(OrderByType.ASC, " ORDER BY {0} ASC ");
            OrderTypeMapper.Add(OrderByType.DESC, " ORDER BY {0} DESC ");
        }

    }
}

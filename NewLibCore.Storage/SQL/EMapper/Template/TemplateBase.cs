using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Text;
using NewLibCore.Storage.SQL.Component;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Template
{
    /// <summary>
    /// 为相应的数据库实例提供对应的模板化SQL
    /// </summary>
    public abstract class TemplateBase
    { 
        protected virtual void SetVersion(int version) { }

        /// <summary>
        /// 谓词关系映射
        /// </summary>
        internal readonly IDictionary<EMType, string> PredicateMapper = new Dictionary<EMType, string>();

        /// <summary>
        /// 连接关系映射
        /// </summary>
        internal readonly IDictionary<EMType, string> JoinMapper = new Dictionary<EMType, string>();

        /// <summary>
        /// 排序方式映射
        /// </summary>
        internal readonly IDictionary<EMType, string> OrderTypeMapper = new Dictionary<EMType, string>();

        /// <summary>
        /// 初始化TemplateBase类的新实例
        /// </summary>
        public TemplateBase()
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
        internal virtual string CreateSelect(string field)
        {
            return string.Format("SELECT {0} ", field ?? "*");
        }

        internal virtual string CreateFrom(string tableName, string aliasName)
        {
            return string.Format(" FROM {0} AS {1}", tableName, aliasName);
        }

        /// <summary>
        /// 添加模板
        /// </summary>
        internal virtual StringBuilder CreateInsert(string tableName, string fields, string placeHolders)
        {
            var s = $@"INSERT {tableName} ({fields}) VALUES ({placeHolders}) {Identity} ";
            return new StringBuilder(s);
        }

        /// <summary>
        /// 更新模板
        /// </summary>
        internal abstract StringBuilder CreateUpdate(string tableName, string aliasName, string placeHolders);

        /// <summary>
        /// 追加关系类型
        /// </summary>
        protected abstract void AppendPredicateType();

        /// <summary>
        /// 返回新增主键
        /// </summary>
        /// <value></value>
        internal abstract string Identity { get; }

        /// <summary>
        /// 执行受影响的行数
        /// </summary>
        /// <value></value>
        internal abstract string AffectedRows { get; }
         
        /// <summary>
        /// 追加分页语句
        /// </summary> 
        internal abstract void CreatePagination(PaginationComponent pagination, string orderBy, StringBuilder rawSql);

        /// <summary>
        /// 创建谓词关系
        /// </summary> 
        internal abstract string CreatePredicate(EMType predicateType, string left, string right);

        /// <summary>
        /// 创建参数
        /// </summary> 
        internal abstract DbParameter CreateParameter(string key, Object value, Type dataType);

        /// <summary>
        /// 创建连接
        /// </summary> 
        internal abstract DbConnection CreateDbConnection(string connectionString);

        /// <summary>
        /// 创建连接关系
        /// </summary> 
        internal StringBuilder CreateJoin(EMType joinRelation, string left, string right)
        {
            Check.IfNullOrZero(joinRelation);
            Check.IfNullOrZero(left);
            Check.IfNullOrZero(right);

            if (!JoinMapper.ContainsKey(joinRelation))
            {
                throw new ArgumentNullException($@"{joinRelation}不存在");
            }

            return new StringBuilder().AppendFormat(string.Format(JoinMapper[joinRelation], left, right));
        }

        /// <summary>
        /// 排序语句构建
        /// </summary> 
        internal string CreateOrderBy(EMType orderByType, string left)
        {
            Check.IfNullOrZero(orderByType);
            Check.IfNullOrZero(left);

            if (!OrderTypeMapper.ContainsKey(orderByType))
            {
                throw new ArgumentNullException($@"{orderByType}不存在");
            }
            return string.Format(OrderTypeMapper[orderByType], left);
        }

        /// <summary>
        /// 将类型转换为数据库类型
        /// </summary> 
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
            PredicateMapper.Add(EMType.AND, " {0} AND {1} ");
            PredicateMapper.Add(EMType.OR, " {0} OR {1} ");
            PredicateMapper.Add(EMType.EQ, " {0} = {1} ");
            PredicateMapper.Add(EMType.NQ, " {0} <> {1} ");
            PredicateMapper.Add(EMType.GT, " {0} < {1} ");
            PredicateMapper.Add(EMType.LT, " {0} > {1} ");
            PredicateMapper.Add(EMType.GE, " {0} <= {1} ");
            PredicateMapper.Add(EMType.LE, " {0} >= {1} ");

            AppendPredicateType();
        }

        /// <summary>
        /// 初始化默认连接语句
        /// </summary>
        private void InitJoinType()
        {
            JoinMapper.Add(EMType.INNER, " INNER JOIN {0} AS {1} ON ");
            JoinMapper.Add(EMType.LEFT, " LEFT JOIN {0} AS {1} ON ");
            JoinMapper.Add(EMType.RIGHT, " RIGHT JOIN {0} AS {1} ON ");
        }

        /// <summary>
        /// 初始化排序类型
        /// </summary>
        private void InitOrderType()
        {
            OrderTypeMapper.Add(EMType.ASC, " ORDER BY {0} ASC ");
            OrderTypeMapper.Add(EMType.DESC, " ORDER BY {0} DESC ");
        }
    }
}

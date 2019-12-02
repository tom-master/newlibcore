
namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 执行的类型
    /// </summary>
    internal enum ExecuteType
    {
        NONE = 0,

        SELECT = 1,

        UPDATE = 2,

        INSERT = 3,
    }

    internal enum MsSqlPaginationVersion
    {
        None = 1,

        /// <summary>
        /// 大于2012版本
        /// </summary>
        GreaterThan2012 = 2,

        /// <summary>
        /// 小于2012版本
        /// </summary>
        LessThen2012 = 3
    }

    /// <summary>
    /// 数据库类型
    /// </summary>
    public enum MapperType
    {
        NONE = 0,
        MSSQL = 1,
        MYSQL = 2
    }

    /// <summary>
    /// 排序类型
    /// </summary>
    internal enum OrderByType
    {
        ASC = 1,
        DESC = 2
    }

    /// <summary>
    /// 连接类型
    /// </summary>
    internal enum JoinRelation
    {
        NONE = 0,
        INNER = 1,
        LEFT = 2,
        RIGHT = 3,
        CROSS = 4,
        SEIF = 5
    }

    /// <summary>
    /// 谓词类型
    /// </summary>
    internal enum PredicateType
    {
        AND = 1,
        OR = 2,
        FULL_LIKE = 3,
        START_LIKE = 4,
        END_LIKE = 5,
        IN = 6,
        EQ = 7,
        NQ = 8,
        GT = 9,
        LT = 10,
        GE = 11,
        LE = 12
    }
}

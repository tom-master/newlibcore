
namespace NewLibCore.Data.SQL
{
    /// <summary>
    /// 执行的类型
    /// </summary>
    internal enum ExecuteType
    {
        NONE = 0,

        /// <summary>
        /// 选择
        /// </summary>
        SELECT = 1,

        /// <summary>
        /// 更新
        /// </summary>
        UPDATE = 2,

        /// <summary>
        /// 插入
        /// </summary>
        INSERT = 3,
    }

    internal enum MsSqlPaginationVersion
    {
        NONE = 1,

        /// <summary>
        /// 大于2012版本
        /// </summary>
        GREATERTHAN2012 = 2,

        /// <summary>
        /// 小于2012版本
        /// </summary>
        LESSTHEN2012 = 3
    }

    /// <summary>
    /// 数据库类型
    /// </summary>
    internal enum MapperType
    {
        NONE = 0,

        /// <summary>
        /// mssql
        /// </summary>
        MSSQL = 1,

        /// <summary>
        /// mysql
        /// </summary>
        MYSQL = 2
    }

    /// <summary>
    /// 排序类型
    /// </summary>
    internal enum OrderByType
    {
        /// <summary>
        /// 升序
        /// </summary>
        ASC = 1,

        /// <summary>
        /// 降序
        /// </summary>
        DESC = 2
    }

    /// <summary>
    /// 连接类型
    /// </summary>
    internal enum JoinRelation
    {
        NONE = 0,

        /// <summary>
        /// 内连接
        /// </summary>
        INNER = 1,

        /// <summary>
        /// 左连接
        /// </summary>
        LEFT = 2,

        /// <summary>
        /// 右连接
        /// </summary>
        RIGHT = 3,

        /// <summary>
        /// 交叉连接
        /// </summary>
        CROSS = 4,

        /// <summary>
        /// 自连接
        /// </summary>
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


namespace NewLibCore.Storage.SQL
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

    internal enum EMType
    {
        NONE = 0,
        AND = 1,
        OR = 2,
        FULL_LIKE = 3,
        START_LIKE = 4,
        END_LIKE = 5,
        IN = 6,

        Equal = 7,
        NotEqual = 8,
        GreaterThan = 9,
        LessThan = 10,
        GreaterThanOrEqual = 11,
        LessThanOrEqual = 12,

        FROM = 13,
        WHERE = 14,

        INNER = 15,
        LEFT = 16,
        RIGHT = 17,
        CROSS = 18,
        SEIF = 19,

        SELECT = 20,
        ASC = 21,
        DESC = 22
    }
}

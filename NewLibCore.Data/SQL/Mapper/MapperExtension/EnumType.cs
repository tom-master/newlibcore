using System.ComponentModel;

namespace NewLibCore.Data.SQL.Mapper.Extension
{
    /// <summary>
    /// 执行的类型
    /// </summary>
    public enum ExecuteType
    {
        SELECT = 1,
        UPDATE = 2,
        INSERT = 3,
        SELECT_SINGLE = 4
    }

    /// <summary>
    /// 数据库类型
    /// </summary>
    public enum DatabaseType
    {
        NONE = 0,
        MSSQL = 1,
        MYSQL = 2
    }

    /// <summary>
    /// 排序类型
    /// </summary>
    public enum OrderByType
    {
        ASC = 1,
        DESC = 2
    }

    /// <summary>
    /// 连接类型
    /// </summary>
    public enum JoinType
    {
        NONE = 0,
        INNER = 1,
        LEFT = 2,
        RIGHT = 3
    }

    /// <summary>
    /// 关系类型
    /// </summary>
    internal enum RelationType
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

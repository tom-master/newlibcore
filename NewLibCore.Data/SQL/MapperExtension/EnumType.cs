using System.ComponentModel;

namespace NewLibCore.Data.SQL.MapperExtension
{
    public enum ExecuteType
    {
        SELECT = 1,

        UPDATE = 2,

        INSERT = 3,

        SELECTSINGLE = 4
    }

    public enum DatabaseType
    {
        NONE = 0,

        MSSQL = 1,

        MYSQL = 2
    }

    public enum OrderByType
    {
        [Description(" ORDER BY {0} ASC ")]
        ASC = 1,

        [Description(" ORDER BY {0} DESC ")]
        DESC = 2
    }

    public enum JoinType
    {
        NONE = 0,

        [Description(" INNER JOIN {0} AS {1} ON ")]
        INNER = 1,

        [Description(" LEFT JOIN {0} AS {1} ON ")]
        LEFT = 2,

        [Description("{0} RIGHT JOIN {1}")]
        RIGHT = 3
    }

    internal enum RelationType
    {
        [Description("{0} AND {1}")]
        AND = 1,

        [Description("{0} OR {1}")]
        OR = 2,

        [Description("{0} LIKE '%@{1}%'")]
        FULL_LIKE = 3,

        [Description("{0} LIKE '@{1}%'")]
        START_LIKE = 4,

        [Description("{0} LIKE '%@{1}'")]
        END_LIKE = 5,

        [Description("{0} IN (@{1})")]
        IN = 6,

        [Description("{0} = {1}")]
        EQ = 7,

        [Description("{0} <> {1}")]
        NQ = 8,

        [Description("{0} < {1}")]
        GT = 9,

        [Description("{0} > {1}")]
        LT = 10,

        [Description("{0} <= {1}")]
        GE = 11,

        [Description("{0} >= {1}")]
        LE = 12
    }
}

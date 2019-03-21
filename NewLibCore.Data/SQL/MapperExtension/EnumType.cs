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
        [StatementTemplate(" ORDER BY {0} ASC ")]
        ASC = 1,

        [StatementTemplate(" ORDER BY {0} DESC ")]
        DESC = 2
    }

    public enum JoinType
    {
        NONE = 0,

        [StatementTemplate("{left} INNER JOIN {right}")]
        INNER = 1,

        [StatementTemplate("{left} LEFT JOIN {right}")]
        LEFT = 2,

        [StatementTemplate("{left} RIGHT JOIN {right}")]
        RIGHT = 3
    }

    internal enum RelationType
    {
        [StatementTemplate("{left} AND {right}")]
        AND = 1,

        [StatementTemplate("{left} OR {right}")]
        OR = 2,

        [StatementTemplate("{left} LIKE (%{right}%)")]
        FULL_LIKE = 3,

        [StatementTemplate("{left} LIKE ({right}%)")]
        START_LIKE = 4,

        [StatementTemplate("{left} LIKE (%{right})")]
        END_LIKE = 5,

        [StatementTemplate("{left} IN {right}")]
        IN = 6,

        [StatementTemplate("{left} = {right}")]
        EQ = 7,

        [StatementTemplate("{left} <> {right}")]
        NQ = 8,

        [StatementTemplate("{left} < {right}")]
        GT = 9,

        [StatementTemplate("{left} > {right}")]
        LT = 10,

        [StatementTemplate("{left} <= {right}")]
        GE = 11,

        [StatementTemplate("{left} >= {right}")]
        LE = 12
    }
}

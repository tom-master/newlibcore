using System.ComponentModel;

namespace NewLibCore.Data.SQL.BuildExtension
{

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

        [Description("INNER JOIN")]
        INNER = 1,

        [Description("LEFT JOIN")]
        LEFT = 2,

        [Description("RIGHT JOIN")]
        RIGHT = 3
    }

    internal enum RelationType
    {
        [Description(" AND ")]
        AND = 1,

        [Description(" OR ")]
        OR = 2,

        [Description(" LIKE ")]
        LIKE = 3,

        START_LIKE = 4,

        END_LIKE = 5,

        [Description(" IN ")]
        IN = 6,

        [Description(" = ")]
        EQ = 7,

        [Description(" <> ")]
        NQ = 8,

        [Description(" < ")]
        GT = 9,

        [Description(" > ")]
        LT = 10,

        [Description(" <= ")]
        GE = 11,

        [Description(" >= ")]
        LE = 12
    }
}

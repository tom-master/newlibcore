using System.ComponentModel;

namespace NewLibCore.Data.SQL.BuildExtension
{
    public enum JoinType
    {
        None = 0,

        [Description("INNER JOIN")]
        Inner = 1,

        [Description("LEFT JOIN")]
        Left = 2,

        [Description("RIGHT JOIN")]
        Right = 3
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

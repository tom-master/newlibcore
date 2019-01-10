using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Text;

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
        AND = 1,

        OR = 2,

        LIKE = 3,

        START_LIKE = 4,

        END_LIKE = 5,

        IN = 6,

        EQ = 7,

        NQ = 8,

        GT = 9,

        LT = 10,

        GE = 11,

        LE = 12,
    }
}

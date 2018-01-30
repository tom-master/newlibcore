using System.ComponentModel;

namespace NewLibCore.Data.SQL.BuildExtension
{
    public enum JoinType
    {
        [Description("Inner")]
        Inner = 1,

        [Description("Left")]
        Left = 2,

        [Description("Left")]
        Right = 3
    }
}

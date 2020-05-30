using System.ComponentModel;

namespace NewLibCore.UnitTest.ValueObject
{
    public enum UserType
    {
        [Description("普通用户")]
        User = 1,

        [Description("管理员")]
        Admin = 2
    }
}

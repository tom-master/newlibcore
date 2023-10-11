using System;
using NewLibCore.Storage.SQL;
using NewLibCore.Storage.SQL.Validate;

namespace NewLibCore.UnitTest.Entitys.Agent
{
    [TableName("newcrm_user_role", "r")]
    public class UserRole: EntityBase
    {
        [Required]
        public Int32 UserId { get; private set; }

        [Required]
        public Int32 RoleId { get; private set; }

        public UserRole(Int32 userId, Int32 roleId)
        {
            UserId = userId;
            RoleId = roleId;
        }

        public UserRole() { }
    }
}

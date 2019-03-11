using NewLibCore.Data.SQL.InternalDataStore;
using NewLibCore.Data.SQL.MapperExtension;
using NewLibCore.Data.SQL.PropertyExtension;
using System;
using System.ComponentModel;

namespace NewLibCore.Run
{
    internal class Program
    {
        private static void Main(String[] args)
        {
            SwitchDatabase.SwitchTo(Database.MYSQL);
            using (var context = new SqlContext())
            {
                context.LeftJoin<VisitorRecord, User>((a, b) => a.UserId == b.Id).Find<VisitorRecord>(e => e.UserName != "");
            }
        }
    }

    [Description("用户"), Serializable]
    public partial class User : DomainModelBase
    {
        /// <summary>
        /// 用户名
        /// </summary>
        [PropertyRequired, PropertyInputRange(4, 10)]
        public String Name { get; private set; }

        /// <summary>
        /// 登陆密码
        /// </summary>
        [PropertyRequired]
        public String LoginPassword { get; private set; }

        /// <summary>
        /// 锁屏密码
        /// </summary>
        [PropertyRequired]
        public String LockScreenPassword { get; private set; }

        /// <summary>
        /// 是否禁用
        /// </summary>
        [PropertyDefaultValue(typeof(Boolean))]
        public Boolean IsDisable { get; private set; }

        /// <summary>
        /// 最后一次登录的时间
        /// </summary>
        [DateTimeDefaultValue]
        public DateTime LastLoginTime { get; private set; }

        /// <summary>
        /// 是否在线
        /// </summary>
        [PropertyDefaultValue(typeof(Boolean))]
        public Boolean IsOnline { get; private set; }

        /// <summary>
        /// 是否为管理员
        /// </summary>
        [PropertyDefaultValue(typeof(Boolean))]
        public Boolean IsAdmin { get; private set; }

        /// <summary>
        /// 配置Id
        /// </summary>
        [PropertyRequired]
        public Int32 ConfigId { get; private set; }

        /// <summary>
        /// 账户头像
        /// </summary>
        public String UserFace { get; private set; }


        public Boolean IsModifyUserFace { get; private set; }

        /// <summary>
        /// 实例化一个用户对象
        /// </summary>
        public User(String name, String password)
        {
            Name = name;
            LoginPassword = password;
            IsDisable = false;
            LastLoginTime = DateTime.Now;
            LockScreenPassword = password;
            IsOnline = false;
        }

        public User() { }
    }

    public partial class User
    {
        /// <summary>
        /// 修改登陆密码
        /// </summary>
        public User ModifyLoginPassword(String password)
        {
            if (String.IsNullOrEmpty(password))
            {
                throw new ArgumentException($@"{nameof(LoginPassword)}不能为空");
            }

            LoginPassword = password;
            OnPropertyChanged(nameof(LoginPassword));
            return this;
        }

        /// <summary>
        /// 修改锁屏密码
        /// </summary>
        public User ModifyLockScreenPassword(String password)
        {
            if (String.IsNullOrEmpty(password))
            {
                throw new ArgumentException($@"{nameof(LockScreenPassword)}不能为空");
            }

            LockScreenPassword = password;
            OnPropertyChanged(nameof(LockScreenPassword));
            return this;
        }

        /// <summary>
        /// 修改关联配置文件Id
        /// </summary>
        public User ModifyConfigId(Int32 configId)
        {
            ConfigId = configId;
            OnPropertyChanged(nameof(ConfigId));
            return this;
        }

        /// <summary>
        /// 账户启用
        /// </summary>
        public User Enable()
        {
            IsDisable = false;
            OnPropertyChanged(nameof(IsDisable));
            return this;
        }

        /// <summary>
        /// 账户禁用
        /// </summary>
        public User Disable()
        {
            IsDisable = true;
            OnPropertyChanged(nameof(IsDisable));
            return this;
        }

        /// <summary>
        /// 上线
        /// </summary>
        /// <returns></returns>
        public User Online()
        {
            IsOnline = true;
            OnPropertyChanged(nameof(IsOnline));

            LastLoginTime = DateTime.Now;
            OnPropertyChanged(nameof(LastLoginTime));
            return this;
        }

        /// <summary>
        /// 下线
        /// </summary>
        /// <returns></returns>
        public User Offline()
        {
            IsOnline = false;
            OnPropertyChanged(nameof(IsOnline));
            return this;
        }

        /// <summary>
        /// 修改角色
        /// </summary>
        public User ModifyRoles(params Int32[] roleIds)
        {
            if (roleIds.Length == 0)
            {
                return this;
            }
            return this;
        }

        /// <summary>
        /// 去除管理员角色
        /// </summary>
        public User DetachAdminRole()
        {
            IsAdmin = false;
            OnPropertyChanged(nameof(IsAdmin));
            return this;
        }

        /// <summary>
        /// 附加管理员角色
        /// </summary>
        /// <returns></returns>
        public User AttachAdminRole()
        {
            IsAdmin = true;
            OnPropertyChanged(nameof(IsAdmin));
            return this;
        }
    }

    public partial class VisitorRecord : DomainModelBase
    {
        public Int32 UserId { get; private set; }

        [PropertyRequired, PropertyInputRange(10), PropertyDefaultValue(typeof(String), "11111")]
        public String UserName { get; private set; }

        public VisitorRecord(Int32 userId, String userName)
        {
            UserId = userId;
            UserName = userName;
        }

        public VisitorRecord() { }
    }
}

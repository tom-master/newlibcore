using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading;
using NewLibCore.Data.SQL.Mapper;
using NewLibCore.Data.SQL.Mapper.Validate;
using Newtonsoft.Json;

namespace NewLibCore.Run
{
    public class Program
    {

        public static void Main(String[] args)
        {
            #region  

            EntityMapper.InitDefaultSetting();

            //EntityMapper.UseMySql();
            //EntityMapper.UseMsSql();

            EntityMapper.ConnectionStringName = "NewCrmDatabase";

            // for (var i = 0; i < 4; i++)
            // {
            //     var thread = new Thread(new ParameterizedThreadStart((a) =>
            //     {
            //         using (var mapper = EntityMapper.CreateMapper())
            //         {
            //             var result = mapper.Query<User>().FirstOrDefault();
            //             Console.WriteLine(JsonConvert.SerializeObject(result));
            //         }
            //     }));
            //     thread.Start();
            // }

            using (var mapper = EntityMapper.CreateMapper())
            {
                var sw = new Stopwatch();
                sw.Start();

                // var user = new User("xiaofan","xiaofan@.1");
                // mapper.Add(user);

                //var a = mapper.Query<User>().FirstOrDefault();
                for (var i = 0; i < 10; i++)
                {
                    var r = mapper.Query<User>().Page(1, 10).ThenByDesc(a => a.Id).ToList();
                }

                sw.Stop();
                Console.WriteLine($@"共花费{Math.Round(sw.Elapsed.TotalSeconds, 4)}秒");

                //var result = mapper.Query<User>().FirstOrDefault();
                //var result = mapper.Query<User>().ToList();
                //var result = mapper.Query<User>().Select(u => new { u.Id, u.Name, u.LoginPassword }).FirstOrDefault();
                //var result = mapper.Query<User>().Select(u => new { u.Id, u.Name, u.LoginPassword }).ToList();
                //var result = mapper.Query<Config>().InnerJoin<User>((c, u) => c.UserId == u.Id).FirstOrDefault();
                //var result = mapper.Query<Config>().InnerJoin<User>((c, u) => c.UserId == u.Id).ToList();
                //var result = mapper.Query<App>().RightJoin<Member>((a, m) => a.Id == m.AppId).ToList();
            }

            #endregion


            Console.Read();
        }
    }

    public class TestModel
    {
        public Int32 Id { get; set; }

        public String Name { get; set; }
    }

    public enum WallpaperSource
    {
        System = 1,

        Web = 2,

        Upload = 3,

        Bing = 4
    }

    [TableName("newcrm_wallpaper", "wallpaper")]
    public partial class Wallpaper : EntityBase
    {
        /// <summary>
        /// 标题
        /// </summary>
        [Required, InputRange(15)]
        public String Title { get; private set; }

        /// <summary>
        /// 图片地址
        /// </summary>
        [Required, InputRange(150)]
        public String Url { get; private set; }

        /// <summary>
        /// 短地址
        /// </summary> 
        [DefaultValue(typeof(String))]
        public String ShortUrl { get; private set; }

        /// <summary>
        /// 来源
        /// </summary>
        [Required, DefaultValue(WallpaperSource.System)]
        public WallpaperSource Source { get; private set; }

        /// <summary>
        /// 描述
        /// </summary>
        [DefaultValue(typeof(String)), InputRange(50)]
        public String Description { get; private set; }

        /// <summary>
        /// 图片的宽
        /// </summary>
        [Required]
        public Int32 Width { get; private set; }

        /// <summary>
        /// 图片的高
        /// </summary>
        [Required]
        public Int32 Height { get; private set; }

        /// <summary>
        /// 上传者（用户）
        /// </summary>
        [Required]
        public Int32 UserId { get; private set; }

        /// <summary>
        /// md5
        /// </summary>
        [Required]
        public String Md5 { get; private set; }

        [DefaultValue(typeof(Guid))]
        public Guid? Guid { get; private set; }

        /// <summary>
        /// 实例化一个壁纸对象
        /// </summary>
        public Wallpaper(String title, String url, String description, Int32 width, Int32 height, String md5, Int32 userId = 0, WallpaperSource wallpaperSource = default(WallpaperSource))
        {
            Title = title;
            Url = url;
            Description = description;
            Width = width;
            Height = height;
            Source = wallpaperSource;
            UserId = userId;
            Md5 = md5;
        }

        public Wallpaper()
        {

        }
    }

    [TableName("newcrm_user_config", "config")]
    public partial class Config : EntityBase
    {
        /// <summary> 
        /// 皮肤
        /// </summary> 
        [Required, InputRange(10)]
        public String Skin { get; private set; }

        /// <summary>
        /// 用户头像
        /// </summary>
        [Required, InputRange(150)]
        public String UserFace { get; private set; }

        /// <summary>
        /// app尺寸
        /// </summary>
        [Required]
        public Int32 AppSize { get; private set; }

        /// <summary>
        /// app垂直间距
        /// </summary>
        [Required]
        public Int32 AppVerticalSpacing { get; private set; }

        /// <summary>
        /// app水平间距
        /// </summary>
        [Required]
        public Int32 AppHorizontalSpacing { get; private set; }

        /// <summary>
        /// 默认桌面编号
        /// </summary>
        [DefaultValue(typeof(Int32), 1)]
        public Int32 DefaultDeskNumber { get; private set; }

        /// <summary>
        /// 默认桌面数量
        /// </summary>
        [DefaultValue(typeof(Int32), 5)]
        public Int32 DefaultDeskCount { get; private set; }

        /// <summary>
        /// 壁纸来源
        /// </summary>
        [DefaultValue(typeof(Boolean))]
        public Boolean IsBing { get; private set; }

        /// <summary>
        /// 壁纸Id
        /// </summary>
        [Required, ForeignKey(typeof(Wallpaper))]
        public Int32 WallpaperId { get; private set; }

        public Wallpaper Wallpaper { get; set; }

        /// <summary>
        /// 账户头像是否被更改
        /// </summary>
        [Required]
        public Boolean IsModifyUserFace { get; private set; }

        public Config()
        {
            UserFace = @"/images/ui/avatar_48.jpg";
            Skin = "default";
            AppSize = 48;
            AppVerticalSpacing = 50;
            AppHorizontalSpacing = 50;
            DefaultDeskNumber = 1;
            DefaultDeskCount = 5;
            IsBing = true;
            IsModifyUserFace = false;
            WallpaperId = 3;
        }
    }

    [TableName("newcrm_user_role")]
    public class UserRole : EntityBase
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

    [TableName("newcrm_user", "user")]
    public partial class User : EntityBase
    {
        /// <summary>
        /// 用户名
        /// </summary>
        [Required, InputRange(4, 10)]
        public String Name { get; private set; }

        /// <summary>
        /// 登陆密码
        /// </summary>
        [Required]
        public String LoginPassword { get; private set; }

        /// <summary>
        /// 锁屏密码
        /// </summary>
        [Required]
        public String LockScreenPassword { get; private set; }

        /// <summary>
        /// 是否禁用
        /// </summary>
        [DefaultValue(typeof(Boolean))]
        public Boolean IsDisable { get; private set; }

        /// <summary>
        /// 最后一次登录的时间
        /// </summary>
        [DateTimeDefaultValue]
        public DateTime LastLoginTime { get; private set; }

        /// <summary>
        /// 是否在线
        /// </summary>
        [DefaultValue(typeof(Boolean))]
        public Boolean IsOnline { get; private set; }

        /// <summary>
        /// 是否为管理员
        /// </summary>
        [DefaultValue(typeof(Boolean))]
        public Boolean IsAdmin { get; private set; }

        /// <summary>
        /// 配置Id
        /// </summary>
        [ForeignKey(typeof(Config))]
        public Int32 ConfigId { get; private set; }

        public Config Config { get; set; }

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
            OnChanged(nameof(LoginPassword));
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
            OnChanged(nameof(LockScreenPassword));
            return this;
        }

        /// <summary>
        /// 账户启用
        /// </summary>
        public User Enable()
        {
            IsDisable = false;
            OnChanged(nameof(IsDisable));
            return this;
        }

        /// <summary>
        /// 账户禁用
        /// </summary>
        public User Disable()
        {
            IsDisable = true;
            OnChanged(nameof(IsDisable));
            return this;
        }

        /// <summary>
        /// 上线
        /// </summary>
        /// <returns></returns>
        public User Online()
        {
            IsOnline = true;
            OnChanged(nameof(IsOnline));

            LastLoginTime = DateTime.Now;
            OnChanged(nameof(LastLoginTime));
            return this;
        }

        /// <summary>
        /// 下线
        /// </summary>
        /// <returns></returns>
        public User Offline()
        {
            IsOnline = false;
            OnChanged(nameof(IsOnline));
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
            OnChanged(nameof(IsAdmin));
            return this;
        }

        /// <summary>
        /// 附加管理员角色
        /// </summary>
        /// <returns></returns>
        public User AttachAdminRole()
        {
            IsAdmin = true;
            OnChanged(nameof(IsAdmin));
            return this;
        }
    }

    [TableName("newcrm_user_member", "a")]
    public partial class Member : EntityBase
    {
        /// <summary>
        /// 应用Id
        /// </summary>
        [Required]
        public Int32 AppId { get; private set; }

        /// <summary>
        /// 桌面应用的宽
        /// </summary>
        [Required]
        public Int32 Width { get; private set; }

        /// <summary>
        /// 桌面应用的高
        /// </summary>
        [Required]
        public Int32 Height { get; private set; }

        /// <summary>
        /// 文件夹Id
        /// </summary>
        [DefaultValue(typeof(Int32))]
        public Int32 FolderId { get; private set; }

        /// <summary>
        /// 名称
        /// </summary>
        [Required, InputRange(10)]
        public String Name { get; private set; }

        /// <summary>
        /// 图标地址
        /// </summary>
        [Required, InputRange(150)]
        public String IconUrl { get; private set; }

        /// <summary>
        /// app地址
        /// </summary>
        [InputRange(150), DefaultValue(typeof(String))]
        public String AppUrl { get; private set; }

        /// <summary>
        /// 桌面应用是否在应用码头上
        /// </summary>
        [DefaultValue(typeof(Boolean))]
        public Boolean IsOnDock { get; private set; }

        /// <summary>
        /// 是否显示app底部的按钮
        /// </summary>
        [DefaultValue(typeof(Boolean))]
        public Boolean IsSetbar { get; private set; }

        /// <summary>
        /// 是否打开最大化
        /// </summary>
        [DefaultValue(typeof(Boolean))]
        public Boolean IsOpenMax { get; private set; }

        /// <summary>
        /// 是否为福莱希
        /// </summary>
        [DefaultValue(typeof(Boolean))]
        public Boolean IsFlash { get; private set; }

        /// <summary>
        /// 是否可以拉伸
        /// </summary>
        [DefaultValue(typeof(Boolean))]
        public Boolean IsResize { get; private set; }

        /// <summary>
        /// 桌面索引
        /// </summary>
        [DefaultValue(typeof(Int32), 1)]
        public Int32 DeskIndex { get; private set; }

        /// <summary>
        /// 账户Id
        /// </summary>
        [Required]
        public Int32 UserId { get; private set; }

        /// <summary>
        /// 图标是否来自上传
        /// </summary>
        [DefaultValue(typeof(Boolean))]
        public Boolean IsIconByUpload { get; private set; }

        public Double StarCount { get; set; }

        /// <summary>
        /// 实例化一个桌面应用对象
        /// </summary>
        public Member(
                    String name,
                    String iconUrl,
                    String appUrl,
                    Int32 appId,
                    Int32 width,
                    Int32 height,
                    Int32 userId,
                    Int32 deskIndex,
                    Boolean isIconByUpload = default(Boolean),
                    Boolean isSetbar = default(Boolean),
                    Boolean isOpenMax = default(Boolean),
                    Boolean isFlash = default(Boolean),
                    Boolean isResize = default(Boolean))
        {
            AppId = appId;
            Width = width > 800 ? 800 : width;
            Height = height > 600 ? 600 : height;
            IsOpenMax = isOpenMax;
            IsSetbar = isSetbar;
            IsFlash = isFlash;
            IsResize = isResize;
            Name = name;
            IconUrl = iconUrl;
            AppUrl = appUrl;
            DeskIndex = 1;
            IsIconByUpload = isIconByUpload;
            UserId = userId;
        }

        /// <summary>
        /// 实例化一个桌面应用对象
        /// </summary>
        public Member(String name,
            String iconUrl,
            Int32 appId,
            Int32 userId,
            Int32 deskIndex,
            Boolean isIconByUpload = default(Boolean))
        {
            AppId = appId;
            Width = 800;
            Height = 600;
            IsOpenMax = false;
            Name = name;
            IconUrl = iconUrl;
            DeskIndex = deskIndex;
            IsIconByUpload = isIconByUpload;
            UserId = userId;
        }

        public Member()
        {
        }

    }

    public partial class Member
    {

        public Member ModifyWidth(Int32 width)
        {
            if (width <= 0)
            {
                throw new ArgumentException($@"{nameof(width)} 不能小于或等于0");
            }

            if (width == Width)
            {
                return this;
            }

            Width = width;
            OnChanged(nameof(Width));
            return this;
        }

        public Member ModifyHeight(Int32 height)
        {
            if (height <= 0)
            {
                throw new ArgumentException($@"{nameof(height)} 不能小于或等于0");
            }

            if (height == Height)
            {
                return this;
            }

            Height = height;
            OnChanged(nameof(Height));
            return this;
        }

        public Member ModifyFolderId(Int32 folderId)
        {
            if (folderId == FolderId)
            {
                return this;
            }

            FolderId = folderId;
            OnChanged(nameof(FolderId));
            return this;
        }

        public Member ModifyName(String name)
        {
            if (String.IsNullOrEmpty(name))
            {
                throw new ArgumentException($@"{nameof(name)}不能为空");
            }

            if (name == Name)
            {
                return this;
            }

            Name = name;
            OnChanged(nameof(Name));
            return this;
        }

        public Member ModifyIconUrl(String iconUrl)
        {
            if (String.IsNullOrEmpty(iconUrl))
            {
                throw new ArgumentException($@"{nameof(iconUrl)}不能为空");
            }

            if (iconUrl == IconUrl)
            {
                return this;
            }

            IconUrl = iconUrl;
            OnChanged(nameof(IconUrl));
            return this;
        }

        public Member ModifyAppUrl(String appUrl)
        {
            if (String.IsNullOrEmpty(appUrl))
            {
                throw new ArgumentException($@"{nameof(appUrl)}不能为空");
            }

            if (appUrl == AppUrl)
            {
                return this;
            }

            AppUrl = appUrl;
            OnChanged(nameof(AppUrl));
            return this;
        }

        public Member OnDock()
        {
            IsOnDock = true;
            OnChanged(nameof(IsOnDock));
            return this;
        }

        public Member OutDock()
        {
            IsOnDock = false;
            OnChanged(nameof(IsOnDock));
            return this;
        }

        public Member Setbar()
        {
            IsSetbar = true;
            OnChanged(nameof(IsSetbar));
            return this;
        }

        public Member NotSetbar()
        {
            IsSetbar = false;
            OnChanged(nameof(IsSetbar));
            return this;
        }

        public Member OpenMax()
        {
            IsOpenMax = true;
            OnChanged(nameof(IsOpenMax));
            return this;
        }

        public Member NotOpenMax()
        {
            IsOpenMax = false;
            OnChanged(nameof(IsOpenMax));
            return this;
        }

        public Member Flash()
        {
            IsFlash = true;
            OnChanged(nameof(IsFlash));
            return this;
        }

        public Member NotFlash()
        {
            IsFlash = false;
            OnChanged(nameof(IsFlash));
            return this;
        }

        public Member Resize()
        {
            IsResize = true;
            OnChanged(nameof(IsResize));
            return this;
        }

        public Member NotResize()
        {
            IsResize = false;
            OnChanged(nameof(IsResize));
            return this;
        }

        public Member ModifyDeskIndex(Int32 deskIndex)
        {
            if (deskIndex <= 0)
            {
                throw new ArgumentException($@"{nameof(deskIndex)} 不能小于或等于0");
            }

            if (deskIndex == DeskIndex)
            {
                return this;
            }

            DeskIndex = deskIndex;
            OnChanged(nameof(DeskIndex));
            return this;
        }

        public Member IconNotFromUpload()
        {
            IsIconByUpload = false;
            OnChanged(nameof(IsIconByUpload));
            return this;
        }

        public Member IconFromUpload()
        {
            IsIconByUpload = true;
            OnChanged(nameof(IsIconByUpload));
            return this;
        }
    }

    [TableName("newcrm_role")]
    public partial class Role : EntityBase
    {
        /// <summary>
        /// 名称
        /// </summary>
        [Required, InputRange(2, 10)]
        public String Name { get; private set; }

        /// <summary>
        /// 角色标识
        /// </summary>
        [Required, InputRange(2, 20)]
        public String RoleIdentity { get; private set; }

        /// <summary>
        /// 备注
        /// </summary>
        [InputRange(50), DefaultValue(typeof(String))]
        public String Remark { get; private set; }

        /// <summary>
        /// 是否允许禁用
        /// </summary>
        [DefaultValue(typeof(Boolean))]
        public Boolean IsAllowDisable { get; private set; }

        /// <summary>
        /// 权限
        /// </summary>
        public IList<RolePower> Powers { get; private set; }

        /// <summary>
        /// 实例化一个角色对象
        /// </summary>
        public Role(String name, String roleIdentity, String remark = default(String), Boolean isAllowDisable = default(Boolean)) : this()
        {
            Name = name;
            Remark = remark;
            RoleIdentity = roleIdentity;
            IsAllowDisable = isAllowDisable;
        }

        /// <summary>
        /// 实例化一个角色对象
        /// </summary>
        public Role()
        {
            Powers = new List<RolePower>();
        }
    }

    public partial class Role
    {
        /// <summary>
        /// 修改角色名称
        /// </summary>
        public Role ModifyRoleName(String roleName)
        {
            if (String.IsNullOrEmpty(roleName))
            {
                throw new ArgumentException($@"{nameof(roleName)} 不能为空");
            }

            if (roleName == Name)
            {
                return this;
            }

            Name = roleName;
            OnChanged(nameof(Name));
            return this;
        }

        public Role ModifyPower(params Int32[] appIds)
        {
            Powers.Clear();
            foreach (var item in appIds)
            {
                Powers.Add(new RolePower(Id, item));
            }
            return this;
        }

    }

    [TableName("newcrm_role_power")]
    public class RolePower : EntityBase
    {
        [Required]
        public Int32 RoleId { get; private set; }

        [Required]
        public Int32 AppId { get; private set; }

        public RolePower(Int32 roleId, Int32 appId)
        {
            RoleId = roleId;
            AppId = appId;
        }

        public RolePower()
        {

        }
    }

    [TableName("newcrm_app", "a")]
    public partial class App : EntityBase
    {
        /// <summary>
        /// 名称
        /// </summary>
        [Required, InputRange(2, 10)]
        public String Name { get; private set; }

        /// <summary>
        /// 图标地址
        /// </summary>
        [Required, InputRange(150)]
        public String IconUrl { get; private set; }

        /// <summary>
        /// app地址
        /// </summary>
        [Required, InputRange(150)]
        public String AppUrl { get; private set; }

        /// <summary>
        /// 备注
        /// </summary>
        [InputRange(50), DefaultValue(typeof(String))]
        public String Remark { get; private set; }

        /// <summary>
        /// 宽度
        /// </summary>
        [Required]
        public Int32 Width { get; private set; }

        /// <summary>
        /// 高度
        /// </summary>
        [Required]
        public Int32 Height { get; private set; }

        /// <summary>
        /// 使用数
        /// </summary>
        [DefaultValue(typeof(Int32))]
        public Int32 UseCount { get; private set; }

        /// <summary>
        /// 是否显示app底部的按钮
        /// </summary>
        [DefaultValue(typeof(Boolean))]
        public Boolean IsSetbar { get; private set; }

        /// <summary>
        /// 是否打开最大化
        /// </summary>
        [DefaultValue(typeof(Boolean))]
        public Boolean IsOpenMax { get; private set; }

        /// <summary>
        /// 是否为系统应用
        /// </summary>
        [DefaultValue(typeof(Boolean))]
        public Boolean IsSystem { get; private set; }

        /// <summary>
        /// 是否为福莱希
        /// </summary>
        [DefaultValue(typeof(Boolean))]
        public Boolean IsFlash { get; private set; }

        /// <summary>
        /// 是否可以拉伸
        /// </summary>
        [DefaultValue(typeof(Boolean))]
        public Boolean IsResize { get; private set; }

        /// <summary>
        /// 开发者(用户)Id
        /// </summary>
        [Required]
        public Int32 UserId { get; private set; }

        /// <summary>
        /// App类型Id
        /// </summary>
        [Required]
        public Int32 AppTypeId { get; private set; }

        /// <summary>
        /// 是否推荐
        /// </summary>
        [DefaultValue(typeof(Boolean))]
        public Boolean IsRecommand { get; private set; }

        /// <summary>
        /// 是否安装
        /// </summary>
        public Boolean IsInstall { get; private set; }

        /// <summary>
        /// 评价
        /// </summary>
        /// <value></value>
        public Double StarCount { get; private set; }

        /// <summary>
        /// 账户名称
        /// </summary>
        public String UserName { get; private set; }

        /// <summary>
        /// 图标是否来自上传
        /// </summary>
        [DefaultValue(typeof(Boolean))]
        public Boolean IsIconByUpload { get; private set; }

        /// <summary>
        /// 实例化一个app对象
        /// </summary>
        public App(String name,
            String iconUrl,
            String appUrl,
            Int32 width,
            Int32 height,
            Int32 appTypeId,
            Boolean isResize,
            Boolean isOpenMax,
            Boolean isFlash,
            Boolean isSetbar,
            Int32 userId = default(Int32),
            String remark = default(String),
            Boolean isIconByUpload = default(Boolean))
        {
            Name = name;
            IconUrl = iconUrl;
            AppUrl = appUrl;
            Width = width > 800 ? 800 : width;
            Height = height > 600 ? 600 : height;
            AppTypeId = appTypeId;

            if (userId == 0)
            {
                IsSystem = true;
            }
            else
            {
                IsSystem = false;
                UserId = userId;
            }

            Remark = remark;

            UseCount = 0;
            IsRecommand = false;
            IsIconByUpload = isIconByUpload;
        }

        public App() { }
    }


}
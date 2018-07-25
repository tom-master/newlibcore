using System;
using System.Collections.Generic;
using System.ComponentModel;
using NewLibCore.Data.Mapper.MapperExtension;
using NewLibCore.Data.Mapper.PropertyExtension;

namespace UnitTestProject1
{
	internal class Account : PropertyMonitor
	{
		/// <summary>
		/// 用户名
		/// </summary>
		[PropertyRequired, InputRange(4, 10)]
		public String Name { get; private set; }

		/// <summary>
		/// 登陆密码
		/// </summary>
		[PropertyRequired, InputRange(6, 12)]
		public String LoginPassword { get; private set; }

		/// <summary>
		/// 锁屏密码
		/// </summary>
		[PropertyRequired, InputRange(6, 12)]
		public String LockScreenPassword { get; private set; }

		/// <summary>
		/// 是否禁用
		/// </summary>
		[PropertyDefaultValue(typeof(Boolean), false)]
		public Boolean IsDisable { get; private set; }

		/// <summary>
		/// 最后一次登录的时间
		/// </summary>
		[DateTimeDefaultValue]
		public DateTime LastLoginTime { get; private set; }

		/// <summary>
		/// 是否在线
		/// </summary>
		[PropertyDefaultValue(typeof(Boolean), false)]
		public Boolean IsOnline { get; private set; }

		/// <summary>
		/// 是否为管理员
		/// </summary>
		[PropertyDefaultValue(typeof(Boolean), false)]
		public Boolean IsAdmin { get; private set; }

		/// <summary>
		/// 配置Id
		/// </summary>
		[PropertyRequired]
		public Int32 ConfigId { get; private set; }

		public String AccountFace { get; private set; }

		/// <summary>
		/// 用户角色
		/// </summary>
		public IEnumerable<Account> Roles { get; private set; }


		internal void Offline()
		{
			IsOnline = true;
			base.OnPropertyChanged(new PropertyArgs(nameof(IsOnline), IsOnline));
		}

		internal void ModifyName()
		{
			Name = "admin";
			OnPropertyChanged(new PropertyArgs(nameof(Name), Name));
		}
	}
}
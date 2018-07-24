using System;
using System.Collections.Generic;
using NewLibCore.Data.Mapper.MapperExtension;
using NewLibCore.Data.Mapper.PropertyExtension;

namespace UnitTestProject1
{
	internal class Account : PropertyMonitor
	{
		public Account()
		{
		}

		public Int32 Id { get; internal set; }

		[PropertyRequired, InputRange(2, 6)]
		public String Name { get; set; }

		[PropertyRequired, InputRange(6, 12)]
		public String Password { get; set; }

		[PropertyDefaultValue(typeof(Boolean), false)]
		public Boolean IsOnline { get; internal set; }

		[PropertyDefaultValue(typeof(Boolean), false)]
		public Boolean IsDeleted { get; internal set; }

		[PropertyDefaultValue(typeof(Boolean), false)]
		public Boolean IsDisable { get; internal set; }

		[DateTimeDefaultValue]
		public DateTime AddTime { get; internal set; }

		public IList<Int32> RoleIds { get; internal set; }

		internal void Offline()
		{
			IsOnline = true;
			base.OnPropertyChanged(new PropertyArgs(nameof(IsOnline), IsOnline));
		}

		internal void ModifyName()
		{
			Name = "123456";
			OnPropertyChanged(new PropertyArgs(nameof(Name), Name));
		}

		internal void ModifyTime()
		{
			AddTime = DateTime.Now.AddDays(10);
			OnPropertyChanged(new PropertyArgs(nameof(AddTime), AddTime));
		}
	}
}
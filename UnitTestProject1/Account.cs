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
		
		public String Name { get; set; }
		
		public String Password { get; set; }
		
		public Boolean IsOnline { get; internal set; }
		
		public Boolean IsDeleted { get; internal set; }
		
		public Boolean IsDisable { get; internal set; }
		
		public DateTime AddTime { get; internal set; }

		public IList<Int32> RoleIds { get; internal set; }

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

		internal void ModifyTime()
		{
			AddTime = DateTime.Now.AddDays(10);
			OnPropertyChanged(new PropertyArgs(nameof(AddTime), AddTime));
		}
	}
}
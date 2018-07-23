using System;
using System.Collections.Generic;
using NewLibCore.Data.Mapper.PropertyExtension;

namespace UnitTestProject1
{
	internal class Account : PropertyMonitor
	{
		public Account()
		{
		}

		public String Name { get; set; }

		public System.Int32 Id { get; internal set; }
		public System.Boolean IsDeleted { get; internal set; }
		public System.Boolean IsDisable { get; internal set; }

		public Boolean IsOnline { get; internal set; }

		public IList<Int32> RoleIds { get; internal set; }

		internal void Offline()
		{
			IsOnline = true;
			base.OnPropertyChanged(new PropertyArgs(nameof(IsOnline), IsOnline));
		}

		internal void ModifyName()
		{
			Name = "wasd";
			OnPropertyChanged(new PropertyArgs(nameof(Name), Name));
		}
	}

	internal class Account2 : PropertyMonitor
	{
		public Account2()
		{
		}

		public String Name { get; set; }

		public System.Int32 Id { get; internal set; }
		public System.Boolean IsDeleted { get; internal set; }
		public System.Boolean IsDisable { get; internal set; }

		public Boolean IsOnline { get; internal set; }

		internal void Offline()
		{
			IsOnline = true;
			//base.OnPropertyChanged(new PropertyArgs(nameof(IsOnline), IsOnline));
		}
	}
}
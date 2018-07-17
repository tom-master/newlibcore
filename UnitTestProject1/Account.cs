using System;
using NewLibCore.Data.Mapper.PropertyExtension;

namespace UnitTestProject1
{
	internal class Account: PropertyMonitor
	{
		public Account()
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
			base.OnPropertyChanged(new PropertyArgs(nameof(IsOnline), IsOnline));
		}
	}
}
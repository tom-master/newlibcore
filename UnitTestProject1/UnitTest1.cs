using System;
using System.Collections.Generic;
using System.Data.Common;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using System.Text.RegularExpressions;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using NewLibCore.Data.Mapper.DataExtension;
using NewLibCore.Data.Mapper.InternalDataStore;
using NewLibCore.Data.Mapper.MapperExtension;
using NewLibCore.Data.Mapper.PropertyExtension;


namespace UnitTestProject1
{
	[TestClass]
	public class UnitTest1
	{
		[TestMethod]
		public void TestMethod1()
		{
			using (var dataStore = new DataStore("", true))
			{
				var model = new Model();
				model.ChangeAddTime();
				model.ChangeAge();
				model.ChangeIsDelete();
				model.ChangeName();
				dataStore.Modify(model, m => m.Name.Contains(model.Name) && !m.IsDelete);
			}
		}
	}


	public class Model : PropertyMonitor
	{
		public Int32 Id { get; set; }

		public String Name { get; set; }

		public Int32 Age { get; set; }

		public Boolean IsDelete { get; set; }

		public DateTime AddTime { get; set; }


		public void ChangeName()
		{
			Name = "wasd";
			OnPropertyChanged(new PropertyArgs(nameof(Name), Name));
		}

		public void ChangeAge()
		{
			Age = new Random(DateTime.Now.Millisecond).Next(0, 5);
			OnPropertyChanged(new PropertyArgs(nameof(Age), Age));
		}

		public void ChangeIsDelete()
		{
			IsDelete = true;
			OnPropertyChanged(new PropertyArgs(nameof(IsDelete), IsDelete));
		}

		public void ChangeAddTime()
		{
			AddTime = DateTime.Now;
			OnPropertyChanged(new PropertyArgs(nameof(AddTime), AddTime));
		}
	}
}

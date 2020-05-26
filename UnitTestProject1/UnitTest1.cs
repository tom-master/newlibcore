using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using NewLibCore.Data.SQL.Mapper;
using NewLibCore.Data.SQL.Mapper.Validate;

namespace UnitTestProject1
{
	[TestClass]
	public class UnitTest1
	{
		[TestMethod]
		public void TestMethod1()
		{
			var instance = new TestModel();
			instance.OnChanged();
			instance.Validate();
		}
	}

	public class TestModel : EntityBase
	{
		[DefaultValue("789")]
		public DateTime Name { get; private set; }

		public TestModel(DateTime name) 
		{
			Name = name;
		}
		public TestModel() { }
	}
}

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
			var instance = new TestModel("xiaofan");
			instance.OnChanged();
			instance.Validate();
		}
	}

	public class TestModel : EntityBase
	{
		[DefaultValue("123")]
		public string Name { get; private set; }

		public TestModel(string name) 
		{
			Name = name;
		}
	}
}

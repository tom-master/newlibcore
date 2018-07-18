using System;
using System.Collections.Generic;
using System.Data.Common;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using System.Text.RegularExpressions;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using NewLibCore.Data.Mapper.DataExtension;
using NewLibCore.Data.Mapper.InternalDataStore;
using NewLibCore.Data.Mapper.MapperExtension;
using NewLibCore.Data.Mapper.PropertyExtension;
using Newtonsoft.Json;
using StackExchange.Redis;

namespace UnitTestProject1
{
	[TestClass]
	public class UnitTest1
	{
		[TestMethod]
		public void TestMethod1()
		{

			//var str = "1.2.3.4.5.6.wwwwwwwwwwww";
			//var b = str.Substring(str.LastIndexOf(".") + 1);


			//var dataStore = new DataStore("Server=39.106.106.137;Database=NewCrmContext;Uid=root;Pwd=xiaofan@.1;port=6033;SslMode=none;Pooling=True;Min Pool Size=5;Max Pool Size=10");
			//var sql = $@"SELECT COUNT(*) FROM Config AS a WHERE a.IsDeleted=0";

			//var a = dataStore.FindSingleValue<Int32>(sql);

			//using (var dataStore = new DataStore(""))
			//{
			//	var a = new ClassA();
			//	a.ModelA();
			//	//dataStore.ExecuteModify(a);


			//	var b = new ClassB();
			//	b.ModelC();
			//	dataStore.ExecuteModify(b);
			//}
			//var a = "/root/files/1/Icon/small_79d3be223b4747de8f53526de9c0a754.png";

			//var b = a.Replace("/root/files","");

			//var a = typeof(classA).GetProperties();
			//foreach (var item in a)
			//{
			//	var a1 = item.DeclaringType;
			//	var a2 = item.MemberType;
			//	var a3 = item.PropertyType;
			//	var a4 = item.ReflectedType;
			//}

			//var r = ConvertExtension.ChangeType(0, typeof(Boolean));

			//RedisValue redisValue = "{a:1,b:2}";

			//var r = JsonConvert.DeserializeObject<dynamic>(redisValue);

			//var skinPath = $@"D:\dev\NewCrmCore\NewCrmCore.Web\wwwroot\images\skins";
			//IDictionary<String, dynamic> dataDictionary = new Dictionary<String, dynamic>();
			//Directory.GetFiles(skinPath, "*.css").ToList().ForEach(path =>
			//{
			//	var fileName = Path.GetFileName(path);
			//	fileName = fileName.Substring(0, fileName.LastIndexOf("."));
			//	var cssPath = path.Substring(path.LastIndexOf("images")-1).Replace(@"\", "/");
			//	dataDictionary.Add(fileName, new
			//	{
			//		cssPath,
			//		imgPath = $@"{cssPath.Substring(0, cssPath.LastIndexOf("."))}/preview.png"
			//	});
			//});

			//using (var dataStore = new DataStore("Server=39.106.106.137;Database=NewCrmContext;Uid=root;Pwd=xiaofan@.1;port=6033;SslMode=none;Pooling=True;Min Pool Size=5;Max Pool Size=10;Treat Tiny As Boolean=false"))
			//{
			//	try
			//	{
			//		dataStore.OpenTransaction();
			//		#region 设置用户下线
			//		{
			//			var account = new Account();
			//			account.Offline();
			//			var a1 = "str";
			//			var rowCount = dataStore.ExecuteModify(account, acc => acc.Id == 4 || acc.Name.Contains(a1) && !acc.IsDeleted && !acc.IsDisable);
			//			if (rowCount == 0)
			//			{

			//			}
			//		}
			//		#endregion

			//		dataStore.Commit();
			//	}
			//	catch (Exception e)
			//	{
			//		dataStore.Rollback();
			//		throw;
			//	}
			//}

			using (var dataStore = new DataStore(""))
			{
				dataStore.Query<Account, Account2>((a, b) => a.Name == b.Name && a.IsDeleted == b.IsDeleted);
			}

			//var queryBuilder = new QueryBuilder<Account>();
			Expression<Func<Account, Object>> expression = a => a.IsDeleted;
		}




		//public class classA
		//{
		//	public string a { get; set; }

		//	public Int32 b { get; set; }
		//}


		//public class ClassB: PropertyMonitor
		//{
		//	public String C { get; set; }

		//	public String D { get; set; }

		//	public void ModelC()
		//	{
		//		OnPropertyChanged(new PropertyArgs(nameof(C), "456"));
		//	}
		//}
	}
}

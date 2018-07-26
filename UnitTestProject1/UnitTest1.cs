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
<<<<<<< HEAD
			var account = new Account();
			var result = account.GetType().GetProperties().Where(w => w.PropertyType.Name != "IList`1" && w.GetCustomAttributes<ValidateBase>().Any());
			var a = 0;
			//using (var dataStore = new DataStore("Server=39.106.106.137;Database=NewCrmContext;Uid=root;Pwd=xiaofan@.1;port=6033;SslMode=none;Pooling=True;Min Pool Size=5;Max Pool Size=10;Treat Tiny As Boolean=false"))
			//{
			//	try
			//	{
			//		dataStore.OpenTransaction();
			//		#region 设置用户下线
			//		{
			//			var account = new Account();
			//			dataStore.ExecuteAdd(account);
			//		}
			//		#endregion
=======
			using (var dataStore = new DataStore("Server=39.106.106.137;Database=NewCrmContext;Uid=root;Pwd=xiaofan@.1;port=6033;SslMode=none;Pooling=True;Min Pool Size=5;Max Pool Size=10;Treat Tiny As Boolean=false"))
			{
				try
				{
					dataStore.OpenTransaction();
					#region 设置用户下线
					{ 
						var account = new Account();
						dataStore.ExecuteAdd(account);
					}
					#endregion
>>>>>>> fb5f71a6703dcb07ca452b71bdf15c40f4d934e8

			//		dataStore.Commit();
			//	}
			//	catch (Exception e)
			//	{
			//		dataStore.Rollback();
			//		throw;
			//	}
			//}
		}
	}
}

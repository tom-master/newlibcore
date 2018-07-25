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


namespace UnitTestProject1
{
	[TestClass]
	public class UnitTest1
	{
		[TestMethod]
		public void TestMethod1()
		{
			using (var dataStore = new DataStore("Server=39.106.106.137;Database=NewCrmContext;Uid=root;Pwd=xiaofan@.1;port=6033;SslMode=none;Pooling=True;Min Pool Size=5;Max Pool Size=10;Treat Tiny As Boolean=false"))
			{
				try
				{
					dataStore.OpenTransaction();
					#region 设置用户下线
					{
						var account = new Account();
						account.ModifyName();
						account.ModifyTime();
						var a1 = "str";
						var rowCount = dataStore.ExecuteModify(account, acc => acc.Id == 4 || acc.Name.Contains(a1) && !acc.IsDeleted && !acc.IsDisable);
						if (rowCount == 0)
						{

						}
					}
					#endregion

					dataStore.Commit();
				}
				catch (Exception e)
				{
					dataStore.Rollback();
					throw;
				}
			}
		}
	}
}

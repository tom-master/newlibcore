using System;
using System.Data.SqlClient;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using NewLib.Security;

namespace Test
{
	[TestClass]
	public class UnitTest1
	{
		[TestMethod]
		public void TestMethod1()
		{
			//RedisConnection 39.106.106.137:6379,allowAdmin=false,password=UGieaY5u9SwRUzoxbuePqw==,connectTimeout=15000,keepAlive=60,syncTimeout=3000
			//RedisPassword UGieaY5u9SwRUzoxbuePqw==


			SqlConnectionStringBuilder conSb = new SqlConnectionStringBuilder();
			conSb.DataSource = @"39.106.106.137";
			conSb.InitialCatalog = "NewCrmContext";
			conSb.Pooling = true;
			conSb.MaxPoolSize = 10;
			conSb.MinPoolSize = 5;
			conSb.IntegratedSecurity = false;
			conSb.UserID = "sa";
			conSb.Password = "xiaofanalahuakeba@.1";
			conSb.PersistSecurityInfo = true;
			var s = conSb.ToString();
			var dataBase = SensitiveDataSafetyProvider.Encrypt(s);

			var decryptRedisResult = SensitiveDataSafetyProvider.Decrypt(@"DfkMZgoYZAdq8NzZaxNIZwos8uRynX7v5nB46WeROYIg+tV/NVRVGgYg8yh8ZwBxkjvURzAFRq/+YMnEGXXH0wVzzLC5nMQxxf3byUBE1/w1tI91z9yo7LAmhkWfYAJQ");

			var redisPassword = SensitiveDataSafetyProvider.Encrypt("39.106.106.137:6379,allowAdmin=false,password=UGieaY5u9SwRUzoxbuePqw==,connectTimeout=15000,keepAlive=60,syncTimeout=3000");
		}

		
	}
}

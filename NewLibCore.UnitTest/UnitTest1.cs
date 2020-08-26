using System;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using NewLibCore.Data.Redis.InternalHelper;
using NewLibCore.Data.SQL;
using NewLibCore.Data.SQL.EMapper;
using NewLibCore.UnitTest.Entitys.Agent;
using NewLibCore.UnitTest.Entitys.System;

namespace NewLibCore.UnitTest
{
    [TestClass]
    public class UnitTest1
    {
        private static readonly Object _obj = new object();
        [TestMethod]
        public void TestMethod1()
        {
            ICacheQueryProvider cacheProvider = new DefaultRedisQueryProvider(0, "39.106.106.137:9736,allowAdmin=false,password=UGieaY5u9SwRUzoxbuePq,connectTimeout=15000,keepAlive=60,syncTimeout=3000");
            var rr = cacheProvider.StringGet<Config>("NewCrm:Config:UserId:4");
            // cacheProvider.StringSet("NewCrm:Config:UserId:4", new Config
            // {
            //     Skin = "wasd123"
            // });
            // EntityMapperConfig.InitDefaultSetting();
            // EntityMapperConfig.ConnectionStringName = "newcrm";

            // //using (var mapper = EntityMapper.CreateMapper())
            // //{
            // //    var result = mapper.Query<User>().Where(w => w.Id == 4).ToList();
            // //    var r = 0;
            // //}
            // try
            // {
            //     Thread[] threads = new Thread[Environment.ProcessorCount];
            //     for (int i = 0; i < Environment.ProcessorCount; i++)
            //     {
            //         threads[i] = new Thread(() =>
            //         {
            //             using (var mapper = EntityMapper.CreateMapper())
            //             {
            //                 var result = mapper.Query<User>().Where(w => w.Id == 4).ToList();
            //                 Console.WriteLine(JsonSerializer.Serialize(result));
            //             }
            //         });
            //     }

            //     foreach (var item in threads)
            //     {
            //         item.Start();
            //     }

            // }
            // catch (Exception)
            // {

            //     throw;
            // }
        }
    }
}

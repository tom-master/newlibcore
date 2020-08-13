using System;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using NewLibCore.Data.SQL;
using NewLibCore.Data.SQL.EMapper;
using NewLibCore.UnitTest.Entitys.Agent;

namespace NewLibCore.UnitTest
{
    [TestClass]
    public class UnitTest1
    {
        private static readonly Object _obj = new object();
        [TestMethod]
        public void TestMethod1()
        {
            EntityMapperConfig.InitDefaultSetting();
            EntityMapperConfig.ConnectionStringName = "newcrm";

            //using (var mapper = EntityMapper.CreateMapper())
            //{
            //    var result = mapper.Query<User>().Where(w => w.Id == 4).ToList();
            //    var r = 0;
            //}
            try
            {
                Thread[] threads = new Thread[Environment.ProcessorCount];
                for (int i = 0; i < Environment.ProcessorCount; i++)
                {
                    threads[i] = new Thread(() =>
                    {
                        using (var mapper = EntityMapper.CreateMapper())
                        {
                            var result = mapper.Query<User>().Where(w => w.Id == 4).ToList();
                            Console.WriteLine(JsonSerializer.Serialize(result));
                        }
                    });
                }

                foreach (var item in threads)
                {
                    item.Start();
                }

            }
            catch (Exception)
            {

                throw;
            }
        }
    }
}

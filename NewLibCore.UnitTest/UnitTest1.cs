using System;
using System.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using NewLibCore.Data.SQL;
using NewLibCore.UnitTest.Entitys.Agent;

namespace NewLibCore.UnitTest
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void TestMethod1()
        {
            #region  
            EntityMapper.InitDefaultSetting();

            //EntityMapper.UseMySql();
            //EntityMapper.UseMsSql();
            EntityMapper.ConnectionStringName = "NewCrmDatabase";

            using (var mapper = EntityMapper.CreateMapper())
            {
                var sw = new Stopwatch();
                sw.Start();

                var a = mapper.Query<User>().Select(s => new { s.Id }).FirstOrDefault();

                sw.Stop();
                Console.WriteLine($@"共花费{Math.Round(sw.Elapsed.TotalSeconds, 4)}秒");
            }

            #endregion
            Console.Read();
        }
    }
}

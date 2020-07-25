using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
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
            var lists = new List<String>
            {
                "1",
                "2",
                "3",
                "4",
                "5",
                "6",
                "7",
                "8",
                "9",
                "10",
                ""
            }.ToList();
            var result = lists.OrderByDescending(d => d);


            #region  
            EntityMapper.InitDefaultSetting();

            //EntityMapper.UseMySql();
            //EntityMapper.UseMsSql();
            EntityMapper.ConnectionStringName = "NewCrmDatabase";

            using (var mapper = EntityMapper.CreateMapper())
            {
                var sw = new Stopwatch();
                sw.Start();

                var a = mapper.Query<User>().ToList();

                sw.Stop();
                Console.WriteLine($@"共花费{Math.Round(sw.Elapsed.TotalSeconds, 4)}秒");
            }

            #endregion
            Console.Read();
        }
    }
}

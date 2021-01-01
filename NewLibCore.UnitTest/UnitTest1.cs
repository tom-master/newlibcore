using System;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using NewLibCore.UnitTest.Entitys.Agent;
using NewLibCore.UnitTest.Entitys.System;

namespace NewLibCore.UnitTest
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void TestMethod1()
        {
            var r = ConfigReader.GetHostVar("sql");
        }
    }

}

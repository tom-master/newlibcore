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
        [TestMethod]
        public void TestMethod1()
        {
            var result1 = EnumExtensions.ToEnum<Test>(1);
            var result2 = EnumExtensions.ToEnum<Test>("B");
        }
    }

    public enum Test
    {
        A = 1,
        B = 2,
        C = 3
    }
}

using System;
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
                for (int i = 0; i < 8; i++)
                {
                    using (var mapper = EntityMapper.CreateMapper())
                    {
                        var result = mapper.Query<User>().Where(w => w.Id == 4).ToList();
                        var r = 0;
                    }
                }

            }
            catch (Exception ex)
            {

                throw;
            }
        }
    }
}

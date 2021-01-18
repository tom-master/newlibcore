using System;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using NewLibCore.Storage.SQL;
using NewLibCore.Storage.SQL.EMapper;
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
            IServiceCollection service = new ServiceCollection();
            service.AddEntityMapper(options =>
            {
                options.UseMySql();
                options.ConnectionStringName = "sql";
            });
            var provider = service.BuildServiceProvider();
            var mapper = provider.GetRequiredService<EntityMapper>();
            var users1 = mapper.Query<User>().ToList();
            var users2 = mapper.Query<User>().ToList();
        }
    }

}

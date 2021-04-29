using Microsoft.Extensions.DependencyInjection;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using NewLibCore.Storage.SQL;
using NewLibCore.Storage.SQL.EMapper;
using NewLibCore.UnitTest.Entitys.Agent;
using NewLibCore.UnitTest.Entitys.System;
using System;

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

            var users1 = mapper.Query<User>()
            .InnerJoin<User, UserRole>((user, role) => user.Id == role.UserId)
            .InnerJoin<User, App>((user, app) => user.Id == app.UserId)
            .Where<User>(user => user.Name != "wasd")
            .ThenByDesc<User, DateTime>(a => a.AddTime)
            .Page(1, 10).Select<UserRole>(role => new { role.RoleId, role.UserId, role.AddTime })
            .Execute();

            var users2 = mapper.Query<User>()
            .InnerJoin<User, UserRole>((user, role) => user.Id == role.UserId)
            .InnerJoin<User, App>((user, app) => user.Id == app.UserId)
            .Where<User>(user => user.Name != "wasd")
            .ThenByDesc<User, DateTime>(a => a.AddTime)
            .Page(1, 10).Select<UserRole>(role => new { role.RoleId, role.UserId, role.AddTime })
            .Execute();

        }
    }
}

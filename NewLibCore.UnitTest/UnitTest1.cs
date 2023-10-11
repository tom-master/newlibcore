using System.Linq;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using NewLibCore.Storage.SQL;
using NewLibCore.Storage.SQL.EMapper;
using NewLibCore.UnitTest.Entitys.Agent;

namespace NewLibCore.UnitTest
{
    [TestClass]
    public class UnitTest1
    {
        private EntityMapper _mapper = InitEntityMapper();

        //[TestMethod]
        //public void Add()
        //{
        //    var user = new User("xiaofan", "xiaofan@.1", default, ValueObject.UserType.Admin);
        //    _mapper.Add(user);
        //}

        [TestMethod]
        public void Query()
        {
            var r = _mapper.Query<User>().InnerJoin<User, UserRole>((u, r) => u.Id == r.UserId)
                .Where(w => w.IsDeleted && w.IsDisable && w.IsOnline && w.IsAdmin)
                .Select(s => new { s.AddTime, s.LoginPassword, s.IsOnline })
                .ToList();
            //var users1 = _mapper.Query<User>()
            //.InnerJoin<User, UserRole>((user, role) => user.Id == role.UserId)
            //.InnerJoin<User, App>((user, app) => user.Id == app.UserId)
            //.Where<User>(user => user.Name != "wasd")
            //.ThenByDesc<User, DateTime>(a => a.AddTime)
            //.Page(1, 10).Select<UserRole>(role => new { role.RoleId, role.UserId, role.AddTime })
            //.Execute();
        }

        //[TestMethod]
        //public void Update()
        //{
        //    var user = new User("xiaofan", "xiaofan@.1", default, ValueObject.UserType.Admin);
        //    _mapper.Update(user, w => w.Id == 1);
        //}


        private static EntityMapper InitEntityMapper()
        {
            IServiceCollection service = new ServiceCollection();
            service.AddEntityMapper(options =>
            {
                options.UseMySql();
                options.SetConnectionString();
                options.SetLogger();
                options.EnableModelValidate();
            });
            var provider = service.BuildServiceProvider();
            return provider.GetRequiredService<EntityMapper>();
        }
    }
}

using System;
using System.Diagnostics;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using NewLibCore.Data.SQL.Mapper;
using NewLibCore.Data.SQL.Mapper.Filter;
using NewLibCore.Data.SQL.Mapper.Validate;
using NewLibCore.UnitTest.Entitys.Agent;
using NewLibCore.UnitTest.Entitys.System;
using NewLibCore.UnitTest.ValueObject;

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
            // for (var i = 0; i < 4; i++)
            // {
            //     var thread = new Thread(new ParameterizedThreadStart((a) =>
            //     {
            //         using (var mapper = EntityMapper.CreateMapper())
            //         {
            //             var result = mapper.Query<User>().FirstOrDefault();
            //             Console.WriteLine(JsonConvert.SerializeObject(result));
            //         }
            //     }));
            //     thread.Start();
            // }

            using (var mapper = EntityMapper.CreateMapper())
            {
                var sw = new Stopwatch();
                sw.Start();

                // var user = new User("xiaofan","xiaofan@.1");
                // mapper.Add(user);

                //var a = mapper.Query<User>().FirstOrDefault();
                var maxKey = 0;
                for (var i = 0; i < 2; i++)
                {
                    var logWhere = FilterFactory.Create<Log>(w => w.LogLevelEnum == LogLevel.Exception);
                    var userWhere = FilterFactory.Create<User>();
                    var filter = logWhere.Append(userWhere);
                    var result = mapper.Query<Log>().LeftJoin<User>((a, b) => a.UserId == b.Id)
                            .Where<User>(filter)
                            .Select(a => new
                            {
                                a.LogLevelEnum,
                                a.Controller,
                                a.Action,
                                a.ExceptionMessage,
                                a.UserId,
                                a.AddTime,
                                a.Id
                            })
                            .Page(i + 1, 9, maxKey)
                            .ThenByDesc<DateTime>(a => a.AddTime)
                            .ToList();
                    maxKey = result.Min(w => w.Id);
                }

                sw.Stop();
                Console.WriteLine($@"共花费{Math.Round(sw.Elapsed.TotalSeconds, 4)}秒");

                //var result = mapper.Query<User>().FirstOrDefault();
                //var result = mapper.Query<User>().ToList();
                //var result = mapper.Query<User>().Select(u => new { u.Id, u.Name, u.LoginPassword }).FirstOrDefault();
                //var result = mapper.Query<User>().Select(u => new { u.Id, u.Name, u.LoginPassword }).ToList();
                //var result = mapper.Query<Config>().InnerJoin<User>((c, u) => c.UserId == u.Id).FirstOrDefault();
                //var result = mapper.Query<Config>().InnerJoin<User>((c, u) => c.UserId == u.Id).ToList();
                //var result = mapper.Query<App>().RightJoin<Member>((a, m) => a.Id == m.AppId).ToList();
            }

            #endregion
            Console.Read();
        }
    }
}

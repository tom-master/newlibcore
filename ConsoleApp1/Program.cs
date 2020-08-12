using System;
using System.Text.Json;
using System.Threading.Tasks;
using NewLibCore.Data.SQL;
using NewLibCore.Data.SQL.EMapper;
using NewLibCore.UnitTest.Entitys.Agent;

namespace ConsoleApp1
{
    class Program
    {
        static void Main(string[] args)
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
                for (int i = 0; i < Environment.ProcessorCount; i++)
                {
                    var r = Task.Run(() =>
                    {
                        using (var mapper = EntityMapper.CreateMapper())
                        {
                            var result = mapper.Query<User>().Where(w => w.Id == 4).ToList();
                            return result;
                        }
                    });
                    Console.WriteLine(JsonSerializer.Serialize(r));
                }
            }
            catch (Exception ex)
            {

                throw;
            }
        }
    }
}

using NewLibCore.Data.SQL.MapperExtension;
using NewLibCore.Data.SQL.PropertyExtension;
using System;
using System.Collections.Generic;

namespace NewLibCore.Run
{
    class Program
    {
        private static readonly IDictionary<String, String> _mapper = new Dictionary<String, String>();

        static void Main(string[] args)
        {
            BuildTestData();

            //var inputValue = "wasd1223";


        }

        private static void BuildTestData()
        {
            _mapper.Add($@"192.168.0.100:8080:wasd1232", "192.168.0.100:8080");
            _mapper.Add($@"192.168.0.100:8082:wasd1213", "192.168.0.100:8082");
            _mapper.Add($@"192.168.0.100:8083:wa2sd1213", "192.168.0.100:8083");
            _mapper.Add($@"192.168.0.100:8081:wasd1223", "192.168.0.100:8081");
            _mapper.Add($@"192.168.0.100:8081:wa2sd1323", "192.168.0.100:8081");
            _mapper.Add($@"192.168.0.100:8080:wasd15423", "192.168.0.100:8080");
            _mapper.Add($@"192.168.0.100:8081:wa5sd123", "192.168.0.100:8081");
            _mapper.Add($@"192.168.0.100:8084:w6asd123", "192.168.0.100:8084");
        }
    }
    public partial class VisitorRecord : DomainModelBase
    {
        public Int32 UserId { get; private set; }

        [PropertyRequired, PropertyInputRange(10), PropertyDefaultValue(typeof(String), "11111")]
        public String UserName { get; private set; }

        public VisitorRecord(Int32 userId, String userName)
        {
            UserId = userId;
            UserName = userName;
        }

        public VisitorRecord() { }
    }
    public abstract class DomainModelBase : PropertyMonitor
    {
        protected DomainModelBase()
        {
            IsDeleted = false;
        }

        public Int32 Id { get; protected set; }

        [PropertyDefaultValue(typeof(Boolean), false)]
        public Boolean IsDeleted { get; protected set; }

        [DateTimeDefaultValue]
        public DateTime AddTime { get; protected set; }

        [DateTimeDefaultValue]
        public DateTime LastModifyTime { get; protected set; }

        public void Remove()
        {
            IsDeleted = true;
            OnPropertyChanged(new PropertyArgs(nameof(IsDeleted), IsDeleted));
        }
    }
}

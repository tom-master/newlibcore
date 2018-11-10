using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using System.Net;
using System.Net.Http;
using System.Net.Security;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading;
using System.Xml;
using NewLibCore.Data.Mapper;
using NewLibCore.Data.Mapper.DomainSpecification.ConcreteSpecification;
using NewLibCore.Data.Mapper.InternalDataStore;
using NewLibCore.Data.Mapper.MapperExtension;
using NewLibCore.Data.Mapper.PropertyExtension;
using Newtonsoft.Json;

namespace NewLibCore.Run
{
    class Program
    {
        static void Main(string[] args)
        {
            using (var dataStore = new DataStore(""))
            {
                dataStore.Add(new VisitorRecord(1, "2222222222222222", "123", "123", "123", "123", "123"));
            }
        }
    }
    public partial class VisitorRecord : DomainModelBase
    {
        public Int32 UserId { get; private set; }

        [PropertyRequired, PropertyInputRange(10), PropertyDefaultValue(typeof(String), "11111")]
        public String UserName { get; private set; }

        [PropertyRequired, PropertyInputRange(25)]
        public String Controller { get; private set; }

        [PropertyRequired, PropertyInputRange(30)]
        public String Action { get; private set; }

        [PropertyRequired]
        public String Ip { get; private set; }

        [PropertyRequired, PropertyInputRange(150)]
        public String VisitorUrl { get; private set; }

        [PropertyRequired, PropertyInputRange(150)]
        public String UrlParameter { get; private set; }

        public VisitorRecord(Int32 userId, String userName, String controller, String action, String ip, String visitorUrl, String urlParameter)
        {
            UserId = userId;
            UserName = userName;
            Controller = controller;
            Action = action;
            Ip = ip;
            VisitorUrl = visitorUrl;
            UrlParameter = urlParameter;
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

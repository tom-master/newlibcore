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
                App app = new App();
                app.ModifyName("xiaofan");
                var ids = new[] { 1, 2, 3, 4 };
                dataStore.Modify(app, a => ids.Contains(a.Id));
            }
        }
    }

    [Serializable, Description("应用")]
    public partial class App : DomainModelBase
    {
        /// <summary>
        /// 名称
        /// </summary>
        [PropertyRequired, PropertyDefaultValue(typeof(String), "wasd"), PropertyInputRange(2, 10)]
        public String Name { get; set; }

        /// <summary>
        /// 宽度
        /// </summary>
        [PropertyRequired]
        public Int32 Width { get; set; }

        /// <summary>
        /// 高度
        /// </summary>
        [PropertyRequired]
        public Int32 Height { get; set; }

        /// <summary>
        /// 使用数
        /// </summary>
        [PropertyDefaultValue(typeof(Int32))]
        public Int32 UseCount { get; set; }

        /// <summary>
        /// 是否显示app底部的按钮
        /// </summary>
        [PropertyDefaultValue(typeof(Boolean))]
        public Boolean IsSetbar { get; private set; }

        public App() { }
    }

    public partial class App
    {
        public void ModifyName(String name)
        {
            Name = "xiaofan";
            OnPropertyChanged(new PropertyArgs(nameof(Name), name));
        }
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

using System;
using System.ComponentModel;
using System.Linq.Expressions;
using NewLibCore.Data.Mapper.DomainSpecification.ConcreteSpecification;
using NewLibCore.Data.Mapper.InternalDataStore;
using NewLibCore.Data.Mapper.MapperExtension;
using NewLibCore.Data.Mapper.PropertyExtension;

namespace NewLibCore.Run
{
    class Program
    {
        static void Main(string[] args)
        {
            var att = new PropertyDefaultValueAttribute(typeof(Int32), "");
        }

    }


}

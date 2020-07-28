using System;
using System.Collections.Generic;
using System.Text;

namespace NewLibCore.Data.SQL.EMapper.Extension.DataConvert
{
    internal class ConverterContext
    {
        internal static IConverter CreateConvert<TResult>()
        {
            var type = typeof(TResult);

            if (type == typeof(ValueTuple<>))
            {
                return new TupleConverter();
            }

            if (!type.IsComplexType())
            {
                return new SimpleTypeConverter();
            }
            else
            {
                return new ClassConverter();
            }
        }
    }
}

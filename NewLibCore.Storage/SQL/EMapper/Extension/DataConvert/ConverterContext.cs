using System;

namespace NewLibCore.Storage.SQL.DataConvert
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
            else if (!type.IsComplexType())
            {
                return new SimpleTypeConverter();
            }

            return new ClassConverter();
        }
    }
}

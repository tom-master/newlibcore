using System;

namespace NewLibCore.Data.SQL.DataConvert
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

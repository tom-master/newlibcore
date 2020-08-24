using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;

namespace NewLibCore.Data.SQL.DataConvert
{
    internal class TupleConverter : IConverter
    {
        public List<TResult> Convert<TResult>(DataTable dt)
        {
            var convertResult = new List<TResult>();
            foreach (DataRow item in dt.Rows)
            {
                var r = CreateValueTuple(item.ItemArray);
                convertResult.Add((TResult)r);
            }
            return convertResult;
        }

        private static Object CreateValueTuple(Object[] rowValues)
        {
            if (rowValues.Length > 8)
            {
                throw new NotSupportedException($@"当已{nameof(ValueTuple)}为返回类型时,{nameof(ValueTuple)}中的项的个数与查询出的列的个数都不能大于8个");
            }

            var parameterTypes = new Type[rowValues.Length];
            for (var i = 0; i < rowValues.Length; i++)
            {
                parameterTypes[i] = rowValues[i].GetType();
            }

            var createMethod = typeof(ValueTuple)
            .GetMethods().Where(m => m.Name == "Create" && m.GetParameters().Length == rowValues.Length).SingleOrDefault();
            var createGenericMethod = createMethod.MakeGenericMethod(parameterTypes);
            var valueTuple = createGenericMethod.Invoke(null, rowValues);
            return valueTuple;
        }
    }
}

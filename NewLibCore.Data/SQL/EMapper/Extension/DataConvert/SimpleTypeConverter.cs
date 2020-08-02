using System;
using System.Collections.Generic;
using System.Data;

namespace NewLibCore.Data.SQL.DataConvert
{
    internal class SimpleTypeConverter : IConverter
    {
        public List<TResult> Convert<TResult>(DataTable dt)
        {
            var convertResults = new List<TResult>();
            var obj = default(TResult);
            if (typeof(TResult) != typeof(String))
            {
                obj = Activator.CreateInstance<TResult>();
            }
            var type = obj == null ? typeof(TResult) : obj.GetType();
            for (var i = 0; i < dt.Rows.Count; i++)
            {
                var r = dt.Rows[i][0];
                convertResults.Add((TResult)dt.Rows[i][0].CastTo(type));
            }
            return convertResults;
        }
    }
}

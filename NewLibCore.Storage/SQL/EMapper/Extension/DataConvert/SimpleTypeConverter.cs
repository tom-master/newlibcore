﻿using System;
using System.Collections.Generic;
using System.Data;

namespace NewLibCore.Storage.SQL.DataConvert
{
    internal class SimpleTypeConverter : IConverter
    {
        public List<TResult> ConvertTo<TResult>(DataTable dt)
        {
            var convertResults = new List<TResult>();
            var obj = default(TResult);
            if (typeof(TResult) != typeof(string))
            {
                obj = Activator.CreateInstance<TResult>();
            }

            _ = obj == null ? typeof(TResult) : obj.GetType();
            for (var i = 0; i < dt.Rows.Count; i++)
            {
                var rowData = dt.Rows[i][0];
                convertResults.Add(rowData.CastTo<TResult>());
            }
            return convertResults;
        }
    }
}

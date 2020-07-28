using System.Collections.Generic;
using System.Data;
using NewLibCore.Data.SQL.EMapper.Extension.DataConvert;

namespace NewLibCore.Data.SQL.Extension
{
    /// <summary>
    /// 将一个DataTable转换为指定的集合
    /// </summary>
    internal static class DataTableExtension
    {
        /// <summary>
        /// 获取列表
        /// </summary>
        /// <typeparam name="TResult">期望的类型</typeparam>
        /// <param name="dataTable">sql执行后的原始结果</param>
        /// <returns></returns>
        internal static List<TResult> ToList<TResult>(this DataTable dataTable)
        {
            if (dataTable == null || dataTable.Rows.Count == 0)
            {
                return new List<TResult>();
            }

            var converter = ConverterContext.CreateConvert<TResult>();
            return converter.Convert<TResult>(dataTable);
        }
    }
}

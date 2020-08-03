using System.Collections.Generic;
using System.Data;

namespace NewLibCore.Data.SQL.DataConvert
{
    internal interface IConverter
    {
        List<TResult> ConvertTo<TResult>(DataTable dt);
    }
}

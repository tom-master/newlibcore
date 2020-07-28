using System;
using System.Collections.Generic;
using System.Data;
using System.Text;

namespace NewLibCore.Data.SQL.EMapper.Extension.DataConvert
{
    internal interface IConverter
    {
        List<TResult> Convert<TResult>(DataTable dt);
    }
}

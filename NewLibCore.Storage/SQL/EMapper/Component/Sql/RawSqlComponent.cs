using System;
using System.Collections.Generic;

namespace NewLibCore.Storage.SQL.Component.Sql
{
    internal class RawSqlComponent
    {
        internal String Sql { get; set; }
        internal IList<MapperParameter> Parameters { get; set; }
    }
}
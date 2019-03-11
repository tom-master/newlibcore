using System;
using System.Linq.Expressions;

namespace NewLibCore.Data.SQL.BuildExtension
{
    internal interface ITranslate
    {
        SqlTemporaryStore Translate(JoinStore joinStore);
    }
}

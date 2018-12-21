using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Text;

namespace NewLibCore.Data.SQL.BuildExtension
{
    internal interface ITranslate
    {
        void Translate(Expression expression);
    }
}

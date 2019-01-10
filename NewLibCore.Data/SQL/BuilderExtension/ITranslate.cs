using System;
using System.Linq.Expressions;

namespace NewLibCore.Data.SQL.BuildExtension
{
    internal interface ITranslate
    {
        void Translate(Expression expression, JoinType joinType = JoinType.None, Boolean alias = false);
    }
}

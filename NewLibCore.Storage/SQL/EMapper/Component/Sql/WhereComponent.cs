using NewLibCore.Validate;
using System.Linq.Expressions;

namespace NewLibCore.Storage.SQL.Component.Sql
{
    internal class WhereComponent : ComponentBase
    {
        internal WhereComponent(Expression expression) 
        {
            Check.IfNullOrZero(expression);
            Expression = expression;
            InitAliasNameMappers(ParseToAliasNames(expression).ToArray());
        }
    }
}
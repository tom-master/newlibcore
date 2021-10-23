using System;
using System.Linq.Expressions;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component
{
    internal class OrderComponent : ComponentBase
    {
        internal OrderByType OrderBy { get; private set; }
        internal void AddOrderBy<TModel, TKey>(Expression<Func<TModel, TKey>> expression, OrderByType orderByType)
                where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(expression);
            Expression = expression;
            OrderBy = orderByType;
        }


        internal (String Fields, String AliasName) ExtractOrderFields()
        {
            var fields = (LambdaExpression)Expression;
            if (fields.Body.NodeType == ExpressionType.MemberAccess)
            {
                var aliasName = fields.Parameters[0].Type.GetEntityBaseAliasName().AliasName;
                var members = (fields.Body as MemberExpression);
                return (members.Member.Name, aliasName);
            }

            throw new Exception();
        }
    }
}
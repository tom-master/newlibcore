using System;
using System.Linq.Expressions;
using NewLibCore.Storage.SQL.Extension;
namespace NewLibCore.Storage.SQL.Component
{
    internal class OrderComponent : RootComponent
    {
        internal OrderByType OrderBy { get; private set; }
        internal void AddOrderType(OrderByType orderByType)
        {
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
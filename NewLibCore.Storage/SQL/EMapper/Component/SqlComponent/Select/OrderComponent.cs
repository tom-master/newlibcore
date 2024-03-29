using System;
using System.Linq;
using System.Linq.Expressions;
using NewLibCore.Storage.SQL.Extension;
namespace NewLibCore.Storage.SQL.Component
{
    internal class OrderComponent: RootComponent
    {
        internal EMType OrderBy { get; private set; }

        internal void AddOrderByType(EMType predicateType)
        {
            OrderBy = predicateType;
        }

        internal (string Fields, string AliasName) ExtractOrderFields(EMType predicateType)
        {
            var orderExpression = PredicateExpressions.Where(w => w.Key == predicateType).FirstOrDefault().Value;

            var fields = (LambdaExpression)orderExpression;
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
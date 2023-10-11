using System.Linq;
using System.Linq.Expressions;
using Microsoft.Extensions.Options;

namespace NewLibCore.Storage.SQL.EMapper.Visitor
{
    internal class FromVisitor: RootVisitor
    {
        internal override int Order => 2;
        public FromVisitor(EMType eMType, Expression expression, IOptions<EntityMapperOptions> options) : base(eMType, expression, options)
        {
        }

        protected override void ParseExpression(LambdaExpression expression)
        {
            var mainTable = ExtractAliasNames(Expression.Value).FirstOrDefault();
            VisitResult = (Expression.Key, Options.Value.TemplateBase.CreateFrom(mainTable.Key, mainTable.Value), null);
        }
    }
}

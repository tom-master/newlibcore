using System.Linq.Expressions;
using Microsoft.Extensions.Options;

namespace NewLibCore.Storage.SQL.EMapper.Visitor
{
    internal class WhereVisitor: RootVisitor
    {
        internal override int Order => 4;
        public WhereVisitor(EMType eMType, Expression expression, IOptions<EntityMapperOptions> options) : base(eMType, expression, options)
        {
        }

        protected override void ParseExpression(LambdaExpression expression)
        {
            var e = new ResolveExpression(Options);
            e.Translate(expression, Expression.Key);
            VisitResult = (Expression.Key, $@"{Expression.Key} {e.TranslationResult}", e.MapperParameters);
        }
    }
}

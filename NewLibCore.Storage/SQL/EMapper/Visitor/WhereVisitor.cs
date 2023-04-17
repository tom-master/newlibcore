using System.Collections.Generic;
using System.Linq.Expressions;
using Microsoft.Extensions.Options;

namespace NewLibCore.Storage.SQL.EMapper.Visitor
{
    internal class WhereVisitor: RootVisitor
    {
        public WhereVisitor(EMType eMType, Expression expression, IOptions<EntityMapperOptions> options) : base(eMType, expression, options)
        {
        }

        protected override void ParseExpression(LambdaExpression expression)
        {
            var e = new ExpressionTranslator(Options);
            e.AliasMapper.AddRange(ExtractAliasNames(Expression.Value));
            e.Translate(expression, Expression.Key);
            VisitResult = (Expression.Key, e.TranslationResult.ToString(), e.MapperParameters);
        }
    }
}

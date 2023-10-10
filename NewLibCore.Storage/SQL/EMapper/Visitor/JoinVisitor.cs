﻿using System.Linq;
using System.Linq.Expressions;
using Microsoft.Extensions.Options;

namespace NewLibCore.Storage.SQL.EMapper.Visitor
{
    internal class JoinVisitor: RootVisitor
    {
        internal override int Order => 3;
        internal JoinVisitor(EMType eMType, Expression expression, IOptions<EntityMapperOptions> options) : base(eMType, expression, options)
        {

        }

        protected override void ParseExpression(LambdaExpression expression)
        {
            var aliasItem = ExtractAliasNames(expression).FirstOrDefault();
            var t = Options.Value.TemplateBase.CreateJoin(Expression.Key, aliasItem.Key, aliasItem.Value.ToLower());
            var e = new ExpressionTranslator(Options);
            e.AliasMapper.AddRange(ExtractAliasNames(Expression.Value));
            e.Translate(expression, Expression.Key);
            VisitResult = (Expression.Key, t.Append(e.TranslationResult).ToString(), e.MapperParameters);
        }
    }
}

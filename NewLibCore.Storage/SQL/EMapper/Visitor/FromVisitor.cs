using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using Microsoft.Extensions.Options;

namespace NewLibCore.Storage.SQL.EMapper.Visitor
{
    internal class FromVisitor: RootVisitor
    {
        public FromVisitor(EMType eMType, Expression expression, IOptions<EntityMapperOptions> options) : base(eMType, expression, options)
        {
        }

        protected override void ParseExpression(LambdaExpression expression)
        {
            var mainTable = GetMainTable();
            VisitResult = new KeyValuePair<EMType, string>(Expression.Key, Options.Value.TemplateBase.CreateFrom(mainTable.Key, mainTable.Value));
        }

        private KeyValuePair<string, string> GetMainTable()
        {
            return ExtractAliasNames(Expression.Value).FirstOrDefault();
        }
    }
}

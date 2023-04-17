using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using Google.Protobuf.WellKnownTypes;
using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.Component;
using NewLibCore.Storage.SQL.Extension;

namespace NewLibCore.Storage.SQL.EMapper.Visitor
{
    internal class SelectVisitor: RootVisitor
    {
        public SelectVisitor(EMType eMType, Expression expression, IOptions<EntityMapperOptions> options) : base(eMType, expression, options)
        {
        }

        protected override void ParseExpression(LambdaExpression expression)
        {
            var anonymousObjFields = new List<string>();

            var fields = expression;
            if (fields.Body.NodeType == ExpressionType.Constant)
            {
                var bodyArguments = (fields.Body as ConstantExpression);
                anonymousObjFields.Add(bodyArguments.Value.ToString());
            }
            else
            {
                var bodyArguments = (fields.Body as NewExpression).Arguments;
                foreach (var item in bodyArguments)
                {
                    var member = (MemberExpression)item;
                    var fieldName = ((ParameterExpression)member.Expression).Type.GetEntityBaseAliasName().AliasName;
                    anonymousObjFields.Add($@"{fieldName}.{member.Member.Name}");
                }
            }
            VisitResult = (Expression.Key, Options.Value.TemplateBase.CreateSelect(string.Join(",", anonymousObjFields)).ToString(), null);
        }
    }
}

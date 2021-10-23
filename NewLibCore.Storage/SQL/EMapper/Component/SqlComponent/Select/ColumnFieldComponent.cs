using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component
{

    internal class ColumnFieldComponent : ComponentBase
    {
        internal void AddColumnField(Expression selector)
        {
            Check.IfNullOrZero(selector);
            Expression = selector;
        }

        internal String ExtractSelectFields()
        {
            var anonymousObjFields = new List<String>();

            var fields = (LambdaExpression)Expression;
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


            return String.Join(",", anonymousObjFields);
        }
    }
}
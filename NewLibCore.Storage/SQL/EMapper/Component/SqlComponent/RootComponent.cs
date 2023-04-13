
using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using NewLibCore.Storage.SQL.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component
{
    internal class RootComponent
    {
        protected internal IList<KeyValuePair<PredicateType, Expression>> PredicateExpressions { get; } = new List<KeyValuePair<PredicateType, Expression>>();

        internal void AddExpression(Expression expression, PredicateType predicateType)
        {
            Check.IfNullOrZero(expression);
            PredicateExpressions.Add(new KeyValuePair<PredicateType, Expression>(predicateType, expression));
        }

        internal KeyValuePair<string, string> GetMainTable()
        {
            var fromExpression = PredicateExpressions.Where(w => w.Key == PredicateType.FROM).FirstOrDefault();
            return ExtractAliasNames(fromExpression.Value).FirstOrDefault();
        }

        public List<KeyValuePair<string, string>> ExtractAliasNames(Expression expression)
        {
            var parameters = ((LambdaExpression)expression).Parameters;
            var result = new List<KeyValuePair<string, string>>();
            foreach (var item in parameters)
            {
                var (tableName, aliasName) = item.Type.GetEntityBaseAliasName();
                result.Add(new KeyValuePair<string, string>(tableName, aliasName));
            }
            return result.Distinct().ToList();
        }

        internal IList<KeyValuePair<String, String>> MergeAllComponentAlias()
        {
            var newAliasMapper = new List<KeyValuePair<String, String>>();

            foreach (var predicateExpression in PredicateExpressions)
            {
                newAliasMapper.AddRange(ExtractAliasNames(predicateExpression.Value));
            }
            newAliasMapper = newAliasMapper.Select(s => s).Distinct().ToList();
            var sameGroup = newAliasMapper.GroupBy(a => a.Value);
            if (sameGroup.Any(w => w.Count() > 1))
            {
                throw new InvalidOperationException("DuplicateTableAliasName");
            }
            return newAliasMapper;
        }

        internal String ExtractSelectFields()
        {
            var anonymousObjFields = new List<String>();

            var columnExpression = PredicateExpressions.Where(w => w.Key == PredicateType.COLUMN).FirstOrDefault().Value;

            var fields = (LambdaExpression)columnExpression;
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
using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Store;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Handler
{
    /// <summary>
    /// 查询处理类
    /// </summary>
    internal class QueryHandler : HandlerBase
    {
        internal readonly ExpressionStore _expressionStore;

        internal QueryHandler(ExpressionStore expressionStore, IServiceProvider serviceProvider) : base(serviceProvider)
        {
            Parameter.Validate(expressionStore);
            _expressionStore = expressionStore;
        }

        /// <summary>
        /// 执行查询操作的翻译
        /// </summary>
        /// <returns></returns>
        protected override ExecuteResult Execute()
        {
            var mainTable = _expressionStore.From.AliaNameMapper[0];
            var parserResult = ParserResult.CreateResult();
            parserResult.Append(String.Format(TemplateBase.SelectTemplate, ParseSelect(), mainTable.Key, mainTable.Value));

            var (sql, parameters) = Parser.CreateParser(ServiceProvider).ExecuteParse(_expressionStore);
            parserResult.Append(sql, parameters);

            var aliasMapper = _expressionStore.MergeAliasMapper();
            foreach (var aliasItem in aliasMapper)
            {
                parserResult.Append($@"{PredicateType.AND} {aliasItem.Value.ToLower()}.IsDeleted = 0");
            }

            if (_expressionStore.Pagination != null)
            {
                if (_expressionStore.Order == null)
                {
                    throw new Exception("分页中没有指定排序字段");
                }
                var (fields, tableName) = ParseOrder();
                var orderTemplate = TemplateBase.CreateOrderBy(_expressionStore.Order.OrderBy, $@"{tableName}.{fields}");
                parserResult = TemplateBase.CreatePagination(_expressionStore.Pagination.Index, _expressionStore.Pagination.Size, orderTemplate, parserResult);
            }
            else if (_expressionStore.Order != null)
            {
                var (fields, tableName) = ParseOrder();
                var orderTemplate = TemplateBase.CreateOrderBy(_expressionStore.Order.OrderBy, $@"{tableName}.{fields}");
                parserResult.Append(orderTemplate);
            }

            return parserResult.Execute(ServiceProvider);
        }

        private (String Fields, String AliasName) ParseOrder()
        {
            var modelAliasName = new List<String>();
            var fields = (LambdaExpression)_expressionStore.Order.Expression;
            var aliasName = fields.Parameters[0].Type.GetTableName().AliasName;

            if (fields.Body.NodeType == ExpressionType.MemberAccess)
            {
                var members = (fields.Body as MemberExpression);
                return (members.Member.Name, aliasName);
            }
            return ("", "");
        }

        private String ParseSelect()
        {
            if (_expressionStore.Select != null)
            {
                var fields = (LambdaExpression)_expressionStore.Select.Expression;

                var modelAliasNames = fields.Parameters.Select(s => s.Type.GetTableName().AliasName);
                var anonymousObjFields = new List<String>();
                var bodyArguments = (fields.Body as NewExpression).Arguments;
                foreach (var item in bodyArguments)
                {
                    var member = (MemberExpression)item;
                    var fieldName = ((ParameterExpression)member.Expression).Type.GetTableName().AliasName;
                    anonymousObjFields.Add($@"{fieldName}.{member.Member.Name}");
                }
                return String.Join(",", anonymousObjFields);
            }

            var types = _expressionStore.MergeParameterTypes();
            var propertys = types.SelectMany(s => s.GetProperties(BindingFlags.Instance | BindingFlags.Public).Where(w => w.GetCustomAttributes<PropertyValidate>().Any()).Select(s1 => $@"{s.GetTableName().AliasName}.{s1.Name}")).ToList();
            return String.Join(",", propertys);
        }
    }
}

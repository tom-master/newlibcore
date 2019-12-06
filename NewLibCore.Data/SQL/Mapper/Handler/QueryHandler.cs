using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Store;
using NewLibCore.Data.SQL.Mapper.Validate;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Handler
{
    /// <summary>
    /// 查询处理类
    /// </summary>
    internal class QueryHandler : HandlerBase
    {
        private readonly ExpressionStore _expressionStore;

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
            ParserResult.Append(String.Format(TemplateBase.SelectTemplate, ParseSelect(), mainTable.Key, mainTable.Value));
        
            var (sql, parameters) = Parser.ExecuteParse(_expressionStore);
            ParserResult.Append(sql, parameters);

            var aliasMapper = _expressionStore.MergeAliasMapper();
            foreach (var aliasItem in aliasMapper)
            {
                ParserResult.Append($@"{PredicateType.AND} {aliasItem.Value.ToLower()}.IsDeleted = 0");
            }

            if (_expressionStore.Pagination != null)
            {
                if (_expressionStore.Order == null)
                {
                    throw new Exception("分页中没有指定排序字段");
                }
                var (fields, tableName) = ParseOrder();
                var orderTemplate = TemplateBase.CreateOrderBy(_expressionStore.Order.OrderBy, $@"{tableName}.{fields}");

                var newSql = TemplateBase.CreatePagination(_expressionStore.Pagination.Index, _expressionStore.Pagination.Size, orderTemplate, ParserResult.ToString());
                ParserResult.ClearSql();
                ParserResult.Append(newSql);
            }
            else if (_expressionStore.Order != null)
            {
                var (fields, tableName) = ParseOrder();
                var orderTemplate = TemplateBase.CreateOrderBy(_expressionStore.Order.OrderBy, $@"{tableName}.{fields}");
                ParserResult.Append(orderTemplate);
            }

            return ParserResult.Execute(ServiceProvider);
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
            throw new Exception("不支持的ORDER BY 表达式");
        }

        private String ParseSelect()
        {
            if (_expressionStore.Select != null)
            {
                var fields = (LambdaExpression)_expressionStore.Select.Expression;

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

            var tableNames = types.Select(s => new KeyValuePair<String, String>(s.Name, s.GetTableName().AliasName)).ToList();
            var propertys = types.SelectMany(s => s.GetProperties(BindingFlags.Instance | BindingFlags.Public)
            .Where(w => w.GetCustomAttributes<PropertyValidate>().Any())
            .Select(s1 => $@"{tableNames.FirstOrDefault(w => w.Key == s.Name).Value}.{s1.Name}"));
            return String.Join(",", propertys);
        }
    }
}

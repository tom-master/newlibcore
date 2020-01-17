using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Store;
using NewLibCore.Data.SQL.Mapper.Template;
using NewLibCore.Data.SQL.Mapper.Validate;

namespace NewLibCore.Data.SQL.Mapper.Handler
{
    /// <summary>
    /// 查询处理类
    /// </summary>
    internal class QueryHandler : HandlerBase
    {

        public QueryHandler(TemplateBase templateBase, ParserExecutor parserExecutor) : base(templateBase, parserExecutor)
        {
        }

        /// <summary>
        /// 执行查询操作的翻译
        /// </summary>
        /// <returns></returns>
        protected override ExecuteResult Execute(ExpressionStore store)
        {
            var mainTable = store.From.AliaNameMapper[0];

            var result = _parserExecutor.Parse(new ParseModel
            {
                Sql = _templateBase.CreateSelect(ParseSelect(store), mainTable.Key, mainTable.Value),
                ExpressionStore = store
            });

            var aliasMapper = store.MergeAliasMapper();
            foreach (var aliasItem in aliasMapper)
            {
                result.Append($@"{PredicateType.AND} {aliasItem.Value.ToLower()}.IsDeleted = 0");
            }

            if (store.Pagination != null)
            {
                if (store.Order == null)
                {
                    throw new Exception("分页中没有指定排序字段");
                }
                var (fields, tableName) = ParseOrder(store);
                var orderTemplate = _templateBase.CreateOrderBy(store.Order.OrderBy, $@"{tableName}.{fields}");

                var newSql = _templateBase.CreatePagination(store.Pagination, orderTemplate, result.ToString());
                result.ClearSql();
                result.Append(newSql);
            }
            else if (store.Order != null)
            {
                var (fields, tableName) = ParseOrder(store);
                var orderTemplate = _templateBase.CreateOrderBy(store.Order.OrderBy, $@"{tableName}.{fields}");
                result.Append(orderTemplate);
            }

            return result.Execute();
        }

        /// <summary>
        /// 解析出排序字段
        /// </summary>
        /// <returns></returns>
        private (String Fields, String AliasName) ParseOrder(ExpressionStore store)
        {
            var modelAliasName = new List<String>();
            var fields = (LambdaExpression)store.Order.Expression;
            var aliasName = fields.Parameters[0].Type.GetTableName().AliasName;

            if (fields.Body.NodeType == ExpressionType.MemberAccess)
            {
                var members = (fields.Body as MemberExpression);
                return (members.Member.Name, aliasName);
            }
            throw new Exception("不支持的ORDER BY 表达式");
        }

        /// <summary>
        /// 解析出Select字段
        /// </summary>
        /// <returns></returns>
        private String ParseSelect(ExpressionStore store)
        {
            if (store.Select != null)
            {
                var fields = (LambdaExpression)store.Select.Expression;

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

            var types = store.MergeParameterTypes();

            var tableNames = types.Select(s => new KeyValuePair<String, String>(s.Name, s.GetTableName().AliasName)).ToList();
            var propertys = types.SelectMany(s => s.GetProperties(BindingFlags.Instance | BindingFlags.Public)
            .Where(w => w.GetCustomAttributes<PropertyValidate>().Any())
            .Select(s1 => $@"{tableNames.FirstOrDefault(w => w.Key == s.Name).Value}.{s1.Name}")).Distinct();
            return String.Join(",", propertys);
        }
    }
}

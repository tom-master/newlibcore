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
        private readonly ExpressionStore _store;

        internal QueryHandler(ExpressionStore store, IServiceProvider serviceProvider) : base(serviceProvider)
        {
            Parameter.Validate(store);
            _store = store;
        }

        /// <summary>
        /// 执行查询操作的翻译
        /// </summary>
        /// <returns></returns>
        protected override ExecuteResult Execute()
        {
            var mainTable = _store.From.AliaNameMapper[0];
            ResultExecutor.AppendResult(String.Format(Template.Select, ParseSelect(), mainTable.Key, mainTable.Value));

            var (sql, parameters) = Parser.Execute(_store);
            ResultExecutor.AppendResult(sql, parameters);

            var aliasMapper = _store.MergeAliasMapper();
            foreach (var aliasItem in aliasMapper)
            {
                ResultExecutor.AppendResult($@"{PredicateType.AND} {aliasItem.Value.ToLower()}.IsDeleted = 0");
            }

            if (_store.Pagination != null)
            {
                if (_store.Order == null)
                {
                    throw new Exception("分页中没有指定排序字段");
                }
                var (fields, tableName) = ParseOrder();
                var orderTemplate = Template.CreateOrderBy(_store.Order.OrderBy, $@"{tableName}.{fields}");

                var newSql = Template.CreatePagination(_store.Pagination.Index, _store.Pagination.Size, orderTemplate, ResultExecutor.ToString());
                ResultExecutor.ClearSql();
                ResultExecutor.AppendResult(newSql);
            }
            else if (_store.Order != null)
            {
                var (fields, tableName) = ParseOrder();
                var orderTemplate = Template.CreateOrderBy(_store.Order.OrderBy, $@"{tableName}.{fields}");
                ResultExecutor.AppendResult(orderTemplate);
            }

            return ResultExecutor.Execute();
        }

        /// <summary>
        /// 解析出排序字段
        /// </summary>
        /// <returns></returns>
        private (String Fields, String AliasName) ParseOrder()
        {
            var modelAliasName = new List<String>();
            var fields = (LambdaExpression)_store.Order.Expression;
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
        private String ParseSelect()
        {
            if (_store.Select != null)
            {
                var fields = (LambdaExpression)_store.Select.Expression;

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

            var types = _store.MergeParameterTypes();

            var tableNames = types.Select(s => new KeyValuePair<String, String>(s.Name, s.GetTableName().AliasName)).ToList();
            var propertys = types.SelectMany(s => s.GetProperties(BindingFlags.Instance | BindingFlags.Public)
            .Where(w => w.GetCustomAttributes<PropertyValidate>().Any())
            .Select(s1 => $@"{tableNames.FirstOrDefault(w => w.Key == s.Name).Value}.{s1.Name}"));
            return String.Join(",", propertys);
        }
    }
}

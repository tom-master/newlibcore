using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 查询处理类
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal class QueryHandler<TModel> : Handler where TModel : new()
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
        internal override RawResult Execute()
        {
            var mainTable = _expressionStore.From.AliaNameMapper[0];

            var parserResult = ParserResult.CreateResult();
            var (Fields, _) = StatementParse(_expressionStore.Select);
            parserResult.Append(String.Format(TemplateBase.SelectTemplate, Fields, mainTable.Key, mainTable.Value));

            var (sql, parameters) = Parser.CreateParser(ServiceProvider).ExecuteParser(_expressionStore);
            parserResult.Append(sql, parameters);

            //当出现查询但张表不加Where条件时，则强制将IsDeleted=0添加到后面
            if (_expressionStore.Where == null)
            {
                parserResult.Append($@"{PredicateType.AND.ToString()} {mainTable.Value}.IsDeleted = 0");
            }
            else
            {
                var aliasMapper = _expressionStore.MergeAliasMapper();
                foreach (var aliasItem in aliasMapper)
                {
                    parserResult.Append($@"{PredicateType.AND} {aliasItem.Value.ToLower()}.IsDeleted = 0");
                }
            }
            if (_expressionStore.Order != null)
            {
                var (fields, tableName) = StatementParse(_expressionStore.Order);
                var orderTemplate = TemplateBase.CreateOrderBy(_expressionStore.Order.OrderBy, $@"{tableName}.{fields}");
                parserResult.Append(orderTemplate);
            }

            if (_expressionStore.Pagination != null)
            {
                parserResult.Append(TemplateBase.Page(_expressionStore.Pagination.Index, _expressionStore.Pagination.Size, parserResult));
            }

            return parserResult.Execute(ServiceProvider);
        }

        /// <summary>
        ///判断表达式语句类型并转换为相应的sql
        /// </summary>
        /// <param name="expressionMapper">表达式分解后的对象</param>
        /// <returns></returns>
        private static (String Fields, String AliasName) StatementParse(ExpressionMapperBase expressionMapperBase)
        {
            var modelAliasName = new List<String>();
            if (expressionMapperBase == null) //如果表达式语句为空则表示需要翻译为SELECT a.xxx,a.xxx,a.xxx 类型的语句
            {
                var modelType = typeof(TModel);
                var f = new List<String>();
                {
                    var aliasName = modelType.GetTableName().AliasName;
                    modelAliasName.Add(aliasName);
                    var mainModelPropertys = modelType.GetProperties().Where(w => w.GetCustomAttributes<PropertyValidate>().Any()).ToList();
                    foreach (var item in mainModelPropertys)
                    {
                        f.Add($@"{aliasName}.{item.Name}");
                    }
                }
                return (String.Join(",", f), modelAliasName.FirstOrDefault());
            }

            var fields = (LambdaExpression)expressionMapperBase.Expression;
            foreach (var item in fields.Parameters)
            {
                modelAliasName.Add(item.Type.GetTableName().AliasName);
            }

            if (fields.Body.NodeType == ExpressionType.Constant)
            {
                var constant = (ConstantExpression)fields.Body;
                return (constant.Value + "", modelAliasName.FirstOrDefault());
            }

            if (fields.Body.NodeType == ExpressionType.MemberAccess)
            {
                var members = (fields.Body as MemberExpression);
                return (members.Member.Name, modelAliasName.FirstOrDefault());
            }

            var anonymousObjFields = new List<String>();
            var bodyArguments = (fields.Body as NewExpression).Arguments;
            foreach (var item in bodyArguments)
            {
                var member = (MemberExpression)item;
                var fieldName = ((ParameterExpression)member.Expression).Type.GetTableName().AliasName;
                anonymousObjFields.Add($@"{fieldName}.{member.Member.Name}");
            }
            return (String.Join(",", anonymousObjFields), modelAliasName.FirstOrDefault());
        }
    }
}

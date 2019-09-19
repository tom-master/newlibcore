using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using NewLibCore.Data.SQL.Mapper.Database;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 查询处理类
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal class QueryHandler<TModel> : Handler where TModel : new()
    {
        private readonly StatementStore _statementStore;

        internal QueryHandler(StatementStore statementStore, IMapperDbContext mapperDbContext) : base(mapperDbContext)
        {
            Parameter.Validate(statementStore);
            _statementStore = statementStore;
        }

        internal override RawResult Execute()
        {
            var (Fields, AliasName) = StatementParse(_statementStore.Select);

            var segment = TranslationContext.CreateTranslation(_statementStore);
            var mainTable = _statementStore.From.AliaNameMapper[0];
            segment.Result.Append(String.Format(Instance.SelectTemplate, Fields, mainTable.Key, mainTable.Value));
            segment.Translate();

            var aliasMapper = _statementStore.MergeAliasMapper();

            //当出现查询但张表不加Where条件时，则强制将IsDeleted=0添加到后面
            if (_statementStore.Where == null)
            {
                segment.Result.Append($@"{RelationType.AND.ToString()} {mainTable.Value}.IsDeleted = 0");
            }
            else
            {
                foreach (var aliasItem in aliasMapper)
                {
                    segment.Result.Append($@"{RelationType.AND} {aliasItem.Value.ToLower()}.IsDeleted = 0");
                }
            }
            if (_statementStore.Order != null)
            {
                var (fields, tableName) = StatementParse(_statementStore.Order);
                var orderTemplate = Instance.OrderByBuilder(_statementStore.Order.OrderBy, $@"{tableName}.{fields}");
                segment.Result.Append(orderTemplate);
            }

            if (_statementStore.Pagination != null)
            {
                var pageIndex = (_statementStore.Pagination.Size * (_statementStore.Pagination.Index - 1)).ToString();
                var pageSize = _statementStore.Pagination.Size.ToString();
                segment.Result.Append(Instance.Extension.Page.Replace("{value}", pageIndex).Replace("{pageSize}", pageSize));
            }

            return segment.Result.Execute(MapperDbContext);
        }

        /// <summary>
        ///判断表达式语句类型并转换为相应的sql
        /// </summary>
        /// <param name="expressionMapper">表达式分解后的对象</param>
        /// <returns></returns>
        private static (String Fields, String AliasName) StatementParse(Statement statement)
        {
            var modelAliasName = new List<String>();
            if (statement == null) //如果表达式语句为空则表示需要翻译为SELECT a.xxx,a.xxx,a.xxx 类型的语句
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

            var fields = (LambdaExpression)statement.Expression;
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

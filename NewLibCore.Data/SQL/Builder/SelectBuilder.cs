using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using NewLibCore.Data.SQL.Mapper.AttributeExtension;
using NewLibCore.Data.SQL.Mapper.AttributeExtension.Association;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Data.SQL.Mapper.Translation;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Builder
{
    /// <summary>
    /// 查询操作构建
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal class SelectBuilder<TModel> : IBuilder<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly ExpressionSegment _expressionSegment;

        internal SelectBuilder(ExpressionSegment expressionSegment)
        {
            Parameter.Validate(expressionSegment);
            _expressionSegment = expressionSegment;
        }

        /// <summary>
        /// 构建一个查询操作的翻译结果
        /// </summary>
        /// <returns></returns>
        public TranslateResult CreateTranslateResult()
        {
            var translation = new TranslateExpression(_expressionSegment);
            {
                var statement = StatementParse(_expressionSegment.Field);
                translation.Result.Append($@"SELECT {statement.fields} FROM {typeof(TModel).GetAliasName()} AS {statement.tableName}");
                translation.Translate();

                var aliasMapper = _expressionSegment.MergeAliasMapper();
                foreach (var aliasItem in aliasMapper)
                {
                    translation.Result.Append($@"AND {aliasItem.Value.ToLower()}.IsDeleted = 0");
                }
            }

            if (_expressionSegment.Order != null)
            {
                var order = StatementParse(_expressionSegment.Order);
                var orderTemplate = MapperConfig.GetInstance().DatabaseInstance.OrderByBuilder(_expressionSegment.Order.OrderBy, $@"{order.tableName}.{order.fields}");
                translation.Result.Append(orderTemplate);
            }

            if (_expressionSegment.Page != null)
            {
                var pageIndex = (_expressionSegment.Page.Size * (_expressionSegment.Page.Index - 1)).ToString();
                var pageSize = _expressionSegment.Page.Size.ToString();
                translation.Result.Append(MapperConfig.GetInstance().DatabaseInstance.Extension.Page.Replace("{value}", pageIndex).Replace("{pageSize}", pageSize));
            }

            return translation.Result;
        }

        /// <summary>
        ///判断表达式语句类型并转换为相应的sql
        /// </summary>
        /// <param name="statement"></param>
        /// <returns></returns>
        private (String fields, String tableName) StatementParse(Statement statement)
        {
            var modelAliasName = new List<String>();
            if (statement == null) //如果表达式语句为空则表示需要翻译为SELECT a.xxx,a.xxx,a.xxx 类型的语句
            {
                var modelType = typeof(TModel);
                var f = new List<String>();
                {
                    var typeName = modelType.GetAliasName().ToLower();
                    modelAliasName.Add(typeName);
                    var mainModelPropertys = modelType.GetProperties().Where(w => w.GetCustomAttributes<PropertyValidate>().Any()).ToList();
                    foreach (var item in mainModelPropertys)
                    {
                        f.Add($@"{typeName}.{item.Name} AS {typeName}_{item.Name}");
                    }
                }

                {
                    var subProperty = modelType.GetProperties().Where(w => w.GetCustomAttribute<SubModelAttribute>() != null);
                    if (subProperty != null)
                    {
                        var propertyType = subProperty.FirstOrDefault().PropertyType;
                        var typeName = propertyType.GetAliasName().ToLower();
                        var subModelPropertys = propertyType.GetProperties().Where(w => w.GetCustomAttributes<PropertyValidate>().Any()).ToList();
                        foreach (var item in subModelPropertys)
                        {
                            f.Add($@"{typeName}.{item.Name} AS {typeName}_{item.Name}");
                        }
                    }
                }

                return (String.Join(",", f), modelAliasName.FirstOrDefault());
            }

            var fields = (LambdaExpression)statement.Expression;
            foreach (var item in fields.Parameters)
            {
                modelAliasName.Add(item.Type.GetAliasName().ToLower());
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

            var dynamicFields = new List<String>();
            var bodyArguments = (fields.Body as NewExpression).Arguments;
            foreach (var item in bodyArguments)
            {
                var member = (MemberExpression)item;
                if (member.Type.IsComplexType() && member.Member.GetAttribute<SubModelAttribute>(true) != null)
                {
                    var propertys = member.Type.GetProperties().Where(w => w.GetCustomAttributes<PropertyValidate>().Any());
                    dynamicFields.AddRange(propertys.Select(s => $@"{member.Type.GetAliasName().ToLower()}.{s.Name} "));
                }
                else
                {
                    var fieldName = ((ParameterExpression)member.Expression).Type.GetAliasName().ToLower();
                    dynamicFields.Add($@"{fieldName}.{member.Member.Name}");
                }
            }

            return (String.Join(",", dynamicFields), modelAliasName.FirstOrDefault());
        }
    }
}

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
    internal class SelectBuilder<TModel> : Builder<TModel> where TModel : PropertyMonitor, new()
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
        internal override TranslateResult CreateTranslateResult()
        {
            var translation = new TranslateExpression(_expressionSegment);
            {
                var (fields, tableName) = StatementParse(_expressionSegment.Field);
                translation.Result.Append($@"SELECT {fields} FROM {typeof(TModel).GetTableName().TableName} AS {tableName}");
                translation.Translate();

                var aliasMapper = _expressionSegment.MergeAliasMapper();
                foreach (var aliasItem in aliasMapper)
                {
                    translation.Result.Append($@"AND {aliasItem.Value.ToLower()}.IsDeleted = 0");
                }
            }

            if (_expressionSegment.Order != null)
            {
                var (fields, tableName) = StatementParse(_expressionSegment.Order);
                var orderTemplate = MapperConfig.DatabaseConfig.OrderByBuilder(_expressionSegment.Order.OrderBy, $@"{tableName}.{fields}");
                translation.Result.Append(orderTemplate);
            }

            if (_expressionSegment.Page != null)
            {
                var pageIndex = (_expressionSegment.Page.Size * (_expressionSegment.Page.Index - 1)).ToString();
                var pageSize = _expressionSegment.Page.Size.ToString();
                translation.Result.Append(MapperConfig.DatabaseConfig.Extension.Page.Replace("{value}", pageIndex).Replace("{pageSize}", pageSize));
            }

            return translation.Result;
        }

        /// <summary>
        ///判断表达式语句类型并转换为相应的sql
        /// </summary>
        /// <param name="statement"></param>
        /// <returns></returns>
        internal override (String fields, String tableName) StatementParse(Statement statement)
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
                        //f.Add($@"{typeName}.{item.Name} AS {typeName}_{item.Name}");
                        f.Add($@"{aliasName}.{item.Name}");
                    }
                }

                {
                    //var subProperty = modelType.GetProperties().Where(w => w.GetCustomAttribute<SubModelAttribute>() != null);
                    //if (subProperty != null && subProperty.Any())
                    //{
                    //    var propertyType = subProperty.FirstOrDefault().PropertyType;
                    //    var typeName = propertyType.GetTableName().ToLower();
                    //    var subModelPropertys = propertyType.GetProperties().Where(w => w.GetCustomAttributes<PropertyValidate>().Any()).ToList();
                    //    foreach (var item in subModelPropertys)
                    //    {
                    //        f.Add($@"{typeName}.{item.Name} AS {typeName}_{item.Name}");
                    //    }
                    //}
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
                if (member.Type.IsComplexType() && member.Member.GetAttribute<SubModelAttribute>(true) != null)
                {
                    var propertys = member.Type.GetProperties().Where(w => w.GetCustomAttributes<PropertyValidate>().Any());
                    anonymousObjFields.AddRange(propertys.Select(s => $@"{member.Type.GetTableName().AliasName}.{s.Name} "));
                }
                else
                {
                    var fieldName = ((ParameterExpression)member.Expression).Type.GetTableName().AliasName;
                    anonymousObjFields.Add($@"{fieldName}.{member.Member.Name}");
                }
            }
            return (String.Join(",", anonymousObjFields), modelAliasName.FirstOrDefault());
        }
    }
}

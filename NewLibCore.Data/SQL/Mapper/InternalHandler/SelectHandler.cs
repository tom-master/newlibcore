using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 查询操作构建
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal class SelectHandler<TModel> : Handler<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly SegmentManager _segmentManager;

        internal SelectHandler(SegmentManager segmentManager)
        {
            Parameter.Validate(segmentManager);
            _segmentManager = segmentManager;
        }

        protected override RawExecuteResult ExecuteTranslate()
        {
            var (Fields, AliasName) = StatementParse(_segmentManager.Field);

            var translationSegment = TranslationSegment.CreateTranslation(_segmentManager);
            translationSegment.AppendSqlResult(String.Format(MapperConfig.Instance.SelectTemplate, Fields, typeof(TModel).GetTableName().TableName, AliasName));
            translationSegment.Translate();

            var aliasMapper = _segmentManager.MergeAliasMapper();
            foreach (var aliasItem in aliasMapper)
            {
                translationSegment.AppendSqlResult($@"AND {aliasItem.Value.ToLower()}.IsDeleted = 0");
            }

            if (_segmentManager.Order != null)
            {
                var (fields, tableName) = StatementParse(_segmentManager.Order);
                var orderTemplate = MapperConfig.Instance.OrderByBuilder(_segmentManager.Order.OrderBy, $@"{tableName}.{fields}");
                translationSegment.AppendSqlResult(orderTemplate);
            }

            if (_segmentManager.Page != null)
            {
                var pageIndex = (_segmentManager.Page.Size * (_segmentManager.Page.Index - 1)).ToString();
                var pageSize = _segmentManager.Page.Size.ToString();
                translationSegment.AppendSqlResult(MapperConfig.Instance.Extension.Page.Replace("{value}", pageIndex).Replace("{pageSize}", pageSize));
            }

            return translationSegment.Execute();
        }

        /// <summary>
        ///判断表达式语句类型并转换为相应的sql
        /// </summary>
        /// <param name="statement"></param>
        /// <returns></returns>
        protected override (String Fields, String AliasName) StatementParse(Statement statement)
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
                //if (member.Type.IsComplexType() && member.Member.GetAttribute<SubModelAttribute>(true) != null)
                //{
                //    var propertys = member.Type.GetProperties().Where(w => w.GetCustomAttributes<PropertyValidate>().Any());
                //    anonymousObjFields.AddRange(propertys.Select(s => $@"{member.Type.GetTableName().AliasName}.{s.Name} "));
                //}
                //else
                {
                    var fieldName = ((ParameterExpression)member.Expression).Type.GetTableName().AliasName;
                    anonymousObjFields.Add($@"{fieldName}.{member.Member.Name}");
                }
            }
            return (String.Join(",", anonymousObjFields), modelAliasName.FirstOrDefault());
        }
    }
}

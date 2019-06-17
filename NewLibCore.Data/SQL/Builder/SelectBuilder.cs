﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Extension.AssociationMapperExtension;
using NewLibCore.Data.SQL.Mapper.Extension.PropertyExtension;
using NewLibCore.Data.SQL.Mapper.Translation;
using NewLibCore.InternalExtension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Builder
{
    internal class SelectBuilder<TModel> : IBuilder<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly StatementStore _statementStore;

        internal SelectBuilder(StatementStore statementStore)
        {
            Parameter.Validate(statementStore);
            _statementStore = statementStore;
        }

        public TranslationCoreResult Build()
        {
            var statement = StatementParse(_statementStore.Field);

            var translation = new TranslationCore(_statementStore);
            translation.Result.Append($@"SELECT {statement.fields} FROM {typeof(TModel).GetAliasName()} AS {statement.tableName}");
            translation.Translate();

            var aliasMapper = _statementStore.MergeAliasMapper();
            foreach (var aliasItem in aliasMapper)
            {
                translation.Result.Append($@"AND {aliasItem.Value.ToLower()}.IsDeleted = 0");
            }

            if (_statementStore.Order != null)
            {
                var order = StatementParse(_statementStore.Order);
                var orderTemplate = MapperFactory.Instance.OrderByBuilder(_statementStore.Order.OrderBy, $@"{order.tableName}.{order.fields}");
                translation.Result.Append(orderTemplate);
            }

            if (_statementStore.Page != null)
            {
                var pageIndex = (_statementStore.Page.Size * (_statementStore.Page.Index - 1)).ToString();
                var pageSize = _statementStore.Page.Size.ToString();
                translation.Result.Append(MapperFactory.Instance.Extension.Page.Replace("{value}", pageIndex).Replace("{pageSize}", pageSize));
            }

            return translation.Result;
        }

        private (String fields, String tableName) StatementParse(Statement statement)
        {
            var modelAliasName = new List<String>();

            if (statement == null)
            {
                var modelType = typeof(TModel);
                modelAliasName.Add(modelType.GetAliasName().ToLower());
                var propertys = modelType.GetProperties().Where(w => w.GetCustomAttributes<PropertyValidate>().Any());
                return (String.Join(",", propertys.Select(s => $@"{modelAliasName.FirstOrDefault()}.{s.Name}")), modelAliasName.FirstOrDefault());
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

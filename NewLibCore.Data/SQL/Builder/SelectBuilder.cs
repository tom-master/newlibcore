﻿using NewLibCore.Data.SQL.BuildExtension;
using NewLibCore.Data.SQL.InternalDataStore;
using NewLibCore.Data.SQL.MapperExtension;
using NewLibCore.Data.SQL.PropertyExtension;
using System;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;

namespace NewLibCore.Data.SQL.Builder
{
    internal class SelectBuilder<TModel> : BuilderBase<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly Expression<Func<TModel, dynamic>> _fields;
        private readonly Int32? _pageIndex;
        private readonly Int32? _pageSize;
        internal SelectBuilder(Expression<Func<TModel, dynamic>> fields = null, Int32? pageIndex = null, Int32? pageSize = null) : base(null)
        {
            _fields = fields;

            _pageIndex = pageIndex;
            _pageSize = pageSize;
        }

        protected internal override SqlTemporaryStore Build(StatementStore statementStore = null)
        {
            var translation = new TranslationToSql();

            var fields = ExtractFieldsAndTableName(_fields);
            translation.TemporaryStore.Append($@"SELECT {fields.fields} FROM {typeof(TModel).Name} AS {fields.tableAliasName} ");
            statementStore.AliasName = fields.tableAliasName;
            translation.Translate(statementStore);
            if (statementStore != null && statementStore.Expression != null)
            {
                translation.TemporaryStore.Append($@" AND {fields.tableAliasName}.IsDeleted = 0");
            }
            else
            {
                translation.TemporaryStore.Append($@" WHERE {fields.tableAliasName}.IsDeleted = 0");
            }

            if (_pageIndex != null && _pageSize != null)
            {
                translation.TemporaryStore.Append($@" {String.Format(SwitchDatabase.Page, _pageSize * (_pageIndex - 1), _pageSize)}");
            }

            Console.WriteLine(translation.TemporaryStore.SqlStore.ToString());
            return translation.TemporaryStore;
        }

        private (String fields, String tableAliasName) ExtractFieldsAndTableName(Expression<Func<TModel, dynamic>> fields)
        {
            var modelAliasName = "";
            if (fields == null)
            {
                var modelType = typeof(TModel);
                modelAliasName = modelType.Name.ToLower();
                var propertys = modelType.GetProperties().Where(w => w.GetCustomAttributes<PropertyValidate>().Any());
                return (String.Join(",", propertys.Select(s => $@"{modelAliasName}.{s.Name}")), modelAliasName);
            }

            modelAliasName = fields.Parameters[0].Type.Name.ToLower();
            if (fields.Body.NodeType == ExpressionType.Constant)
            {
                var constant = (ConstantExpression)fields.Body;
                return (constant.Value + "", modelAliasName);
            }

            var dynamicFields = (fields.Body as NewExpression).Members.Select(s => $@"{modelAliasName}.{s.Name}");
            return (String.Join(",", dynamicFields), modelAliasName);
        }
    }
}

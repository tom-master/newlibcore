﻿using System;
using System.Linq;
using NewLibCore.Data.SQL.Mapper;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Translation;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Builder
{
    internal class ModifyBuilder<TModel> : IBuilder<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly Boolean _isValidate;
        private readonly TranslationStatementStore _statementStore;
        private readonly TModel _instance;

        public ModifyBuilder(TModel model, TranslationStatementStore statementStore, Boolean isValidate = false)
        {
            Parameter.Validate(model);
            Parameter.Validate(statementStore);

            _isValidate = isValidate;
            _statementStore = statementStore;
            _instance = model;
        }

        public TranslationCoreResult Build()
        {
            Parameter.Validate(_instance.GetPropertys());
            _instance.SetUpdateTime();

            if (_isValidate)
            {
                _instance.Validate();
            }

            var translation = new TranslationCore(_statementStore);
            var placeHolder = String.Join(",", _instance.GetPropertys().Select(key => $@"{key.Key.Name}=@{key.Key.Name}"));
            var entityParameters = _instance.GetPropertys().Select(c => new EntityParameter($@"@{c.Key.Name}", c.Value));

            translation.Result.Append($@"UPDATE {typeof(TModel).GetAliasName()} SET {String.Join(",", placeHolder)}", entityParameters);

            if (_statementStore.Where != null)
            {
                translation.Translate();
            }
            _instance.Reset();
            translation.Result.Append($@"{MapperFactory.Mapper.Extension.RowCount}");
            return translation.Result;
        }
    }
}

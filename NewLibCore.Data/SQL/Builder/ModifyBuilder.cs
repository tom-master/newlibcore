using System;
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
        private readonly StatementStore _statementStore;
        private readonly TModel _instance;

        public ModifyBuilder(TModel model, StatementStore statementStore, Boolean isValidate = false)
        {
            Parameter.Validate(model);
            Parameter.Validate(statementStore);

            _isValidate = isValidate;
            _statementStore = statementStore; 
            _instance = model;
        }  

        public TranslationCoreResult Build()
        {
            Parameter.Validate(_instance.PropertyInfos);
            _instance.SetUpdateTime();

            if (_isValidate)
            {
                _instance.Validate();
            }

            var translation = new TranslationCore(_statementStore);
            var placeHolder = String.Join(",", _instance.PropertyInfos.Select(key => $@"{key.Key.Name}=@{key.Key.Name}"));
            var entityParameters = _instance.PropertyInfos.Select(c => new EntityParameter($@"@{c.Key.Name}", c.Value));

            translation.Result.Append($@"UPDATE {typeof(TModel).Name} SET {String.Join(",", placeHolder)}", entityParameters);

            if (_statementStore.Where != null)
            {
                translation.Translate();
            }
            translation.Result.Append($@"{MapperFactory.Mapper.Extension.RowCount}");
            return translation.Result;
        }
    }
}

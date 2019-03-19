using NewLibCore.Data.SQL.DataStore;
using NewLibCore.Data.SQL.MapperExtension;
using NewLibCore.Data.SQL.MapperExtension.AssociationMapperExtension;
using NewLibCore.Data.SQL.MapperExtension.PropertyExtension;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

namespace NewLibCore.Data.SQL.Builder
{
    internal class AddBuilder<TModel> : BuilderBase<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly Boolean _isVerifyModel;
        private readonly IEnumerable<PropertyInfo> _propertyInfos;

        internal AddBuilder(TModel model, Boolean isVerifyModel = false) : base(model)
        {
            _isVerifyModel = isVerifyModel;
            _propertyInfos = ModelType.GetProperties();
        }

        protected internal override TranslationResult Build(StatementStore statementStore = null)
        {
            var translationResult = new TranslationResult();
            var propertyInfos = _propertyInfos.Where(w => w.GetCustomAttributes<PropertyValidate>().Any());
            if (!propertyInfos.Any())
            {
                throw new Exception($@"{ModelType.Name}:没有要插入的列");
            }

            ModelInstance.SetAddTime();
            if (_isVerifyModel)
            {
                ModelInstance.Validate(propertyInfos);
            }
            var parameters = propertyInfos.ToList().Select(c => new SqlParameterMapper($@"@{c.Name}", c.GetValue(ModelInstance))).ToArray();
            Builder(ModelType.Name, translationResult, propertyInfos, parameters);

            return translationResult;
        }

        private void Builder(String tableName, TranslationResult translationResult, IEnumerable<PropertyInfo> propertyInfos, SqlParameterMapper[] parameters)
        {
            translationResult.Append($@" INSERT {tableName} ({String.Join(",", propertyInfos.Select(c => c.Name))} ) VALUES ({String.Join(",", propertyInfos.Select(key => $@"@{key.Name}"))}) {SwitchDatabase.DatabaseSyntax.IdentitySuffix}");
            translationResult.AppendParameter(parameters);

            var subModels = _propertyInfos.Where(w => w.GetCustomAttributes<SubModelAttribute>().Any());
            if (subModels.Any())
            {
                foreach (var subModel in subModels)
                {
                    if ((((TypeInfo)subModel.PropertyType).ImplementedInterfaces as IList<Type>).Any(a => a == typeof(IList) || a == typeof(ICollection) || a == typeof(IEnumerable)))
                    {
                        foreach (var item in subModel.PropertyType.GenericTypeArguments)
                        {
                            Builder(item.Name, translationResult, item.GetProperties(), parameters);
                        }
                    }
                }
            }
        }
    }
}

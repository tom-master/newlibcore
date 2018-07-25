using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using NewLibCore.Data.Mapper.InternalDataStore;
using NewLibCore.Data.Mapper.MapperExtension;

namespace NewLibCore.Data.Mapper
{
    internal class AddBuilder<TModel> : SqlBuilder<TModel> where TModel : class, new()
    {
        private Boolean _isVerifyModel;

        public AddBuilder(TModel model, Boolean isVerifyModel = false) : base(model)
        {
            _isVerifyModel = isVerifyModel;
        }

        protected internal override BuildEntry<TModel> Build()
        {
            var buildEntry = new BuildEntry<TModel>();

            if (_isVerifyModel)
            {
                VerifyModel();
            }

            var columns = GetColumns();
            buildEntry.Append($@" INSERT {ModelType.Name} ({String.Join(",", columns.Select(c => c.Name))} ) VALUES ({String.Join(",", columns.Select(key => $@"@{key.Name}"))})");

            foreach (var item in columns.Select(c => new ParameterMapper($@"@{c.Name}", c.GetValue(ModelInstance))))
            {
                buildEntry.Parameters.Add(item);
            }
            return buildEntry;
        }

        private IEnumerable<PropertyInfo> GetColumns()
        {
            foreach (var item in ModelType.GetProperties(BindingFlags.Instance | BindingFlags.Public).Where(w => w.PropertyType.Name != "IList`1" && w.CustomAttributes.Count() != 0))
            {
                yield return item;
            }
        }


        protected void VerifyModel()
        {
            var propertys = ModelType.GetProperties(BindingFlags.Instance | BindingFlags.Public);
            foreach (var propertyItem in propertys)
            {
                if (propertyItem.CustomAttributes.Count() == 0)
                {
                    continue;
                }

                var validateBases = GetValidateAttributes(propertyItem);
                foreach (var validateItem in validateBases)
                {
                    VerifyPropertyValue(propertyItem, validateItem, propertyItem.GetValue(ModelInstance));

                    var defaultValueAttribute = validateItem as PropertyDefaultValueAttribute;
                    if (defaultValueAttribute != null)
                    {
                        var value = defaultValueAttribute.Value;
                        if (defaultValueAttribute.Value != propertyItem.GetValue(ModelInstance))
                        {
                            if (defaultValueAttribute.Type == typeof(DateTime))
                            {
                                value = defaultValueAttribute.Value;
                            }
                            else
                            {
                                value = propertyItem.GetValue(ModelInstance) ?? value;
                            }
                        }

                        propertyItem.SetValue(ModelInstance, value);
                    }
                }
            }
        }

        #region private

        private IList<ValidateBase> GetValidateAttributes(PropertyInfo propertyInfo)
        {
            var validateAttributes = propertyInfo.GetCustomAttributes<ValidateBase>(true);

            if (validateAttributes.GroupBy(g => g.Order).Where(w => w.Count() > 1).Any())
            {
                throw new Exception($@"{propertyInfo.Name} 中使用了多个优先级相同的特性");
            }

            return validateAttributes.OrderByDescending(o => o.Order).ToList();
        }



        private void VerifyPropertyValue(PropertyInfo propertyInfo, ValidateBase validate, Object value)
        {
            if (!validate.IsValidate(value))
            {
                throw new Exception(validate.FailReason($@"{propertyInfo.DeclaringType.FullName}.{propertyInfo.Name}"));
            }
        }

        #endregion
    }
}

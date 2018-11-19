using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using NewLibCore.Data.SQL.BuildExtension;
using NewLibCore.Data.SQL.MapperExtension;
using NewLibCore.Data.SQL.PropertyExtension;

namespace NewLibCore.Data.SQL.Builder
{
    internal abstract class BuilderBase<TModel> where TModel : PropertyMonitor, new()
    {
        protected Type ModelType { get; }

        protected TModel ModelInstance { get; }

        protected BuilderBase(TModel model)
        {
            if (model == null)
            {
                ModelInstance = Activator.CreateInstance<TModel>();
                ModelType = typeof(TModel);
            }
            else
            {
                ModelInstance = model;
                ModelType = model.GetType();
            }
        }

        protected internal abstract BuildEntry<TModel> Build();

        protected void ValidateModel(IList<PropertyInfo> propertyInfos)
        {
            var propertys = propertyInfos;
            foreach (var propertyItem in propertys)
            {
                if (!propertyItem.CustomAttributes.Any())
                {
                    continue;
                }

                var validateBases = GetValidateAttributes(propertyItem);
                var propertyValue = propertyItem.GetValue(ModelInstance);
                for (int i = 0; i < validateBases.Count; i++)
                {
                    if (validateBases[i] is PropertyRequiredAttribute)
                    {
                        if (!validateBases[i].IsValidate(propertyValue))
                        {
                            if (i + 1 >= validateBases.Count)
                            {
                                ThrowValidateException(validateBases[i + 1], propertyItem);
                            }

                            if (validateBases[i + 1] is PropertyDefaultValueAttribute)
                            {
                                SetPropertyDefaultValue((PropertyDefaultValueAttribute)validateBases[i + 1], propertyItem, propertyValue);
                                i = i + 1;
                                continue;
                            }
                            ThrowValidateException(validateBases[i], propertyItem);
                        }
                    }
                    else if (validateBases[i] is PropertyDefaultValueAttribute)
                    {
                        if (!validateBases[i].IsValidate(propertyValue))
                        {
                            ThrowValidateException(validateBases[i], propertyItem);
                        }
                        if (String.IsNullOrEmpty(propertyValue + ""))
                        {
                            SetPropertyDefaultValue((PropertyDefaultValueAttribute)validateBases[i], propertyItem, propertyValue);
                        }
                    }
                    else if (validateBases[i] is PropertyInputRangeAttribute)
                    {
                        if (!validateBases[i].IsValidate(propertyValue))
                        {
                            ThrowValidateException(validateBases[i], propertyItem);
                        }
                    }
                }
            }
        }

        private void SetPropertyDefaultValue(PropertyDefaultValueAttribute defaultValueAttribute, PropertyInfo propertyItem, Object rawPropertyValue)
        {
            var propertyInstanceValue = rawPropertyValue;
            if (String.IsNullOrEmpty(propertyInstanceValue + "") || (propertyInstanceValue.GetType() == typeof(DateTime) && (DateTime)propertyInstanceValue == default(DateTime)))
            {
                propertyItem.SetValue(ModelInstance, defaultValueAttribute.Value);
            }
        }

        private void ThrowValidateException(PropertyValidate validateBase, PropertyInfo propertyItem)
        {
            throw new Exception(validateBase.FailReason($@"{propertyItem.DeclaringType.FullName}.{propertyItem.Name}"));
        }

        private IList<PropertyValidate> GetValidateAttributes(PropertyInfo propertyInfo)
        {
            var validateAttributes = propertyInfo.GetCustomAttributes<PropertyValidate>(true);

            if (validateAttributes.GroupBy(g => g.Order).Where(w => w.Count() > 1).Any())
            {
                throw new Exception($@"{propertyInfo.Name} 中使用了多个优先级相同的特性");
            }

            return validateAttributes.OrderByDescending(o => o.Order).ToList();
        }
    }
}

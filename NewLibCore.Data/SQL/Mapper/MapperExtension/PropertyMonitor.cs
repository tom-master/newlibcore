using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using NewLibCore.Data.SQL.Mapper.Extension.PropertyExtension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Extension
{
    public abstract class PropertyMonitor
    {
        private readonly List<KeyValuePair<PropertyInfo, Object>> _propertys = new List<KeyValuePair<PropertyInfo, Object>>();

        protected void OnChanged(String propertyName, Object propertyValue)
        {
            Parameter.Validate(propertyName);

            var propertyInfo = GetType().GetProperty(propertyName, BindingFlags.Public | BindingFlags.Instance | BindingFlags.NonPublic);
            if (propertyInfo == null)
            {
                throw new ArgumentException($@"属性：{propertyName},不属于类：{GetType().Name}或它的父类");
            }
            _propertys.Add(new KeyValuePair<PropertyInfo, Object>(propertyInfo, propertyValue));
        }

        protected internal IReadOnlyList<KeyValuePair<PropertyInfo, Object>> GetPropertys()
        {
            return _propertys.AsReadOnly();
        }

        protected internal virtual void SetUpdateTime() { }

        protected internal virtual void SetAddTime() { }

        protected internal void Validate()
        {
            Parameter.Validate(GetPropertys());
            var propertys = GetPropertys();
            foreach (var keyValuePair in propertys)
            {
                var propertyItem = keyValuePair.Key;
                if (!propertyItem.CustomAttributes.Any())
                {
                    continue;
                }

                var validateBases = GetValidateAttributes(propertyItem);
                var propertyValue = keyValuePair.Value;
                for (var i = 0; i < validateBases.Count; i++)
                {
                    if (validateBases[i] is RequiredAttribute)
                    {
                        if (!validateBases[i].IsValidate(propertyValue))
                        {
                            if (i + 1 >= validateBases.Count)
                            {
                                ThrowValidateException(validateBases[i + 1], propertyItem);
                            }

                            if (validateBases[i + 1] is DefaultValueAttribute)
                            {
                                SetPropertyDefaultValue((DefaultValueAttribute)validateBases[i + 1], propertyItem, propertyValue);
                                i = i + 1;
                                continue;
                            }
                            ThrowValidateException(validateBases[i], propertyItem);
                        }
                    }
                    else if (validateBases[i] is DefaultValueAttribute)
                    {
                        if (!validateBases[i].IsValidate(propertyValue))
                        {
                            ThrowValidateException(validateBases[i], propertyItem);
                        }
                        SetPropertyDefaultValue((DefaultValueAttribute)validateBases[i], propertyItem, propertyValue);
                    }
                    else if (validateBases[i] is InputRangeAttribute)
                    {
                        if (!validateBases[i].IsValidate(propertyValue))
                        {
                            ThrowValidateException(validateBases[i], propertyItem);
                        }
                    }
                }
            }
        }

        internal void Reset()
        {
            _propertys.Clear();
        }

        private void SetPropertyDefaultValue(DefaultValueAttribute defaultValueAttribute, PropertyInfo propertyItem, Object rawPropertyValue)
        {
            Parameter.Validate(defaultValueAttribute);
            Parameter.Validate(propertyItem);

            var propertyInstanceValue = rawPropertyValue;
            if (String.IsNullOrEmpty(propertyInstanceValue + "") || (propertyInstanceValue.GetType() == typeof(DateTime) && (DateTime)propertyInstanceValue == default(DateTime)))
            {
                propertyItem.SetValue(this, defaultValueAttribute.Value);
            }
        }

        private void ThrowValidateException(PropertyValidate validateBase, PropertyInfo propertyItem)
        {
            throw new Exception(validateBase.FailReason($@"{propertyItem.DeclaringType.FullName}.{propertyItem.Name}"));
        }

        private IList<PropertyValidate> GetValidateAttributes(PropertyInfo propertyInfo)
        {
            Parameter.Validate(propertyInfo);

            var validateAttributes = propertyInfo.GetCustomAttributes<PropertyValidate>(true);
            if (validateAttributes.GroupBy(g => g.Order).Where(w => w.Count() > 1).Any())
            {
                throw new Exception($@"{propertyInfo.Name} 中使用了多个优先级相同的特性");
            }

            return validateAttributes.OrderByDescending(o => o.Order).ToList();
        }
    }
}



using NewLibCore.Data.SQL.MapperExtension;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

namespace NewLibCore.Data.SQL.PropertyExtension
{
    public abstract class PropertyMonitor
    {
        protected internal IList<PropertyInfo> PropertyInfos { get; }

        protected PropertyMonitor()
        {
            PropertyInfos = new List<PropertyInfo>();
        }

        protected void OnPropertyChanged(String propertyName)
        {
            if (String.IsNullOrEmpty(propertyName))
            {
                throw new ArgumentNullException("需要指定更新的字段名称");
            }
            var propertyInfo = GetType().GetProperty(propertyName);
            if (propertyInfo == null)
            {
                throw new ArgumentException($@"属性：{propertyName},不属于类：{GetType().Name}");
            }
            PropertyInfos.Add(propertyInfo);
        }

        protected internal virtual void SetUpdateTime() { }

        protected internal void Validate(IEnumerable<PropertyInfo> propertyInfos)
        {
            var propertys = propertyInfos;
            foreach (var propertyItem in propertys)
            {
                if (!propertyItem.CustomAttributes.Any())
                {
                    continue;
                }

                var validateBases = GetValidateAttributes(propertyItem);
                var propertyValue = propertyItem.GetValue(this);
                for (var i = 0; i < validateBases.Count; i++)
                {
                    if (validateBases[i] is PropertyKeyAttribute)
                    {
                        continue;
                    }

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
                        SetPropertyDefaultValue((PropertyDefaultValueAttribute)validateBases[i], propertyItem, propertyValue);
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
                propertyItem.SetValue(this, defaultValueAttribute.Value);
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



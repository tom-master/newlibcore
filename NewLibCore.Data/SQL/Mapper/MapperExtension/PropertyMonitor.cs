using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using NewLibCore.Data.SQL.Mapper.Extension.PropertyExtension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Extension
{
    internal class PO
    {
        internal String DeclaringTypeFullName { get; set; }

        internal String PropertyName { get; set; }

        internal Object Value { get; set; }

        internal PropertyValidate[] Validates { get; set; }
    }


    public abstract class PropertyMonitor
    {
        private readonly IList<PO> _propertys = new List<PO>();

        protected void OnChanged(String propertyName)
        {
            Parameter.Validate(propertyName);

            var propertyInfo = GetType().GetProperty(propertyName, BindingFlags.Public | BindingFlags.Instance | BindingFlags.NonPublic);
            if (propertyInfo == null)
            {
                throw new ArgumentException($@"属性：{propertyName},不属于类：{GetType().Name}或它的父类");
            }

            _propertys.Add(new PO
            {
                DeclaringTypeFullName = propertyInfo.DeclaringType.FullName,
                PropertyName = propertyName,
                Value = new FastProperty(propertyInfo).Get(this),
                Validates = propertyInfo.GetCustomAttributes<PropertyValidate>(true).ToArray()
            });
        }

        internal void OnChanged()
        {
            var propertys = GetType().GetProperties().Where(w => w.GetCustomAttributes<PropertyValidate>().Any() && w.Name != "AddTime" && w.Name != "LastModifyTime");
            SetAddTime();
            SetUpdateTime();
            foreach (var item in propertys)
            {
                OnChanged(item.Name);
            }
        }

        internal IReadOnlyList<KeyValuePair<String, Object>> GetPropertys()
        {
            return _propertys.Select(s => new KeyValuePair<String, Object>(s.PropertyName, s.Value)).ToList().AsReadOnly();
        }

        protected internal virtual void SetUpdateTime() { }

        protected internal virtual void SetAddTime() { }

        protected internal void Validate()
        {
            foreach (var po in _propertys)
            {
                if (!po.Validates.Any())
                {
                    continue;
                }

                var validateBases = ValidateAttributeOrder(po.PropertyName, po.Validates);
                var propertyValue = po.Value;
                for (var i = 0; i < validateBases.Count; i++)
                {
                    if (validateBases[i] is RequiredAttribute)
                    {
                        if (!validateBases[i].IsValidate(propertyValue))
                        {
                            if (i + 1 >= validateBases.Count)
                            {
                                ThrowValidateException(validateBases[i + 1], po);
                            }

                            if (validateBases[i + 1] is DefaultValueAttribute)
                            {
                                SetPropertyDefaultValue((DefaultValueAttribute)validateBases[i + 1], po, propertyValue);
                                i = i + 1;
                                continue;
                            }
                            ThrowValidateException(validateBases[i], po);
                        }
                    }
                    else if (validateBases[i] is DefaultValueAttribute)
                    {
                        if (!validateBases[i].IsValidate(propertyValue))
                        {
                            ThrowValidateException(validateBases[i], po);
                        }
                        SetPropertyDefaultValue((DefaultValueAttribute)validateBases[i], po, propertyValue);
                    }
                    else if (validateBases[i] is InputRangeAttribute)
                    {
                        if (!validateBases[i].IsValidate(propertyValue))
                        {
                            ThrowValidateException(validateBases[i], po);
                        }
                    }
                }
            }
        }

        internal void Reset()
        {
            _propertys.Clear();
        }

        private void SetPropertyDefaultValue(DefaultValueAttribute defaultValueAttribute, PO propertyItem, Object rawPropertyValue)
        {
            Parameter.Validate(defaultValueAttribute);
            Parameter.Validate(propertyItem);

            var propertyInstanceValue = rawPropertyValue;
            if (String.IsNullOrEmpty(propertyInstanceValue + "") || (propertyInstanceValue.GetType() == typeof(DateTime) && (DateTime)propertyInstanceValue == default))
            {
                propertyItem.Value = defaultValueAttribute.Value;
            }
        }

        private void ThrowValidateException(PropertyValidate validateBase, PO po)
        {
            Parameter.Validate(po);
            Parameter.Validate(validateBase);
            throw new Exception(validateBase.FailReason($@"{po.DeclaringTypeFullName}.{po.PropertyName}"));
        }

        private IList<PropertyValidate> ValidateAttributeOrder(String propertyName, PropertyValidate[] validates)
        {
            Parameter.Validate(validates);
            if (validates.GroupBy(g => g.Order).Where(w => w.Count() > 1).Any())
            {
                throw new Exception($@"{propertyName} 中使用了多个优先级相同的特性");
            }
            return validates.OrderByDescending(o => o.Order).ToList();
        }
    }
}



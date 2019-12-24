using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Validate;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 监控实体值变更
    /// </summary>
    public abstract class PropertyMonitor
    {
        private readonly IList<ChangedProperty> _changedPropertys = new List<ChangedProperty>();

        private readonly Type _type;

        internal PropertyMonitor()
        {
            _type = GetType();
        }

        /// <summary>
        /// 获取所有出现值变更的属性
        /// </summary>
        /// <param name="propertyName">属性名称</param>
        protected void OnChanged(String propertyName)
        {
            Parameter.Validate(propertyName);

            var propertyInfo = _type.GetProperty(propertyName, BindingFlags.Public | BindingFlags.Instance | BindingFlags.NonPublic);
            if (propertyInfo == null)
            {
                throw new ArgumentException($@"属性：{propertyName},不属于类：{_type.Name}或它的父类");
            }

            _changedPropertys.Add(new ChangedProperty
            {
                IsNullable = Nullable.GetUnderlyingType(propertyInfo.PropertyType) != null,
                DeclaringType = propertyInfo.DeclaringType.FullName,
                PropertyName = propertyName,
                Value = new FastProperty(propertyInfo).Get(this),
                Validates = propertyInfo.GetCustomAttributes<PropertyValidate>(true).ToArray()
            });
        }

        /// <summary>
        /// 获取所有出现值变更的属性
        /// </summary>
        internal void OnChanged()
        {
            var propertys = _type.GetProperties(BindingFlags.Instance | BindingFlags.Public).Where(w => w.GetCustomAttributes<PropertyValidate>().Any() && w.Name != "AddTime" && w.Name != "LastModifyTime");
            SetAddTime();
            SetUpdateTime();
            foreach (var item in propertys)
            {
                OnChanged(item.Name);
            }
        }

        /// <summary>
        /// 获取值发生变更的属性
        /// </summary>
        /// <returns></returns>
        internal IReadOnlyList<KeyValuePair<String, Object>> GetChangedPropertys()
        {
            return _changedPropertys.Select(s => new KeyValuePair<String, Object>(s.PropertyName, s.Value)).ToList().AsReadOnly();
        }

        /// <summary>
        /// 设置更新时间
        /// </summary>
        protected internal virtual void SetUpdateTime() { }

        /// <summary>
        /// 设置添加时间
        /// </summary>
        protected internal virtual void SetAddTime() { }

        /// <summary>
        /// 验证属性是否合法
        /// </summary>
        protected internal void Validate()
        {
            foreach (var changedProperty in _changedPropertys)
            {
                if (!changedProperty.Validates.Any() || changedProperty.IsNullable)
                {
                    continue;
                }

                var validateBases = ValidateAttributeOrder(changedProperty.PropertyName, changedProperty.Validates);
                var propertyValue = changedProperty.Value;
                for (var i = 0; i < validateBases.Count; i++)
                {
                    if (validateBases[i] is RequiredAttribute)
                    {
                        if (!validateBases[i].IsValidate(propertyValue))
                        {
                            if (i + 1 >= validateBases.Count)
                            {
                                ThrowValidateException(validateBases[i + 1], changedProperty);
                            }

                            if (validateBases[i + 1] is DefaultValueAttribute)
                            {
                                SetPropertyDefaultValue((DefaultValueAttribute)validateBases[i + 1], changedProperty, propertyValue);
                                i = i + 1;
                                continue;
                            }
                            ThrowValidateException(validateBases[i], changedProperty);
                        }
                    }
                    else if (validateBases[i] is DefaultValueAttribute)
                    {
                        if (!validateBases[i].IsValidate(propertyValue))
                        {
                            ThrowValidateException(validateBases[i], changedProperty);
                        }
                        SetPropertyDefaultValue((DefaultValueAttribute)validateBases[i], changedProperty, propertyValue);
                    }
                    else if (validateBases[i] is InputRangeAttribute)
                    {
                        if (!validateBases[i].IsValidate(propertyValue))
                        {
                            ThrowValidateException(validateBases[i], changedProperty);
                        }
                    }
                }
            }
        }

        /// <summary>
        /// 清空存储的属性
        /// </summary>
        internal void Reset()
        {
            _changedPropertys.Clear();
        }

        /// <summary>
        /// 对属性设置默认值
        /// </summary>
        /// <param name="defaultValueAttribute">默认值</param>
        /// <param name="propertyItem">属性项</param>
        /// <param name="rawPropertyValue">原始的属性值</param>
        private void SetPropertyDefaultValue(DefaultValueAttribute defaultValueAttribute, ChangedProperty propertyItem, Object rawPropertyValue)
        {
            Parameter.Validate(defaultValueAttribute);
            Parameter.Validate(propertyItem);

            var propertyInstanceValue = rawPropertyValue;
            var propertyInstanceValueType = rawPropertyValue.GetType();
            if (String.IsNullOrEmpty(propertyInstanceValue + "") || propertyInstanceValueType.IsValueType || propertyInstanceValueType.IsNumeric() || (propertyInstanceValue.GetType() == typeof(DateTime) && (DateTime)propertyInstanceValue == default))
            {
                propertyItem.Value = defaultValueAttribute.Value;
            }
        }

        /// <summary>
        /// 抛出验证失败的异常
        /// </summary>
        /// <param name="validateBase">特性验证基类</param>
        /// <param name="po">属性项</param>
        private void ThrowValidateException(PropertyValidate validateBase, ChangedProperty changedProperty)
        {
            Parameter.Validate(changedProperty);
            Parameter.Validate(validateBase);
            throw new Exception(validateBase.FailReason($@"{changedProperty.DeclaringType}.{changedProperty.PropertyName}"));
        }

        /// <summary>
        /// 将属性上同时应用的多个特性进行排序
        /// </summary>
        /// <param name="propertyName">属性名</param>
        /// <param name="validates">属性验证列表</param>
        /// <returns></returns>
        private IList<PropertyValidate> ValidateAttributeOrder(String propertyName, PropertyValidate[] validates)
        {
            Parameter.Validate(validates);
            if (validates.GroupBy(g => g.Order).Where(w => w.Count() > 1).Any())
            {
                throw new Exception($@"{propertyName} 中使用了多个优先级相同的特性");
            }
            return validates.OrderByDescending(o => o.Order).ToList();
        }

        private class ChangedProperty
        {
            internal Boolean IsNullable { get; set; }

            internal String DeclaringType { get; set; }

            internal String PropertyName { get; set; }

            internal Object Value { get; set; }

            internal PropertyValidate[] Validates { get; set; }
        }
    }
}



using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using NewLibCore.Storage.SQL.DataConvert;
using NewLibCore.Storage.SQL.Validate;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL
{
    /// <summary>
    /// 监控实体值变更
    /// </summary>
    public abstract class PropertyMonitor
    {
        private readonly Type _subClassType;

        private readonly IList<ChangedProperty> _changedPropertys = new List<ChangedProperty>();

        internal PropertyMonitor()
        {
            _subClassType = GetType();
        }

        /// <summary>
        /// 获取所有出现值变更的属性
        /// </summary>
        /// <param name="propertyName">属性名称</param>
        private void OnChanged(params PropertyInfo[] propertyInfos)
        {
            foreach (var propertyInfo in propertyInfos)
            {
                _changedPropertys.Add(new ChangedProperty
                {
                    IsNullable = Nullable.GetUnderlyingType(propertyInfo.PropertyType) != null,
                    DeclaringType = propertyInfo.DeclaringType.FullName,
                    Type = propertyInfo.PropertyType,
                    PropertyName = propertyInfo.Name,
                    Value = new FastProperty(propertyInfo).Get(this),
                    Validates = propertyInfo.GetAttributes<PropertyValidateAttribute>(true)
                });
            }
        }

        protected void OnChanged(string propertyName)
        {
            var propertyInfo = _subClassType.GetProperty(propertyName, BindingFlags.Public | BindingFlags.Instance);
            if (propertyInfo == null)
            {
                throw new ArgumentException($@"属性：{propertyName},不属于类：{_subClassType.Name}或它的父类");
            }
            OnChanged(propertyInfo);
        }

        /// <summary>
        /// 获取所有出现值变更的属性
        /// </summary>
        internal void OnChanged()
        {
            var propertys = _subClassType.GetProperties(BindingFlags.Instance | BindingFlags.Public)
                .Where(w => w.GetAttributes<PropertyValidateAttribute>().Any() && !w.GetAttributes<PrimaryKeyAttribute>().Any())
                .ToArray();
            OnChanged(propertys);
        }

        internal SqlElements GetSqlElements()
        {
            return new SqlElements(_changedPropertys);
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
        /// 检查属性的值是否合法
        /// </summary>
        internal void CheckPropertyValue()
        {
            foreach (var changedProperty in _changedPropertys)
            {
                if (!changedProperty.Validates.Any() || changedProperty.IsNullable)
                {
                    continue;
                }

                var validateBases = ValidateAttributeOrder(changedProperty.PropertyName, changedProperty.Validates);
                for (var i = 0; i < validateBases.Count; i++)
                {
                    if (validateBases[i] is RequiredAttribute)
                    {
                        if (!validateBases[i].IsValidate(changedProperty))
                        {
                            if (i + 1 >= validateBases.Count)
                            {
                                ThrowValidateException(validateBases[i], changedProperty);
                            }

                            if (validateBases[i + 1] is DefaultValueAttribute attribute)
                            {
                                if (!validateBases[i + 1].IsValidate(changedProperty))
                                {
                                    ThrowValidateException(validateBases[i], changedProperty);
                                }
                                SetPropertyDefaultValue(attribute, changedProperty, changedProperty.Value);
                                i += 1;
                                continue;
                            }
                            ThrowValidateException(validateBases[i], changedProperty);
                        }
                    }
                    else if (validateBases[i] is DefaultValueAttribute attribute)
                    {
                        if (!validateBases[i].IsValidate(changedProperty))
                        {
                            ThrowValidateException(validateBases[i], changedProperty);
                        }
                        SetPropertyDefaultValue(attribute, changedProperty, changedProperty.Value);
                    }
                    else if (validateBases[i] is InputRangeAttribute)
                    {
                        if (!validateBases[i].IsValidate(changedProperty))
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
            Check.IfNullOrZero(defaultValueAttribute);
            Check.IfNullOrZero(propertyItem);

            var propertyInstanceValue = rawPropertyValue;
            var propertyInstanceValueType = propertyItem.Type;

            var isDefaultValue = propertyInstanceValue.ToString() == (propertyInstanceValueType.IsValueType ? Activator.CreateInstance(propertyInstanceValueType).ToString() : null);
            //判断是否为字符串类型的属性值为空
            if (propertyInstanceValueType == typeof(String) && String.IsNullOrEmpty(propertyInstanceValue + ""))
            {
                propertyItem.Value = defaultValueAttribute.Value;
            }
            else if ((propertyInstanceValueType.IsValueType && propertyInstanceValueType.IsNumeric()) && isDefaultValue)  //判断是否为值类型并且值为值类型的默认值
            {
                propertyItem.Value = defaultValueAttribute.Value;
            }
            else if (propertyInstanceValue.GetType() == typeof(DateTime) && isDefaultValue)  //判断是否为时间类型并且时间类型的值为默认值
            {
                propertyItem.Value = defaultValueAttribute.Value;
            }
            else if (propertyInstanceValue.GetType() == typeof(Boolean) && isDefaultValue)
            {
                propertyItem.Value = defaultValueAttribute.Value;
            }
        }

        /// <summary>
        /// 抛出验证失败的异常
        /// </summary>
        /// <param name="validateBase">特性验证基类</param>
        /// <param name="po">属性项</param>
        private void ThrowValidateException(PropertyValidateAttribute validateBase, ChangedProperty changedProperty)
        {
            Check.IfNullOrZero(changedProperty);
            Check.IfNullOrZero(validateBase);

            var reason = validateBase.FailReason($@"{changedProperty.DeclaringType}.{changedProperty.PropertyName}");
            throw new Exception(reason);
        }

        /// <summary>
        /// 将属性上同时应用的多个特性进行排序
        /// </summary>
        /// <param name="propertyName">属性名</param>
        /// <param name="validates">属性验证列表</param>
        /// <returns></returns>
        private List<PropertyValidateAttribute> ValidateAttributeOrder(String propertyName, IEnumerable<PropertyValidateAttribute> validates)
        {
            Check.IfNullOrZero(validates);
            if (validates.GroupBy(g => g.Order).Where(w => w.Count() > 1).Any())
            {
                throw new Exception($@"{propertyName} 中使用了多个优先级相同的特性");
            }
            return validates.OrderByDescending(o => o.Order).ToList();
        }
    }

    /// <summary>
    /// 继承于EntityBase类的子类中的信息
    /// </summary>
    internal class ChangedProperty
    {
        /// <summary>
        /// 是否为可空类型
        /// </summary>
        internal Boolean IsNullable { get; set; }

        /// <summary>
        /// 属性类型
        /// </summary>
        internal Type Type { get; set; }

        /// <summary>
        /// 属性全类型名
        /// </summary>
        internal String DeclaringType { get; set; }

        /// <summary>
        /// 属性名称
        /// </summary>
        internal String PropertyName { get; set; }

        /// <summary>
        /// 属性值
        /// </summary>
        internal Object Value { get; set; }

        /// <summary>
        /// 属性上应用的PropertyValidate特性
        /// </summary>
        internal IEnumerable<PropertyValidateAttribute> Validates { get; set; }
    }

    internal class SqlElements
    {
        internal String Fields { get; private set; }

        internal String InsertPlaceHolders { get; private set; }

        internal String UpdatePlaceHolders { get; private set; }

        internal IEnumerable<MapperParameter> Parameters { get; private set; }

        internal SqlElements(IEnumerable<ChangedProperty> changedProperties)
        {
            Check.IfNullOrZero(changedProperties);

            Fields = String.Join(",", changedProperties.Select(c => c.PropertyName));
            InsertPlaceHolders = String.Join(",", changedProperties.Select(key => $@"@{key.PropertyName}"));
            UpdatePlaceHolders = String.Join(",", changedProperties.Select(c => $@"{c.PropertyName}=@{c.PropertyName}"));
            Parameters = changedProperties.Select(c => new MapperParameter(c.PropertyName, c.Value)).ToArray();
        }
    }
}



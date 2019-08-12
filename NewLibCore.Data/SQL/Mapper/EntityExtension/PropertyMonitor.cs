using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.EntityExtension
{
    internal class PO
    {
        internal String DeclaringTypeFullName { get; set; }

        internal String PropertyName { get; set; }

        internal Object Value { get; set; }

        internal PropertyValidate[] Validates { get; set; }
    }

    /// <summary>
    /// 监控实体值变更
    /// </summary>
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

        /// <summary>
        /// 获取值发生变更的属性
        /// </summary>
        /// <returns></returns>
        internal IReadOnlyList<KeyValuePair<String, Object>> GetChangedProperty()
        {
            return _propertys.Select(s => new KeyValuePair<String, Object>(s.PropertyName, s.Value)).ToList().AsReadOnly();
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

        /// <summary>
        /// 清空存储的属性
        /// </summary>
        internal void Reset()
        {
            _propertys.Clear();
        }

        /// <summary>
        /// 如果属性值为空并且用默认值特性进行了修饰则设置属性默认值
        /// </summary>
        /// <param name="defaultValueAttribute"></param>
        /// <param name="propertyItem"></param>
        /// <param name="rawPropertyValue"></param>
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

        /// <summary>
        /// 抛出异常
        /// </summary>
        /// <param name="validateBase"></param>
        /// <param name="po"></param>
        private void ThrowValidateException(PropertyValidate validateBase, PO po)
        {
            Parameter.Validate(po);
            Parameter.Validate(validateBase);
            throw new Exception(validateBase.FailReason($@"{po.DeclaringTypeFullName}.{po.PropertyName}"));
        }

        /// <summary>
        /// 将属性上同时应用的多个特性进行排序
        /// </summary>
        /// <param name="propertyName"></param>
        /// <param name="validates"></param>
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
    }
}



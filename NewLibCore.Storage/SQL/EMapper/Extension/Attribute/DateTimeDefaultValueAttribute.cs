﻿using System;

namespace NewLibCore.Storage.SQL.Validate
{
    /// <summary>
    /// 提供默认时间值
    /// </summary>
    public class DateTimeDefaultValueAttribute : DefaultValueAttribute
    {
        /// <summary>
        /// 初始化DateTimeDefaultValueAttribute类的实例
        /// </summary>
        public DateTimeDefaultValueAttribute() : base(typeof(DateTime), DateTime.Now)
        {

        }

        internal override bool IsValidate(ChangedProperty property) => base.IsValidate(property);
    }
}

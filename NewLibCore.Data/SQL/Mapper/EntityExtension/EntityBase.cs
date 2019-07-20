using System;
using NewLibCore.Data.SQL.Mapper.AttributeExtension;
using NewLibCore.Data.SQL.Mapper.AttributeExtension.Association;

namespace  NewLibCore.Data.SQL.Mapper.EntityExtension
{
    /// <summary>
    /// 实体基类
    /// </summary>
    public abstract class EntityBase : PropertyMonitor
    {
        protected EntityBase()
        {
            IsDeleted = false;
        }

        /// <summary>
        /// 主键
        /// </summary>
        /// <value></value>
        [PrimaryKey]
        public Int32 Id { get; internal set; }

        /// <summary>
        /// 是否删除
        /// </summary>
        /// <value></value>
        [DefaultValue(typeof(Boolean))]
        public Boolean IsDeleted { get; internal set; }

        /// <summary>
        /// 添加时间
        /// </summary>
        /// <value></value>
        [DateTimeDefaultValue]
        public DateTime AddTime { get; internal set; }

        /// <summary>
        /// 更新时间
        /// </summary>
        /// <value></value>
        [DateTimeDefaultValue]
        public DateTime LastModifyTime { get; internal set; }

        /// <summary>
        /// 删除一个对象
        /// </summary>
        public virtual void Remove()
        {
            IsDeleted = true;
            OnChanged(nameof(IsDeleted));
        }

        /// <summary>
        /// 设置添加时间
        /// </summary>
        protected internal override void SetAddTime()
        {
            AddTime = DateTime.Now;
            OnChanged(nameof(AddTime));
        }

        /// <summary>
        /// 设置更新时间
        /// </summary>
        protected internal override void SetUpdateTime()
        {
            LastModifyTime = DateTime.Now;
            OnChanged(nameof(LastModifyTime));
        }
    }
}

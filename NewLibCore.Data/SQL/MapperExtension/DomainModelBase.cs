using NewLibCore.Data.SQL.MapperExtension.AssociationMapperExtension;
using NewLibCore.Data.SQL.MapperExtension.PropertyExtension;
using System;

namespace NewLibCore.Data.SQL.MapperExtension
{
    public abstract class DomainModelBase : PropertyMonitor
    {
        protected DomainModelBase()
        {
            IsDeleted = false;
        }

        [PrimaryKey]
        public Int32 Id { get; internal set; }

        [DefaultValue(typeof(Boolean))]
        public Boolean IsDeleted { get; private set; }

        [DateTimeDefaultValue]
        internal DateTime AddTime { get; private set; }

        [DateTimeDefaultValue]
        internal DateTime LastModifyTime { get; private set; }

        public virtual void Remove()
        {
            IsDeleted = true;
            OnPropertyChanged(nameof(IsDeleted));
        }

        protected internal override void SetAddTime()
        {
            AddTime = DateTime.Now;
            OnPropertyChanged(nameof(AddTime));
        }

        protected internal override void SetUpdateTime()
        {
            LastModifyTime = DateTime.Now;
            OnPropertyChanged(nameof(LastModifyTime));
        }
    }
}

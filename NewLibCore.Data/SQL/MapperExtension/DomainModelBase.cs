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
        public Boolean IsDeleted { get; internal set; }

        [DateTimeDefaultValue]
        public DateTime AddTime { get; internal set; }

        [DateTimeDefaultValue]
        public DateTime LastModifyTime { get; internal set; }

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

using NewLibCore.Data.SQL.MapperExtension;
using System;

namespace NewLibCore.Data.SQL.PropertyExtension
{
    public abstract class DomainModelBase : PropertyMonitor
    {
        protected DomainModelBase()
        {
            IsDeleted = false;
        }

        public Int32 Id { get; internal set; }

        [PropertyDefaultValue(typeof(Boolean))]
        public Boolean IsDeleted { get; protected set; }

        [DateTimeDefaultValue]
        public DateTime AddTime { get; protected set; }

        [DateTimeDefaultValue]
        public DateTime LastModifyTime { get; protected set; }

        public void Remove()
        {
            IsDeleted = true;
            OnPropertyChanged(nameof(IsDeleted));
        }

        public override void SetUpdateTime()
        {
            LastModifyTime = DateTime.Now;
            OnPropertyChanged(nameof(LastModifyTime));
        }
    }
}

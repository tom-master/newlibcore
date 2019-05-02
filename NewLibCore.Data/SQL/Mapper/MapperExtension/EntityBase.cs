using System;
using NewLibCore.Data.SQL.Mapper.Execute;
using NewLibCore.Data.SQL.Mapper.Extension.AssociationMapperExtension;
using NewLibCore.Data.SQL.Mapper.Extension.PropertyExtension;
using NewLibCore.Data.SQL.Mapper.Translation;

namespace NewLibCore.Data.SQL.Mapper.Extension
{
    public abstract class EntityBase : PropertyMonitor
    {
        protected EntityBase()
        {
            IsDeleted = false;

            StatementStore = new StatementStore();
            ExecuteCore = new ExecuteCore();
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

        internal StatementStore StatementStore { get; }

        internal ExecuteCore ExecuteCore { get; }
    }
}

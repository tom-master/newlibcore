using NewLibCore.Data.SQL.DataStore;
using NewLibCore.Data.SQL.MapperExtension;
using NewLibCore.Data.SQL.PropertyExtension;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

namespace NewLibCore.Data.SQL.Builder
{
    internal abstract class BuilderBase<TModel> where TModel : PropertyMonitor, new()
    {
        protected Type ModelType { get; }

        protected TModel ModelInstance { get; }

        protected BuilderBase(TModel model)
        {
            if (model == null)
            {
                ModelInstance = Activator.CreateInstance<TModel>();
                ModelType = typeof(TModel);
            }
            else
            {
                ModelInstance = model;
                ModelType = model.GetType();
            }
        }

        protected internal abstract TranslationResult Build(StatementStore statementStore = null); 
    }
}

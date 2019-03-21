using NewLibCore.Data.SQL.InternalExecute;
using NewLibCore.Data.SQL.InternalTranslation;
using NewLibCore.Data.SQL.MapperExtension;
using System;
using System.Collections.Generic;
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

        protected internal TranslationResult Build()
        {
            var properties = ValidateModel();
            var translation = new TranslationToSql();

            InternalBuildHead(properties, translation);
            InternalBuildTail(translation);

            return translation.TranslationResult;
        }

        protected internal virtual IList<PropertyInfo> ValidateModel() { return null; }

        protected internal virtual void InternalBuildTail(TranslationToSql translation) { return; }

        protected internal virtual void InternalBuildHead(IList<PropertyInfo> properties, TranslationToSql translation) { return; }
    }
}

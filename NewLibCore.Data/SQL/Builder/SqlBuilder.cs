﻿using System;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Translation;

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

        protected internal abstract TranslationCoreResult Build();
    }
}

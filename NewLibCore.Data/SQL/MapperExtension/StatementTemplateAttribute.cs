using System;

namespace NewLibCore.Data.SQL.MapperExtension
{
    [AttributeUsage(AttributeTargets.All, AllowMultiple = true, Inherited = true)]
    internal class StatementTemplateAttribute : Attribute
    {
        internal String Template { get; private set; }

        internal StatementTemplateAttribute(String template)
        {
            Template = template;
        }
    }
}

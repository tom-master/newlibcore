using System;
using System.Collections.Generic;
using System.Text;

namespace NewLibCore.Data.SQL.MapperExtension.EnumPropertyExtension
{
    [AttributeUsage(AttributeTargets.All, AllowMultiple = true, Inherited = true)]
    internal class SyntaxTemplateAttribute : Attribute
    {
        internal String Template { get; private set; }

        internal SyntaxTemplateAttribute(String template)
        {
            Template = template;
        }
    }
}

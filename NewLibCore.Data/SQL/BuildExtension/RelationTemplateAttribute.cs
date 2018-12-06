using System;
namespace NewLibCore.Data.SQL.BuildExtension
{
    [AttributeUsage(AttributeTargets.Property)]
    public class RelationTemplateAttribute : Attribute
    {

        public RelationTemplateAttribute(String template)
        {
            Template = template;
        }
        public String Template { get; private set; }
    }
}
using System;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 表达式处理基类
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal abstract class Handler
    {
        /// <summary>
        /// 初始化一个Handler类的实例
        /// </summary>
        protected internal Handler(IServiceProvider serviceProvider)
        {
            Parameter.Validate(serviceProvider);
            ServiceProvider = serviceProvider;
        }

        protected internal IServiceProvider ServiceProvider { get; private set; }

        /// <summary>
        /// SQL模板
        /// </summary>
        protected internal TemplateBase TemplateBase
        {
            get
            {
                return ServiceProvider.GetService<TemplateBase>();
            }
        }

        /// <summary>
        /// 执行表达式段的翻译
        /// </summary>
        /// <returns></returns>
        internal abstract RawResult Execute();
    }
}

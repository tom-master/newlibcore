using System;
using NewLibCore.Data.SQL.Mapper.EntityExtension;

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
        internal Handler(IServiceProvider serviceProvider)
        {
            ServiceProvider = serviceProvider;
        }

        protected internal IServiceProvider ServiceProvider { get; private set; }

        /// <summary>
        /// 执行表达式段的翻译
        /// </summary>
        /// <returns></returns>
        internal abstract RawResult Execute();
    }
}

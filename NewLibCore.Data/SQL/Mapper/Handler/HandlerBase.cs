using System;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.Template;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Handler
{
    /// <summary>
    /// 表达式处理基类
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal abstract class HandlerBase
    {
        private readonly Parser _parser;

        private readonly ParserResult _parserResult;

        /// <summary>
        /// 初始化一个Handler类的实例
        /// </summary>
        internal HandlerBase(IServiceProvider serviceProvider)
        {
            Parameter.Validate(serviceProvider);
            ServiceProvider = serviceProvider;
            _parserResult = ParserResult.CreateResult();
            _parser = Parser.CreateParser(serviceProvider);
        }

        protected IServiceProvider ServiceProvider { get; }

        /// <summary>
        /// SQL模板
        /// </summary>
        protected TemplateBase TemplateBase
        {
            get
            {
                return ServiceProvider.GetService<TemplateBase>();
            }
        }

        protected ParserResult ParserResult
        {
            get
            {
                return _parserResult;
            }
        }

        protected Parser Parser
        {
            get
            {
                return _parser;
            }
        }

        internal ExecuteResult Process()
        {
            return Execute();
        }

        /// <summary>
        /// 执行表达式段的翻译
        /// </summary>
        /// <returns></returns>
        protected abstract ExecuteResult Execute();

        
    }
}

using Microsoft.Extensions.Options;
using NewLibCore.Storage.SQL.EMapper;
using NewLibCore.Storage.SQL.EMapper.Parser;
using NewLibCore.Storage.SQL.Template;
using NewLibCore.Validate;
using System;

namespace NewLibCore.Storage.SQL.ProcessorFactory
{
    internal abstract class Processor
    {
        protected readonly TemplateBase TemplateBase;

        protected readonly EntityMapperOptions Options;

        protected readonly ConditionProcessor ConditionProcessor;

        internal virtual String CurrentId { get { return GetType().Name; } }

        protected Processor(ConditionProcessor conditionProcessor)
        {
            ConditionProcessor = conditionProcessor;
        }
        protected Processor(TemplateBase templateBase, ConditionProcessor conditionProcessor, IOptions<EntityMapperOptions> options)
        {
            Check.IfNullOrZero(conditionProcessor);
            Check.IfNullOrZero(templateBase);
            Check.IfNullOrZero(options);
            Options = options.Value;
            TemplateBase = templateBase;
            ConditionProcessor = conditionProcessor;
        }

        internal SqlExecuteResultConvert Process()
        {
            return Execute();
        }

        protected abstract SqlExecuteResultConvert Execute();
    }
}
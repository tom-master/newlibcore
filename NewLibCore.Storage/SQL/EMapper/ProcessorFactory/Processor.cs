using System;
using NewLibCore.Storage.SQL.EMapper;
using NewLibCore.Storage.SQL.EMapper.Parser;
using NewLibCore.Storage.SQL.Store;
using NewLibCore.Storage.SQL.Template;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.ProcessorFactory
{
    internal abstract class Processor
    {
        protected readonly TemplateBase TemplateBase;

        protected readonly EntityMapperOptions Options;

        protected readonly ConditionProcessor ConditionProcessor;

        internal virtual String CurrentId { get { return GetType().Name; } }

        protected Processor(ConditionProcessor conditionProcessor) : this(null, conditionProcessor, null)
        {

        }
        protected Processor(TemplateBase templateBase, ConditionProcessor conditionProcessor, EntityMapperOptions options)
        {
            Check.IfNullOrZero(conditionProcessor);

            Options = options;
            TemplateBase = templateBase;
            ConditionProcessor = conditionProcessor;
        }

        internal SqlExecuteResultConvert Process(ExpressionStore store)
        {
            Check.IfNullOrZero(store);
            return Execute(store);
        }

        protected abstract SqlExecuteResultConvert Execute(ExpressionStore store);
    }
}
using System;
using NewLibCore.Storage.SQL.EMapper.Parser;
using NewLibCore.Storage.SQL.Store;
using NewLibCore.Storage.SQL.Template;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.ProcessorFactory
{
    internal abstract class Processor
    {
        protected readonly TemplateBase _templateBase;

        protected readonly ConditionProcessor _conditionProcessor;

        internal virtual String CurrentId { get { return GetType().Name; } }

        protected Processor(ConditionProcessor conditionProcessor) : this(null, conditionProcessor)
        {

        }
        protected Processor(TemplateBase templateBase, ConditionProcessor conditionProcessor)
        {
            Check.IfNullOrZero(templateBase);
            Check.IfNullOrZero(conditionProcessor);

            _templateBase = templateBase;
            _conditionProcessor = conditionProcessor;
        }

        internal SqlExecuteResultConvert Process(ExpressionStore store)
        {
            Check.IfNullOrZero(store);
            return Execute(store);
        }

        protected abstract SqlExecuteResultConvert Execute(ExpressionStore store);
    }
}
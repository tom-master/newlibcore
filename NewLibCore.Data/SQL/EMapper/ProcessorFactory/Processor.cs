using System;
using NewLibCore.Data.SQL.EMapper.Parser;
using NewLibCore.Data.SQL.Store;
using NewLibCore.Data.SQL.Template;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.ProcessorFactory
{
    internal abstract class Processor
    {
        protected readonly TemplateBase _templateBase;

        protected readonly ExpressionProcessor _expressionProcessor;

        internal virtual String CurrentId { get { return GetType().Name; } }

        protected Processor(TemplateBase templateBase, ExpressionProcessor expressionProcessor)
        {
            Parameter.IfNullOrZero(templateBase);
            Parameter.IfNullOrZero(expressionProcessor);

            _templateBase = templateBase;
            _expressionProcessor = expressionProcessor;
        }

        internal ExecuteResult Process(ExpressionStore store)
        {
            Parameter.IfNullOrZero(store);
            return Execute(store);
        }

        protected abstract ExecuteResult Execute(ExpressionStore store);
    }
}
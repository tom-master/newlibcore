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

        protected readonly ExpressionProcessor _expressionProcessor;

        internal virtual String CurrentId { get { return GetType().Name; } }

        protected Processor(ExpressionProcessor expressionProcessor) : this(null, expressionProcessor)
        {

        }
        protected Processor(TemplateBase templateBase, ExpressionProcessor expressionProcessor)
        {
            Check.IfNullOrZero(templateBase);
            Check.IfNullOrZero(expressionProcessor);

            _templateBase = templateBase;
            _expressionProcessor = expressionProcessor;
        }

        internal SqlExecuteResultConvert Process(ExpressionStore store)
        {
            Check.IfNullOrZero(store);
            return Execute(store);
        }

        protected abstract SqlExecuteResultConvert Execute(ExpressionStore store);
    }
}
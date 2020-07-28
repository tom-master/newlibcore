using System;
using NewLibCore.Data.SQL.Store;
using NewLibCore.Data.SQL.Template;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.ProcessorFactory
{
    internal abstract class Processor
    {
        protected readonly TemplateBase _templateBase;

        protected readonly ParserExecutor _parserExecutor;

        internal virtual String CurrentId { get { return GetType().Name; } }

        protected Processor(TemplateBase templateBase, ParserExecutor parserExecutor)
        {
            Parameter.Validate(templateBase);
            Parameter.Validate(parserExecutor);

            _templateBase = templateBase;
            _parserExecutor = parserExecutor;
        }

        internal ExecuteResult Process(ExpressionStore store)
        {
            Parameter.Validate(store);
            return Execute(store);
        }

        protected abstract ExecuteResult Execute(ExpressionStore store);
    }
}
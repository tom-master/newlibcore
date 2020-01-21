using System;
using NewLibCore.Data.SQL.Mapper.Store;
using NewLibCore.Data.SQL.Mapper.Template;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Handler
{
    internal abstract class HandlerBase
    {
        protected readonly TemplateBase _templateBase;

        protected readonly ParserExecutor _parserExecutor;

        internal virtual String CurrentId { get { return GetType().Name; } }

        protected HandlerBase(TemplateBase templateBase, ParserExecutor parserExecutor)
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
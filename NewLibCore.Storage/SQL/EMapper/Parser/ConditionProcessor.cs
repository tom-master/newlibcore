using System;
using System.Collections.Generic;
using System.Linq;
using NewLibCore.Storage.SQL.Store;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.EMapper.Parser
{
    /// <summary>
    /// 将Expression解析为对应的SQL谓词
    /// </summary>
    internal abstract class ConditionProcessor
    {
        protected readonly ProcessorResult _processorResult;
        protected ExpressionStore _expressionStore;

        internal ConditionProcessor(ProcessorResult processorResult)
        {
            _processorResult = processorResult;
        }

        /// <summary>
        /// 翻译
        /// </summary>
        /// <returns></returns>
        protected abstract ProcessorResult InnerProcessor();

        internal ProcessorResult Processor(ParseModel parseModel)
        {
            Check.IfNullOrZero(parseModel);
            Check.IfNullOrZero(parseModel.Sql);
            _processorResult.Dispose();
            if (parseModel.Parameters != null)
            {
                _processorResult.Append(parseModel.Sql, parseModel.Parameters.ToArray());
            }
            else
            {
                _processorResult.Append(parseModel.Sql);
            }

            if (parseModel.ExpressionStore != null)
            {
                _expressionStore = parseModel.ExpressionStore;
                InnerProcessor();
            }

            return _processorResult;
        }
    }


    internal class ParseModel
    {
        internal String Sql { get; set; }

        internal IEnumerable<MapperParameter> Parameters { get; set; }

        internal ExpressionStore ExpressionStore { get; set; }
    }
}

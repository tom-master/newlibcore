using System;
using System.Collections.Generic;
using System.Linq;
using NewLibCore.Data.SQL.Store;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.EMapper.Parser
{
    /// <summary>
    /// 将Expression解析为对应的SQL谓词
    /// </summary>
    internal abstract class ExpressionProcessor
    {
        protected readonly ProcessorResult _processorResult;
        protected ExpressionStore _expressionStore;

        internal ExpressionProcessor(ProcessorResult processorResult)
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
            Parameter.IfNullOrZero(parseModel);
            Parameter.IfNullOrZero(parseModel.Sql);
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

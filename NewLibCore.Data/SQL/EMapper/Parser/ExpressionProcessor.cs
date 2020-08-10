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
        protected readonly ExpressionProcessorResult _expressionProcessorResult;
        protected ExpressionStore _expressionStore;

        internal ExpressionProcessor(ExpressionProcessorResult expressionProcessorResult)
        {
            _expressionProcessorResult = expressionProcessorResult;
        }

        /// <summary>
        /// 翻译
        /// </summary>
        /// <returns></returns>
        protected abstract ExpressionProcessorResult InnerProcessor();

        internal ExpressionProcessorResult Parse(ParseModel parseModel)
        {
            Parameter.IfNullOrZero(parseModel);
            Parameter.IfNullOrZero(parseModel.Sql);

            if (parseModel.Parameters != null)
            {
                _expressionProcessorResult.Append(parseModel.Sql, parseModel.Parameters.ToArray());
            }
            else
            {
                _expressionProcessorResult.Append(parseModel.Sql);
            }

            if (parseModel.ExpressionStore != null)
            {
                _expressionStore = parseModel.ExpressionStore;
                InnerProcessor();
            }

            return _expressionProcessorResult;
        }
    }


    internal class ParseModel
    {
        internal String Sql { get; set; }

        internal IEnumerable<MapperParameter> Parameters { get; set; }

        internal ExpressionStore ExpressionStore { get; set; }
    }
}

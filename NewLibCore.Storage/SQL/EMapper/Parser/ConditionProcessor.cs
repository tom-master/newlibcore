using System;
using System.Collections.Generic;
using System.Linq;
using NewLibCore.Storage.SQL.Component.Sql;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.EMapper.Parser
{
    /// <summary>
    /// 将Expression解析为对应的SQL谓词
    /// </summary>
    internal abstract class ConditionProcessor
    {
        protected readonly ProcessorResult _processorResult;
        protected SqlComponent _sqlComponent;

        internal ConditionProcessor(ProcessorResult processorResult)
        {
            _processorResult = processorResult;
        }

        /// <summary>
        /// 翻译
        /// </summary>
        /// <returns></returns>
        protected abstract ProcessorResult Process();

        internal ProcessorResult Process(ParseModel parseModel)
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

            if (parseModel.SqlComponent != null)
            {
                _sqlComponent = parseModel.SqlComponent;
                Process();
            }

            return _processorResult;
        }
    }


    internal class ParseModel
    {
        internal String Sql { get; set; }

        internal IEnumerable<MapperParameter> Parameters { get; set; }

        internal SqlComponent SqlComponent { get; set; }
    }
}

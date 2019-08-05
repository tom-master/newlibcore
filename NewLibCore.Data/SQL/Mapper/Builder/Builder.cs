using System;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Data.SQL.Mapper.Translation;

namespace NewLibCore.Data.SQL.Mapper.Builder
{
    internal abstract class Builder<TModel> where TModel : PropertyMonitor, new()
    {
        /// <summary>
        /// 获取表达式段翻译后的结果
        /// </summary>
        /// <returns></returns>
        internal TranslationResult GetSegmentResult()
        {
            return ExecuteSegmentTranslate();
        }

        /// <summary>
        /// 执行表达式段的翻译
        /// </summary>
        /// <returns></returns>
        protected abstract TranslationResult ExecuteSegmentTranslate();

        /// <summary>
        /// 字段转换
        /// </summary>
        /// <param name="statement"></param>
        /// <returns></returns>
        protected virtual (String Fields, String AliasName) StatementParse(Statement statement)
        {
            throw new NotImplementedException();
        }
    }
}

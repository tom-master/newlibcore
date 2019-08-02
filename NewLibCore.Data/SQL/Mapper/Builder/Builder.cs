using System;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Data.SQL.Mapper.Translation;

namespace NewLibCore.Data.SQL.Mapper.Builder
{
    internal abstract class Builder<TModel> where TModel : PropertyMonitor, new()
    {
        /// <summary>
        /// 创建并返回一个具体的构建对象
        /// </summary>
        /// <returns></returns>
        internal TranslateResult CreateResult()
        {
            return CreateTranslateResult();
        }

        /// <summary>
        /// 创建一个TranslationResult对象
        /// </summary>
        /// <returns></returns>
        protected abstract TranslateResult CreateTranslateResult();

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

using System;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Data.SQL.Mapper.Translation;

namespace NewLibCore.Data.SQL.Builder
{
    internal abstract class Builder<TModel> where TModel : PropertyMonitor, new()
    {
        /// <summary>
        /// 创建一个TranslationResult对象
        /// </summary>
        /// <returns></returns>
        internal abstract TranslateResult CreateTranslateResult();

        /// <summary>
        /// 字段转换
        /// </summary>
        /// <param name="statement"></param>
        /// <returns></returns>
        internal virtual (String fields, String tableName) StatementParse(Statement statement)
        {
            throw new NotImplementedException();
        }
    }
}

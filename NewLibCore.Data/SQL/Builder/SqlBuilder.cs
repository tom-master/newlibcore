using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.Translation;

namespace NewLibCore.Data.SQL.Builder
{
    internal interface IBuilder<TModel> where TModel : PropertyMonitor, new()
    {
        /// <summary>
        /// 创建一个TranslationResult对象
        /// </summary>
        /// <returns></returns>
        TranslateResult CreateTranslateResult();
    }
}

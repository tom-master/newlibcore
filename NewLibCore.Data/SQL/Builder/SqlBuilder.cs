using System;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.Translation;

namespace NewLibCore.Data.SQL.Builder
{
    internal interface IBuilder<TModel> where TModel : PropertyMonitor, new()
    {
        /// <summary>
        /// 构建一个翻译结果对象
        /// </summary>
        /// <returns></returns>
        TranslationResult Build();
    }
}

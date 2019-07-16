using System;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Translation;

namespace NewLibCore.Data.SQL.Builder
{
    internal interface IBuilder<TModel> where TModel : PropertyMonitor, new()
    {
        /// <summary>
        /// 构建一个翻译结果对象
        /// </summary>
        /// <returns></returns>
        TranslationCoreResult Build();
    }
}

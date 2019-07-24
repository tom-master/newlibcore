namespace NewLibCore.Data.SQL.Mapper.Translation
{
    /// <summary>
    /// 翻译表达式
    /// </summary>
    internal interface ITranslateExpression
    {
        /// <summary>
        /// 翻译
        /// </summary>
        /// <returns></returns>
        TranslateResult Translate();
    }
}

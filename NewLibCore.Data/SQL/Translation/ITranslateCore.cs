namespace NewLibCore.Data.SQL.Translation
{
    internal interface ITranslateCore
    {
        TranslationCoreResult Translate(StatementStore statementStore);
    }
}

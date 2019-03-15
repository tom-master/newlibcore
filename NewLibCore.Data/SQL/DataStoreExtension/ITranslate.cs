using NewLibCore.Data.SQL.DataStore;

namespace NewLibCore.Data.SQL.BuildExtension
{
    internal interface ITranslate
    {
        TranslationResult Translate(StatementStore statementStore);
    }
}

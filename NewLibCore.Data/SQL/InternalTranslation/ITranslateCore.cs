using NewLibCore.Data.SQL.InternalExecute;

namespace NewLibCore.Data.SQL.InternalTranslation
{
    internal interface ITranslateCore
    {
        TranslationCoreResult Translate(StatementStore statementStore);
    }
}

using NewLibCore.Data.SQL.InternalExecute;

namespace NewLibCore.Data.SQL.InternalTranslation
{
    internal interface ITranslate
    {
        TranslationResult Translate(StatementStore statementStore);
    }
}
 
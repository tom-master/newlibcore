using NewLibCore.Data.SQL.InternalDataStore;

namespace NewLibCore.Data.SQL.BuildExtension
{
    internal interface ITranslate
    {
        SqlTemporaryStore Translate(StatementStore statementStore);
    }
}

using NewLibCore.Data.SQL.InternalDataStore;

namespace NewLibCore.Data.SQL.BuildExtension
{
    internal interface ITranslate
    {
        FinalResultStore Translate(StatementStore statementStore);
    }
}

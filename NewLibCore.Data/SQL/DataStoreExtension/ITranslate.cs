using NewLibCore.Data.SQL.DataStore;

namespace NewLibCore.Data.SQL.BuildExtension
{
    internal interface ITranslate
    {
        FinalResultStore Translate(StatementStore statementStore);
    }
}

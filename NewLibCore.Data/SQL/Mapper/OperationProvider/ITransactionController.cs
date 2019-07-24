namespace NewLibCore.Data.SQL.Mapper.OperationProvider
{
    public interface ITransactionController
    {
        void OpenTransaction();

        void Commit();

        void Rollback();
    }
}

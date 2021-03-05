
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component.Sql
{

    internal class InsertComponent<TModel> where TModel : EntityBase, new()
    {
        internal EntityBase Model { get; set; }
        internal InsertComponent(TModel model)
        {
            Check.IfNullOrZero(model);
            Model = model;
        }
    }
}

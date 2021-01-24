
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component.Sql
{

    internal class InsertComponent
    {
        internal EntityBase Model { get; set; }
        internal void AddModel<TModel>(TModel model) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(model);
            Model = model;
        }
    }
}

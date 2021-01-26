using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component.Sql
{
    internal class UpdateComponent
    {
        internal EntityBase Model { get; set; }

        internal WhereComponent WhereComponent { get; private set; }

        internal FromComponent FromComponent { get; private set; }

        internal void AddModel<TModel>(TModel model) where TModel : EntityBase, new()
        {
            Check.IfNullOrZero(model);
            Model = model;
        }

        internal void AddWhereComponent(WhereComponent whereComponent)
        {
            Check.IfNullOrZero(whereComponent);
            WhereComponent = whereComponent;
        }

        internal void AddFromComponent(FromComponent fromComponent)
        {
            Check.IfNullOrZero(fromComponent);
            FromComponent = fromComponent;
        }
    }

}
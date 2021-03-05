using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component.Sql
{
    internal class UpdateComponent<TModel> where TModel : EntityBase, new()
    {
        internal EntityBase Model { get; set; }

        internal WhereComponent WhereComponent { get; private set; }

        internal FromComponent FromComponent { get; private set; }

        internal UpdateComponent(TModel model, WhereComponent whereComponent)
        {
            Check.IfNullOrZero(model);
            Check.IfNullOrZero(whereComponent);

            Model = model;
            WhereComponent = whereComponent;
        }

        internal void AddFromComponent(FromComponent fromComponent)
        {
            Check.IfNullOrZero(fromComponent);
            FromComponent = fromComponent;
        }
    }
}
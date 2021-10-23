
using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using NewLibCore.Storage.SQL.Extension;

namespace NewLibCore.Storage.SQL.Component
{
    internal class FromComponent : ComponentBase
    {
        // internal void AddFrom<TModel>() where TModel : EntityBase, new()
        // {
        //     var modelType = typeof(TModel);
        //     Expression<Func<TModel, TModel>> expression = (a) => a;

        //     var (tableName, aliasName) = modelType.GetEntityBaseAliasName();
        //     var KeyValuePair = new KeyValuePair<String, String>(tableName, aliasName);
        //     Expression = expression;
        //     InitAliasNameMappers(KeyValuePair);
        // }
    }
}
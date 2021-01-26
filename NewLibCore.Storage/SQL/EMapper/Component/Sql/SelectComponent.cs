using System;
using System.Linq.Expressions;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component.Sql
{

    internal class SelectComponent : ComponentBase
    {
        internal void AddSelect(Expression selector)
        {
            Check.IfNullOrZero(selector);
            Expression = selector;
        }
        // internal IList<KeyValuePair<String, String>> MergeComponentAlias()
        // {
        //     var newAliasMapper = new List<KeyValuePair<String, String>>();
        //     if (WhereComponent != null)
        //     {
        //         newAliasMapper.AddRange(WhereComponent.AliasNameMappers);
        //     }
        //     if (JoinComponent.JoinComponents.Any())
        //     {
        //         newAliasMapper.AddRange(JoinComponent.JoinComponents.SelectMany(s => s.AliasNameMappers));
        //     }
        //     if (FromComponent != null)
        //     {
        //         newAliasMapper.AddRange(FromComponent.AliasNameMappers);
        //     }
        //     newAliasMapper = newAliasMapper.Select(s => s).Distinct().ToList();

        //     var sameGroup = newAliasMapper.GroupBy(a => a.Value);
        //     foreach (var groupItem in sameGroup)
        //     {
        //         if (groupItem.Count() > 1)
        //         {
        //             throw new ArgumentException($@"表:{String.Join(",", groupItem.Select(s => s.Key))}指定了相同别名:{groupItem.Key}");
        //         }
        //     }

        //     return newAliasMapper;
        // }


        // internal IList<Type> GetParameterTypes()
        // {
        //     var types = new List<Type>();
        //     if (FromComponent != null)
        //     {
        //         var type = (FromComponent.Expression as LambdaExpression).Parameters[0].Type;
        //         types.Add(type);
        //     }

        //     if (JoinComponent.JoinComponents.Any())
        //     {
        //         foreach (var item in JoinComponent.JoinComponents)
        //         {
        //             foreach (var parameter in (item.Expression as LambdaExpression).Parameters)
        //             {
        //                 types.Add(parameter.Type);
        //             }
        //         }
        //     }
        //     return types.Distinct().ToList();
        // }
    }
}
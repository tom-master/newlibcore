using System;
using System.Collections.Generic;
using System.Linq;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.EMapper.Extension
{
    internal static class ChangedPropertyExtension
    {
        internal static SqlElements GetSqlElements(this IReadOnlyList<ChangedProperty> changedProperties)
        {
            return new SqlElements(changedProperties);
        }
    }

    internal class SqlElements
    {
        internal String Fields { get; private set; }

        internal String InsertPlaceHolders { get; private set; }

        internal String UpdatePlaceHolders { get; private set; }

        internal IEnumerable<MapperParameter> Parameters { get; private set; }

        internal SqlElements(IEnumerable<ChangedProperty> changedProperties)
        {
            Check.IfNullOrZero(changedProperties);

            Fields = String.Join(",", changedProperties.Select(c => c.PropertyName));
            InsertPlaceHolders = String.Join(",", changedProperties.Select(key => $@"@{key.PropertyName}"));
            UpdatePlaceHolders = String.Join(",", changedProperties.Select(c => $@"{c.PropertyName}=@{c.PropertyName}"));
            Parameters = changedProperties.Select(c => new MapperParameter(c.PropertyName, c.Value)).ToArray();
        }
    }
}
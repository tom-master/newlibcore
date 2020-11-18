using System;
using NewLibCore.Validate;

namespace NewLibCore.Storage.SQL.Component
{
    internal class PaginationComponent
    {

        internal Int32 Index { get; private set; }

        internal Int32 Size { get; private set; }

        internal Int32 MaxKey { get; private set; }

        internal void AddPagination(Int32 pageIndex, Int32 pageSize, Int32 maxKey = 0)
        {
            Check.IfNullOrZero(pageIndex);
            Check.IfNullOrZero(pageSize);

            Index = pageIndex;
            Size = pageSize;
            MaxKey = maxKey;
        }
    }
}
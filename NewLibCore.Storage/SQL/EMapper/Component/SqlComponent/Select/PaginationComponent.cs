using System;
using NewLibCore.Validate;
namespace NewLibCore.Storage.SQL.Component
{
    internal class PaginationComponent
    {

        internal int Index { get; private set; }

        internal int Size { get; private set; }

        internal int MaxKey { get; private set; }

        internal void AddPagination(int pageIndex, int pageSize, int maxKey = 0)
        {
            Check.IfNullOrZero(pageIndex);
            Check.IfNullOrZero(pageSize);

            Index = pageIndex;
            Size = pageSize;
            MaxKey = maxKey;
        }
    }
}
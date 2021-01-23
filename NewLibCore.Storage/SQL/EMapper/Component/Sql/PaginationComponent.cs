using System;
namespace NewLibCore.Storage.SQL.Component.Sql
{
    /// <summary>
    /// 分页语句对象
    /// </summary>
    internal class PaginationComponent : AliasComponentBase
    {
        internal Int32 Index { get; private set; }

        internal Int32 Size { get; private set; }

        internal Int32 MaxKey { get; private set; }

        protected internal PaginationComponent(Int32 index, Int32 size, Int32 maxKey) : base(null)
        {
            Index = index;
            Size = size;
            MaxKey = maxKey;
        }
    }
}
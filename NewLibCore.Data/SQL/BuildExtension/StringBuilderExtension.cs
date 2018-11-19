using System;
using System.Text;

namespace NewLibCore.Data.SQL.BuildExtension
{
    public static class StringBuilderExtension
    {
        public static void Append(this StringBuilder builder, String left, String opt, String right)
        {
            if (opt.ToUpper() == RelationType.IN.ToString())
            {
                builder.Append($@" FIND_IN_SET({left}, @{right})>0 ");
            }
            else if (opt.ToUpper() == RelationType.LIKE.ToString())
            {
                builder.Append($@" {left} {opt} CONCAT('%',@{right},'%') ");
            }
            else if (opt.ToUpper() == RelationType.START_LIKE.ToString())
            {
                builder.Append($@" {left} {RelationType.LIKE} CONCAT('',@{right},'%') ");
            }
            else if (opt.ToUpper() == RelationType.END_LIKE.ToString())
            {
                builder.Append($@" {left} {RelationType.LIKE} CONCAT('%',@{right},'') ");
            }
            else
            {
                builder.Append($@" {left} {opt} @{right} ");
            }
        }
    }
}
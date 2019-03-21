using System;
using System.Text;

namespace NewLibCore.Data.SQL.MapperExtension
{
    internal class MySqlSyntaxBuilder : DatabaseSyntaxBuilder
    {
        internal override String SyntaxBuilder(RelationType relationType, String left, String right)
        {
            Clear();

            var type = relationType;
            if (type == RelationType.IN)
            {
                Builder.Append($@"{left} IN (@{right}) ");
            }
            else if (type == RelationType.LIKE)
            {
                Builder.Append($@"{left} {RelationType.LIKE} CONCAT('%',@{right},'%') ");
            }
            else if (type == RelationType.START_LIKE)
            {
                Builder.Append($@"{left} {RelationType.LIKE} CONCAT('',@{right},'%') ");
            }
            else if (type == RelationType.END_LIKE)
            {
                Builder.Append($@"{left} {RelationType.LIKE} CONCAT('%',@{right},'') ");
            }
            else
            {
                SyntaxBuilderBase(type, left, right);
            }

            return Builder.ToString();
        }
    }

    internal class MsSqlSyntaxBuilder : DatabaseSyntaxBuilder
    {
        internal override String SyntaxBuilder(RelationType relationType, String left, String right)
        {
            Clear();

            if (relationType == RelationType.IN)
            {
                Builder.Append($@"{left} IN (@{right}) ");
            }
            else if (relationType == RelationType.LIKE)
            {
                Builder.Append($@"{left} {RelationType.LIKE} '%@{right}%'");
            }
            else if (relationType == RelationType.START_LIKE)
            {
                Builder.Append($@"{left} {RelationType.LIKE} '@{right}%' ");
            }
            else if (relationType == RelationType.END_LIKE)
            {
                Builder.Append($@"{left} {RelationType.LIKE} '@%{right}'  ");
            }
            else
            {
                SyntaxBuilderBase(relationType, left, right);
            }

            return Builder.ToString();
        }

    }

    public abstract class DatabaseSyntaxBuilder
    {
        protected StringBuilder Builder = new StringBuilder();

        internal String IdentitySuffix { get; set; }

        internal String RowCountSuffix { get; set; }

        internal String Page { get; set; }

        internal abstract String SyntaxBuilder(RelationType relationType, String left, String right);

        internal void SyntaxBuilderBase(RelationType relationType, String left, String right)
        {
            Builder.Append($@" {left} {relationType.GetDescription()} @{right} ");
        }

        protected virtual void Clear() { Builder.Clear(); }
    }
}
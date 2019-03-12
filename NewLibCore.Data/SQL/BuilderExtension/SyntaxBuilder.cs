using System;
using System.Text;

namespace NewLibCore.Data.SQL.BuildExtension
{
    internal class MysqlSyntaxBuilder : DatabaseSyntaxBuilder
    {
        internal override String SyntaxBuilder(RelationType relationType, String left, String right)
        {
            Clear();

            var type = relationType;
            if (type == RelationType.IN)
            {
                Builder.Append($@" FIND_IN_SET({left}, @{right})>0 ");
            }
            else if (type == RelationType.LIKE)
            {
                Builder.Append($@" {left} {RelationType.LIKE} CONCAT('%',@{right},'%') ");
            }
            else if (type == RelationType.START_LIKE)
            {
                Builder.Append($@" {left} {RelationType.LIKE} CONCAT('',@{right},'%') ");
            }
            else if (type == RelationType.END_LIKE)
            {
                Builder.Append($@" {left} {RelationType.LIKE} CONCAT('%',@{right},'') ");
            }

            SyntaxBuilderBase(type, left, right);

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
                Builder.Append($@" {left} IN(@{right}) ");
            }
            else if (relationType == RelationType.LIKE)
            {
                Builder.Append($@" {left} {RelationType.LIKE} '%@{right}%'");
            }
            else if (relationType == RelationType.START_LIKE)
            {
                Builder.Append($@" {left} {RelationType.LIKE} '@{right}%' ");
            }
            else if (relationType == RelationType.END_LIKE)
            {
                Builder.Append($@" {left} {RelationType.LIKE} '@%{right}'  ");
            }

            SyntaxBuilderBase(relationType, left, right);

            return Builder.ToString();
        }

    }

    public abstract class DatabaseSyntaxBuilder
    {
        protected StringBuilder Builder = new StringBuilder();

        internal abstract String SyntaxBuilder(RelationType relationType, String left, String right);

        internal void SyntaxBuilderBase(RelationType relationType, String left, String right)
        {
            Builder.Append($@" {left} {relationType.GetDescription()} @{right} ");
        }

        protected virtual void Clear() { Builder.Clear(); }
    }
}
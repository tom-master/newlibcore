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

        protected override void Clear()
        {
            Builder.Clear();
        }
    }

    internal class MsSqlSyntaxBuilder : DatabaseSyntaxBuilder
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
                Builder.Append($@" {left} {RelationType.LIKE} '%@{right}%'");
            }
            else if (type == RelationType.START_LIKE)
            {
                Builder.Append($@" {left} {RelationType.LIKE} '@{right}%' ");
            }
            else if (type == RelationType.END_LIKE)
            {
                Builder.Append($@" {left} {RelationType.LIKE} '@%{right}'  ");
            }

            SyntaxBuilderBase(type, left, right);

            return Builder.ToString();
        }

        protected override void Clear()
        {
            Builder.Clear();
        }
    }


    internal abstract class DatabaseSyntaxBuilder
    {
        protected StringBuilder Builder = new StringBuilder();

        internal abstract String SyntaxBuilder(RelationType relationType, String left, String right);

        protected void SyntaxBuilderBase(RelationType relationType, String left, String right)
        {
            if (relationType == RelationType.EQ)
            {
                Builder.Append($@" {left} = @{right} ");
            }
            else if (relationType == RelationType.NQ)
            {
                Builder.Append($@" {left} <> @{right} ");
            }
            else if (relationType == RelationType.GT)
            {
                Builder.Append($@" {left} > @{right} ");
            }
            else if (relationType == RelationType.LT)
            {
                Builder.Append($@" {left} < @{right} ");
            }
            else if (relationType == RelationType.GE)
            {
                Builder.Append($@" {left} >= @{right} ");
            }
            else if (relationType == RelationType.LE)
            {
                Builder.Append($@" {left} <= @{right} ");
            }
        }

        protected virtual void Clear() { throw new NotImplementedException(); }
    }


}
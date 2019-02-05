using System;
using System.Text;

namespace NewLibCore.Data.SQL.BuildExtension
{
    internal class MysqlSyntaxBuilder : DatabaseSyntaxBuilder
    {
        private StringBuilder _syntaxBuilder = new StringBuilder();

        internal override String SyntaxBuilder(RelationType relationType, String left, String right)
        {
            Clear();
            var type = relationType;
            if (type == RelationType.IN)
            {
                _syntaxBuilder.Append($@" FIND_IN_SET({left}, @{right})>0 ");
            }
            else if (type == RelationType.LIKE)
            {
                _syntaxBuilder.Append($@" {left} LIKE CONCAT('%',@{right},'%') ");
            }
            else if (type == RelationType.START_LIKE)
            {
                _syntaxBuilder.Append($@" {left} {RelationType.LIKE} CONCAT('',@{right},'%') ");
            }
            else if (type == RelationType.END_LIKE)
            {
                _syntaxBuilder.Append($@" {left} {RelationType.LIKE} CONCAT('%',@{right},'') ");
            }
            else if (type == RelationType.EQ)
            {
                _syntaxBuilder.Append($@" {left} = @{right} ");
            }
            else if (type == RelationType.NQ)
            {
                _syntaxBuilder.Append($@" {left} <> @{right} ");
            }
            else if (type == RelationType.GT)
            {
                _syntaxBuilder.Append($@" {left} > @{right} ");
            }
            else if (type == RelationType.LT)
            {
                _syntaxBuilder.Append($@" {left} < @{right} ");
            }
            else if (type == RelationType.GE)
            {
                _syntaxBuilder.Append($@" {left} >= @{right} ");
            }
            else if (type == RelationType.LE)
            {
                _syntaxBuilder.Append($@" {left} <= @{right} ");
            }
            return _syntaxBuilder.ToString();
        }

        internal override void Clear()
        {
            _syntaxBuilder.Clear();
        }
    }


    internal abstract class DatabaseSyntaxBuilder
    {
        internal abstract String SyntaxBuilder(RelationType relationType, String left, String right);

        internal virtual void Clear() { throw new NotImplementedException(); }
    }
}
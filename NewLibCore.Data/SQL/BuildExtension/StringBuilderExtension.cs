using System;
using System.Text;

namespace NewLibCore.Data.SQL.BuildExtension
{
    internal class MysqlSyntaxBuilder : DatabaseSyntaxBuilder
    {
        private StringBuilder _syntaxBuilder = new StringBuilder();
        internal override String SyntaxBuilder(String relationType, String left, String right)
        {
            var type = ParseRelationType(relationType);

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
            else
            {
                //_syntaxBuilder.Append($@" {left} {opt} @{right} ");
            }

            return _syntaxBuilder.ToString();
        }

        private RelationType ParseRelationType(String relation)
        {
            if (String.IsNullOrEmpty(relation))
            {
                throw new InvalidOperationException($@"无效的转换类型:{relation}");
            }

            RelationType _relation;

            if (Enum.TryParse(relation, out _relation))
            {
                return _relation;
            }

            throw new InvalidOperationException($@"无效的转换类型:{relation}");
        }
    }

    internal abstract class DatabaseSyntaxBuilder
    {
        internal abstract String SyntaxBuilder(String relationType, String left, String right);
    }
}
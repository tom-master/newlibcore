using NewLibCore.Data.SQL.InternalDataStore;
using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Text;

namespace NewLibCore.Data.SQL.BuildExtension
{
    internal abstract class BuilderBase
    {
        internal DatabaseSyntaxBuilder SyntaxBuilder = new MysqlSyntaxBuilder(true);

        internal StringBuilder _builder = new StringBuilder();

        internal Stack<String> _parameterNameStack = new Stack<String>();

        internal Stack<RelationType> _operationalCharacterStack = new Stack<RelationType>();

        internal IList<SqlParameterMapper> WhereParameters { get; private set; } = new List<SqlParameterMapper>();

        internal abstract void Translate(Expression expression);
    }
}

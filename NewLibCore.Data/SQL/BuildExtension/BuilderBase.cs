using System.Linq.Expressions;

namespace NewLibCore.Data.SQL.BuildExtension
{
    public abstract class BuilderBase
    {
        internal abstract void Translate(Expression expression);
    }
}

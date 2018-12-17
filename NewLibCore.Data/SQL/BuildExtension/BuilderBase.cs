using System.Linq.Expressions;

namespace NewLibCore.Data.SQL.BuildExtension
{
    public abstract class BuilderBase
    {
        public abstract void Translate(Expression expression);
    }
}

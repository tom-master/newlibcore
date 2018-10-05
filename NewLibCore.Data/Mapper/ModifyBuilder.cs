using NewLibCore.Data.Mapper.PropertyExtension;
using System;
using System.Linq;
using System.Linq.Expressions;

namespace NewLibCore.Data.Mapper
{
    internal class ModifyBuilder<TModel> : SqlBuilder<TModel> where TModel : PropertyMonitor, new()
    {
        private Expression<Func<TModel, Boolean>> _where;
        private Boolean _isValidate;
        private static readonly String _rowCount = " ; SELECT CAST(ROW_COUNT() AS SIGNED) AS c";

        public ModifyBuilder(TModel model, Expression<Func<TModel, Boolean>> where = null, Boolean isValidate = false) : base(model)
        {
            _where = where;
            _isValidate = isValidate;
        }

        protected internal override BuildEntry<TModel> Build()
        {
            if (!ModelInstance.Args.Any())
            {
                throw new ArgumentNullException("没有找到需要更新的字段");
            }

            var columns = ModelInstance.Args;
            if (_isValidate)
            {
                ValidateModel(columns.Select(s => s.PropertyInfo).ToList());
            }

            var buildEntry = new BuildEntry<TModel>(ModelInstance);
            buildEntry.AppendSqlPart($@"UPDATE {ModelType.Name} SET {String.Join(",", columns.Select(s => $@"{s.PropertyName}=@{s.PropertyName}"))},LastModifyTime='$@{DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")}'");

            if (_where != null)
            {
                var builderWhere = new BuilderWhere<TModel>();
                builderWhere.Where(_where);
                buildEntry.AppendSqlPart(builderWhere.ToString());
                buildEntry.ParameterMappers.AddRange(builderWhere.WhereParameters);
            }
            buildEntry.AppendSqlPart(_rowCount);

            buildEntry.AppendParameter(ModelInstance.Args.Select(s => s.PropertyInfo).ToList());

            return buildEntry;
        }
    }
}

using NewLibCore.Data.Mapper.BuildExtension;
using NewLibCore.Data.Mapper.InternalDataStore;
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
            var args = ModelInstance.Args;
            ModelInstance.SetUpdateTime();
            if (!args.Any())
            {
                throw new ArgumentNullException("没有找到需要更新的字段");
            }

            if (_isValidate)
            {
                ValidateModel(args.Select(s => s.PropertyInfo).ToList());
            }

            var buildEntry = new BuildEntry<TModel>(ModelInstance);
            buildEntry.Append($@"UPDATE {ModelType.Name} SET {String.Join(",", args.Select(s => $@"{s.PropertyName}=@{s.PropertyName}"))}");

            if (_where != null)
            {
                var builderWhere = new BuilderWhere<TModel>();
                builderWhere.Translate(_where);
                buildEntry.Append(builderWhere.ToString());
                buildEntry.ParameterMappers.AddRange(builderWhere.WhereParameters);
            }
            buildEntry.Append(_rowCount);
            buildEntry.AppendParameter(args.Select(s => s.PropertyInfo).ToList().Select(c => new ParameterMapper($@"@{c.Name}", c.GetValue(ModelInstance))));

            return buildEntry;
        }
    }
}

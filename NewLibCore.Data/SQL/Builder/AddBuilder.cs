using NewLibCore.Data.SQL.BuildExtension;
using NewLibCore.Data.SQL.InternalDataStore;
using NewLibCore.Data.SQL.MapperExtension;
using NewLibCore.Data.SQL.PropertyExtension;
using System;
using System.Linq;
using System.Reflection;

namespace NewLibCore.Data.SQL.Builder
{
    internal class AddBuilder<TModel> : BuilderBase<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly Boolean _isVerifyModel;

        private static readonly String _maxIdentity = " ; SELECT CAST(@@IDENTITY AS SIGNED) AS c ";

        internal AddBuilder(TModel model, Boolean isVerifyModel = false) : base(model)
        {
            _isVerifyModel = isVerifyModel;
        }

        protected internal override SqlTemporaryStore Build()
        {
            var sqlTemporary = new SqlTemporaryStore();
            var propertyInfos = ModelType.GetProperties().Where(w => w.GetCustomAttributes<PropertyValidate>().Any());

            if (!propertyInfos.Any())
            {
                throw new Exception($@"{ModelType.Name}:没有要插入的列");
            }

            if (_isVerifyModel)
            {
                ValidateModel(propertyInfos.ToList());
            }

            sqlTemporary.Append($@" INSERT {ModelType.Name} ({String.Join(",", propertyInfos.Select(c => c.Name))} ) VALUES ({String.Join(",", propertyInfos.Select(key => $@"@{key.Name}"))}) {_maxIdentity} ");

            sqlTemporary.AppendParameter(propertyInfos.ToList().Select(c => new SqlParameterMapper($@"@{c.Name}", c.GetValue(ModelInstance))).ToArray());

            return sqlTemporary;
        }
    }
}

using System;
using System.Linq;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{

    /// <summary>
    /// 新增操作builder类
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal class InsertHandler<TModel> : Handler<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly Boolean _isVerifyModel;
        private readonly TModel _instance;

        internal InsertHandler(TModel model, Boolean isVerifyModel = false)
        {
            Parameter.Validate(model);
            _isVerifyModel = isVerifyModel;
            _instance = model;
        }

        protected override RawExecuteResult ExecuteTranslate()
        {
            _instance.OnChanged();
            if (_isVerifyModel)
            {
                _instance.Validate();
            }

            var propertyInfos = _instance.GetChangedProperty();
            _instance.Reset();
            var template = String.Format(MapperConfig.Instance.AddTemplate, typeof(TModel).GetTableName().TableName, String.Join(",", propertyInfos.Select(c => c.Key)), String.Join(",", propertyInfos.Select(key => $@"@{key.Key}")), MapperConfig.Instance.Extension.Identity);
            var sqlResult = new SqlResult();
            sqlResult.Append(template, propertyInfos.Select(c => new EntityParameter(c.Key, c.Value)));
            return sqlResult.GetExecuteResult();
        }
    }
}

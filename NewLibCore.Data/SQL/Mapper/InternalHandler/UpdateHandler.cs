using System;
using System.Linq;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Data.SQL.Mapper.Translation;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.InternalHandler
{
    /// <summary>
    /// 更新操作builder类
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal class UpdateHandler<TModel> : Handler<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly TModel _instance;

        private readonly Boolean _isVerifyModel;

        private readonly SegmentManager _segmentManager;

        public UpdateHandler(TModel model, SegmentManager segmentManager, Boolean isVerifyModel = false)
        {
            Parameter.Validate(model);
            Parameter.Validate(segmentManager);

            _instance = model;
            _isVerifyModel = isVerifyModel;
            _segmentManager = segmentManager;
        }

        protected override TranslationResult ExecuteTranslate()
        {
            _instance.SetUpdateTime();

            if (_isVerifyModel)
            {
                _instance.Validate();
            }

            var (TableName, AliasName) = typeof(TModel).GetTableName();

            var propertys = _instance.GetChangedProperty();
            var translationSegment = new TranslationSegment(_segmentManager);
            translationSegment.TranslationResult.Append(String.Format(MapperConfig.DatabaseConfig.UpdateTemplate, TableName, AliasName, String.Join(",", propertys.Select(p => $@"{AliasName}.{p.Key}=@{p.Key}"))), propertys.Select(c => new EntityParameter(c.Key, c.Value)));
            if (_segmentManager.Where != null)
            {
                translationSegment.Translate();
            }
            _instance.Reset();

            translationSegment.TranslationResult.Append($@"{MapperConfig.DatabaseConfig.Extension.RowCount}");
            return translationSegment.TranslationResult;
        }
    }
}

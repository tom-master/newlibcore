using System;
using System.Linq;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 更新操作builder类
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal class UpdateHandler<TModel> : Handler<TModel> where TModel : EntityBase, new()
    {
        private readonly TModel _modelInstance;

        private readonly Boolean _verifyModel;

        private readonly SegmentManager _segmentManager;

        public UpdateHandler(TModel model, SegmentManager segmentManager, Boolean verifyModel = false)
        {
            Parameter.Validate(model);
            Parameter.Validate(segmentManager);

            _modelInstance = model;
            _verifyModel = verifyModel;
            _segmentManager = segmentManager;
        }

        protected override TranslationResult ExecuteTranslate()
        {
            _modelInstance.SetUpdateTime();

            if (_verifyModel)
            {
                _modelInstance.Validate();
            }

            var (TableName, AliasName) = typeof(TModel).GetTableName();

            var propertys = _modelInstance.GetChangedProperty();
            var segment = TranslationSegment.CreateTranslation(_segmentManager);
            segment.Result.Append(String.Format(Instance.UpdateTemplate, TableName, AliasName, String.Join(",", propertys.Select(p => $@"{AliasName}.{p.Key}=@{p.Key}"))), propertys.Select(c => new EntityParameter(c.Key, c.Value)));
            if (_segmentManager.Where != null)
            {
                segment.Translate();
            }
            _modelInstance.Reset();
            return segment.Result.Append($@"{Instance.Extension.RowCount}");
        }
    }
}

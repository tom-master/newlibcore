using System;
using System.Collections.Generic;
using System.Linq;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 更新处理类
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal class UpdateHandler<TModel> : Handler<TModel> where TModel : EntityBase, new()
    {
        private readonly TModel _modelInstance;
        private readonly Boolean _verifyModel;
        private readonly SegmentManager _segmentManager;

        /// <summary>
        /// 初始化一个UpdateHandler类的实例
        /// </summary>
        /// <param name="model">要更新的模型</param>
        /// <param name="segmentManager">表达式分解后的对象</param>
        /// <param name="verifyModel">是否验证模型</param>
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
            segment.Result.Append(ReplacePlaceholder(TableName, AliasName, propertys), CreateParameter(propertys));
            segment.Translate();
            segment.Result.Append($@"{RelationType.AND} {AliasName}.IsDeleted=0");
            _modelInstance.Reset();

            return segment.Result.Append($@"{Instance.Extension.RowCount}");
        }

        private static IEnumerable<EntityParameter> CreateParameter(IReadOnlyList<KeyValuePair<String, Object>> propertys)
        {
            return propertys.Select(c => new EntityParameter(c.Key, c.Value));
        }

        private String ReplacePlaceholder(String TableName, String AliasName, IReadOnlyList<KeyValuePair<String, Object>> propertys)
        {
            return String.Format(Instance.UpdateTemplate, TableName, AliasName, String.Join(",", propertys.Select(p => $@"{AliasName}.{p.Key}=@{p.Key}")));
        }
    }
}

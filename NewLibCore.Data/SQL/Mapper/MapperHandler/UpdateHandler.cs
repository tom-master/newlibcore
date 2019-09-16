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
    internal class UpdateHandler<TModel> : Handler where TModel : EntityBase, new()
    {
        private SegmentManager _segmentManager;

        private readonly TModel _modelInstance;

        /// <summary>
        /// 初始化一个UpdateHandler类的实例
        /// </summary>
        /// <param name="model">要更新的模型</param>
        public UpdateHandler(TModel model)
        {
            Parameter.Validate(model);
            _modelInstance = model;
        }

        internal override void AddSegmentManager(SegmentManager segmentManager)
        {
            Parameter.Validate(segmentManager);
            _segmentManager = segmentManager;
        }

        internal override RawExecuteResult Execute()
        {
            _modelInstance.SetUpdateTime();

            if (MapperConfig.EnableModelValidate)
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

            return segment.Result.Append($@"{Instance.Extension.RowCount}").Execute();
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

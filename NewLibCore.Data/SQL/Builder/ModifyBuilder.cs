using System;
using System.Linq;
using NewLibCore.Data.SQL.Mapper;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;
using NewLibCore.Data.SQL.Mapper.Translation;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Builder
{
    /// <summary>
    /// 更新操作builder类
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal class ModifyBuilder<TModel> : Builder<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly TModel _instance;

        private readonly Boolean _isVerifyModel;

        private readonly ExpressionSegment _expressionSegment;

        public ModifyBuilder(TModel model, ExpressionSegment expressionSegment, Boolean isVerifyModel = false)
        {
            Parameter.Validate(model);
            Parameter.Validate(expressionSegment);

            _instance = model;
            _isVerifyModel = isVerifyModel;
            _expressionSegment = expressionSegment;
        }

        /// <summary>
        /// 构建一个修改操作的翻译结果
        /// </summary>
        /// <returns></returns>
        internal override TranslateResult CreateTranslateResult()
        {
            _instance.SetUpdateTime();

            if (_isVerifyModel)
            {
                _instance.Validate();
            }
            var propertys = _instance.GetPropertys();
            var aliasName = typeof(TModel).GetTableName();

            var translation = new TranslateExpression(_expressionSegment);

            translation.Result.Append($@"UPDATE {aliasName} AS {aliasName} SET {String.Join(",", propertys.Select(p => $@"{aliasName}.{p.Key}=@{p.Key}"))} ", propertys.Select(c => new EntityParameter($@"@{c.Key}", c.Value)));
            if (_expressionSegment.Where != null)
            {
                translation.Translate();
            }
            _instance.Reset();
            translation.Result.Append($@"{MapperConfig.DatabaseConfig.Extension.RowCount}");
            return translation.Result;
        }
    }
}

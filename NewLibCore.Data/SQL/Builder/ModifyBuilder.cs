﻿using System;
using System.Linq;
using NewLibCore.Data.SQL.Mapper;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.Translation;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Builder
{
    /// <summary>
    /// 更新操作builder类
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal class ModifyBuilder<TModel> : IBuilder<TModel> where TModel : PropertyMonitor, new()
    {
        private readonly Boolean _isVerifyModel;

        private readonly StatementStore _statementStore;

        private readonly TModel _instance;

        public ModifyBuilder(TModel model, StatementStore statementStore, Boolean isVerifyModel = false)
        {
            Parameter.Validate(model);
            Parameter.Validate(statementStore);

            _instance = model;
            _isVerifyModel = isVerifyModel;
            _statementStore = statementStore;
        }

        /// <summary>
        /// 构建一个修改操作的翻译结果
        /// </summary>
        /// <returns></returns>
        public TranslateResult CreateTranslateResult()
        {
            _instance.SetUpdateTime();

            if (_isVerifyModel)
            {
                _instance.Validate();
            }

            var propertys = _instance.GetPropertys(); 
            var translation = new TranslateExpression(_statementStore);
            translation.Result.Append($@"UPDATE {typeof(TModel).GetAliasName()} SET {String.Join(",", propertys.Select(p => $@"{p.Key}=@{p.Key}"))}", propertys.Select(c => new EntityParameter($@"@{c.Key}", c.Value)));
            if (_statementStore.Where != null)
            {
                translation.Translate();
            }
            _instance.Reset();

            translation.Result.Append($@"{MapperConfig.GetInstance().DatabaseInstance.Extension.RowCount}");
            return translation.Result;
        }
    }
}

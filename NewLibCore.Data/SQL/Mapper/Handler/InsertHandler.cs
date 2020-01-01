﻿using System;
using System.Linq;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Handler
{
    /// <summary>
    /// 新增操作处理
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal class InsertHandler<TModel> : HandlerBase where TModel : EntityBase, new()
    {
        private readonly TModel _instance;

        /// <summary>
        /// 初始化一个InsertHandler类的实例
        /// </summary>
        /// <param name="model">要插入的模型</param>
        internal InsertHandler(TModel model, IServiceProvider serviceProvider) : base(serviceProvider)
        {
            Parameter.Validate(model);
            _instance = model;
        }

        /// <summary>
        /// 执行插入操作的翻译
        /// </summary>
        /// <returns></returns>
        protected override ExecuteResult Execute()
        {
            _instance.OnChanged();
            if (MapperConfig.EnableModelValidate)
            {
                _instance.Validate();
            }

            var propertys = _instance.GetChangedPropertys();
            if (!propertys.Any())
            {
                throw new Exception("没有获取到值发生变更的属性");
            }
            var insert = String.Format(Template.Insert, _instance.GetType().GetTableName().TableName, String.Join(",", propertys.Select(c => c.Key)), String.Join(",", propertys.Select(key => $@"@{key.Key}")), Template.Identity);
            return ParserResult.Append(insert, propertys.Select(c => new MapperParameter(c.Key, c.Value))).Execute();
        }
    }
}

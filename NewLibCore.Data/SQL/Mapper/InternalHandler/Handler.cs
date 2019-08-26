using System;
using Microsoft.Extensions.DependencyInjection;
using NewLibCore.Data.SQL.Mapper.Config;
using NewLibCore.Data.SQL.Mapper.EntityExtension;
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;

namespace NewLibCore.Data.SQL.Mapper
{
    internal abstract class Handler<TModel> where TModel : EntityBase, new()
    {
        internal Handler()
        {
            Instance = MapperConfig.ServiceProvider.GetService<InstanceConfig>();
        }

        protected InstanceConfig Instance { get; private set; }

        /// <summary>
        /// 获取表达式段翻译后的结果
        /// </summary>
        /// <returns></returns>
        internal TranslationResult GetTranslationResult()
        {
            return ExecuteTranslate();
        }

        /// <summary>
        /// 执行表达式段的翻译
        /// </summary>
        /// <returns></returns>
        protected abstract TranslationResult ExecuteTranslate();

        /// <summary>
        /// 字段转换
        /// </summary>
        /// <param name="statement"></param>
        /// <returns></returns>
        protected virtual (String Fields, String AliasName) StatementParse(Statement statement)
        {
            throw new NotImplementedException();
        }
    }
}

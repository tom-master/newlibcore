using System;
using Microsoft.Extensions.DependencyInjection; 
using NewLibCore.Data.SQL.Mapper.ExpressionStatment;

namespace NewLibCore.Data.SQL.Mapper
{
    /// <summary>
    /// 表达式处理基类
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    internal abstract class Handler
    {
        /// <summary>
        /// 初始化一个Handler类的实例
        /// </summary>
        internal Handler()
        {
            Instance = MapperConfig.ServiceProvider.GetService<InstanceConfig>();
        }

        /// <summary>
        /// 数据库配置实例
        /// </summary>
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
        /// <param name="statement">表达式拆分后的语句对象</param>
        /// <returns></returns>
        protected virtual (String Fields, String AliasName) StatementParse(Statement statement)
        {
            throw new NotImplementedException();
        }
    }
}

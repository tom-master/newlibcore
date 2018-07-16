using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Reflection;
using NewLibCore.InternalExtension;

namespace NewLibCore.Validate
{

	public class Parameter
	{

	}

	public static class ParameterExtension
	{
		#region 公共方法
		/// <summary>
		/// 验证引用类型是否合法
		/// </summary>
		public static Parameter Validate(this Parameter parameter, Object vaildateParameter, Boolean canNull = false)
		{
			if (!canNull && vaildateParameter == null)
			{
				throw ThrowComponentException($"参数 {nameof(vaildateParameter)} 为空引发异常。");
			}

			if (vaildateParameter is String)
			{
				return parameter;
			}
			if (vaildateParameter.GetType().IsAssignableToGenericType(typeof(IEnumerable<>)))
			{
				return parameter;
			}

			#region 判断如果需要验证的参数是复杂类型的话 获取应用在类型的属性上的特性，并判断

			//判断如果需要验证的参数是复杂类型的话 获取应用在类型的属性上的特性，并判断
			var instance = vaildateParameter;
			var propertys = instance.GetType().GetProperties();

			foreach (var propertyInfo in propertys)
			{
				var propertyAttributes = propertyInfo.GetCustomAttributes().ToArray();
				if (!propertyAttributes.Any())
				{
					continue;
				}

				foreach (var attribute in propertyAttributes)
				{
					var value = propertyInfo.GetValue(instance);
					if (attribute.GetType() == typeof(RequiredAttribute))
					{
						if (!((RequiredAttribute)attribute).AllowEmptyStrings)
						{
							if ((value + "").Length <= 0)
							{
								var internalField = propertyInfo.Name;
								throw new ArgumentNullException($"字段:{internalField}值不能为空");
							}
						}
					}

					if (attribute.GetType() == typeof(StringLengthAttribute))
					{
						var contentLength = ((StringLengthAttribute)attribute).MaximumLength;
						if ((value + "").Length > contentLength)
						{
							var internalField = propertyInfo.Name;
							throw new ArgumentNullException($"字段:{internalField}值长度不能超过:{contentLength}");
						}
					}
				}
			}

			#endregion

			return parameter;
		}

		/// <summary>
		/// 验证值类型是否合法
		/// </summary>
		/// <param name="parameter"></param>
		/// <param name="canZero"></param>
		public static Parameter Validate(this Parameter parameter, ValueType valueType, Boolean canZero = false)
		{
			Type type = valueType.GetType();

			if (type.IsValueType && type.IsNumeric())
			{
				Boolean flag = !canZero ? valueType.CastTo(0.0) <= 0.0 : valueType.CastTo(0.0) < 0.0;

				if (flag)
				{
					throw ThrowComponentException($"参数 {valueType.GetType().Name} 不在有效范围内引发异常。具体信息请查看系统日志。", new ArgumentNullException(parameter.GetType().Name));
				}
			}

			return parameter;
		}

		/// <summary>
		///     向调用层抛出组件异常
		/// </summary>
		/// <param name="msg"> 自定义异常消息 </param>
		/// <param name="e"> 实际引发异常的异常实例 </param>
		private static ArgumentNullException ThrowComponentException(String msg, Exception e = default(Exception))
		{
			if (String.IsNullOrEmpty(msg) && e != null)
			{
				msg = e.Message;
			}
			else if (String.IsNullOrEmpty(msg))
			{
				msg = "未知异常，详情请查看日志信息。";
			}

			return e == null ? new ArgumentNullException($"组件异常：{msg}") : new ArgumentNullException($"组件异常：{msg}", e);
		}

		#endregion
	}
}

using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using NewLibCore.Data.Mapper.MapperExtension;

namespace NewLibCore.Data.Mapper
{
	internal abstract class SqlBuilder<TModel> where TModel : class, new()
	{
		private StringBuilder _sqlBuilder = new StringBuilder();

		protected Type ModelType { get; }

		protected TModel ModelInstance { get; }

		protected SqlBuilder(TModel model)
		{
			if (model == null)
			{
				ModelInstance = Activator.CreateInstance<TModel>();
				ModelType = typeof(TModel);
			}
			else
			{
				ModelInstance = model;
				ModelType = model.GetType();
			}

		}

		protected internal abstract BuildEntry<TModel> Build();

		protected void ValidateModel(IList<PropertyInfo> propertyInfos)
		{
			var propertys = propertyInfos;
			foreach (var propertyItem in propertys)
			{
				if (propertyItem.CustomAttributes.Count() == 0)
				{
					continue;
				}

				var validateBases = GetValidateAttributes(propertyItem);
				foreach (var validateItem in validateBases)
				{
					VerifyPropertyValue(propertyItem, validateItem, propertyItem.GetValue(ModelInstance));

					if (validateItem is PropertyDefaultValueAttribute defaultValueAttribute)
					{
						Object value = null;
						var propertyInstanceValue = propertyItem.GetValue(ModelInstance);
						if (String.IsNullOrEmpty(propertyInstanceValue + "") || (propertyInstanceValue.GetType() == typeof(DateTime) && (DateTime)propertyInstanceValue == default(DateTime)))
						{
							value = defaultValueAttribute.Value;
						}
						else
						{
							value = propertyInstanceValue;
						}
						propertyItem.SetValue(ModelInstance, value);
					}
				}
			}
		}

		#region private

		private IList<ValidateBase> GetValidateAttributes(PropertyInfo propertyInfo)
		{
			var validateAttributes = propertyInfo.GetCustomAttributes<ValidateBase>(true);

			if (validateAttributes.GroupBy(g => g.Order).Where(w => w.Count() > 1).Any())
			{
				throw new Exception($@"{propertyInfo.Name} 中使用了多个优先级相同的特性");
			}

			return validateAttributes.OrderByDescending(o => o.Order).ToList();
		}

		private void VerifyPropertyValue(PropertyInfo propertyInfo, ValidateBase validate, Object value)
		{
			if (!validate.IsValidate(value))
			{
				throw new Exception(validate.FailReason($@"{propertyInfo.DeclaringType.FullName}.{propertyInfo.Name}"));
			}
		}

		#endregion
	}
}

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

		protected void ValidateModel()
		{
			var propertys = GetProperty();
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

					var defaultValueAttribute = validateItem as PropertyDefaultValueAttribute;
					if (defaultValueAttribute != null)
					{
						var value = defaultValueAttribute.Value;
						if (defaultValueAttribute.Value != propertyItem.GetValue(ModelInstance))
						{
							if (defaultValueAttribute.Type == typeof(DateTime))
							{
								value = defaultValueAttribute.Value;
							}
							else
							{
								value = propertyItem.GetValue(ModelInstance) ?? value;
							}
						}

						propertyItem.SetValue(ModelInstance, value);
					}
				}
			}
		}

		protected abstract IList<PropertyInfo> GetProperty();

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

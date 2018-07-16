using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Linq;
using System.Reflection;
using System.Text;
using NewLib.Data.Mapper.MapperExtension;

namespace NewLib.Data.Mapper
{
	public abstract class SqlBuilder<TModel> where TModel : class, new()
	{
		private StringBuilder _sqlBuilder = new StringBuilder();
		private Type _modelType;
		private Stack<String> _parameterStack;

		private TModel _modelInstance;
		private IList<SqlParameter> _parameters;

		protected SqlBuilder(TModel model)
		{
			if (model == null)
			{
				throw new ArgumentException($@"{nameof(model)} is null");
			}

			_modelType = model.GetType();
			_modelInstance = model;

			_parameterStack = new Stack<String>();
			_parameters = new List<SqlParameter>();
		}

		#region property
		protected StringBuilder Builder
		{
			get { return _sqlBuilder; }
		}

		protected Type ModelType
		{
			get { return _modelType; }
		}

		public TModel ModelInstance
		{
			get { return _modelInstance; }
		}

		protected Stack<String> ParameterStack
		{
			get { return _parameterStack; }
		}

		protected IList<SqlParameter> Parameters
		{
			get { return _parameters; }
		}

		#endregion

		public abstract String ParseToSql();

		public override String ToString()
		{
			return Builder.ToString();
		}

		public IList<SqlParameter> GetParameters()
		{
			return Parameters;
		}

		#region protected

		protected abstract Object ParseValue(Object value);

		protected virtual IEnumerable<String> GetFields()
		{
			foreach (var item in ModelType.GetProperties(BindingFlags.Instance | BindingFlags.Public).Where(w => w.PropertyType.Name != "IList`1" && w.CustomAttributes.Count() != 0))
			{
				yield return item.Name;
			}
		}

		protected void And()
		{
			Builder.Append("AND");
		}

		protected void Or()
		{
			Builder.Append("OR");
		}

		protected void Append(String appendString)
		{
			if (String.IsNullOrEmpty(appendString))
			{
				return;
			}

			Builder.Append(appendString);
		}

		protected void ResetBuilder()
		{
			_sqlBuilder = _sqlBuilder.Clear();
		}

		protected void ResetParameters()
		{
			Parameters.Clear();
		}



		#endregion
		protected void VerifyModel()
		{
			var propertys = ModelType.GetProperties(BindingFlags.Instance | BindingFlags.Public);
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

		#region private

		private IList<ValidateBase> GetValidateAttributes(PropertyInfo propertyInfo)
		{
			var validateAttributes = propertyInfo.GetCustomAttributes<ValidateBase>(true);

			if (VerifyTheAttributeIsDuplicate(validateAttributes))
			{
				throw new Exception($@"{propertyInfo.Name} 中使用了多个优先级相同的特性");
			}

			return validateAttributes.OrderByDescending(o => o.Order).ToList();
		}

		private Boolean VerifyTheAttributeIsDuplicate(IEnumerable<ValidateBase> validates)
		{
			return validates.GroupBy(g => g.Order).Where(w => w.Count() > 1).Any();
		}

		private void VerifyPropertyValue(PropertyInfo propertyInfo, ValidateBase validate, Object value)
		{
			if (!validate.IsValidate(value))
			{
				var reason = validate.FailReason($@"{propertyInfo.DeclaringType.FullName}.{propertyInfo.Name}");
				throw new Exception(reason);
			}
		}

		#endregion
	}
}

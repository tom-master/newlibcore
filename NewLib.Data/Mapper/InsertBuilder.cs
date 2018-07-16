using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace NewLib.Data.Mapper
{

	public class InsertBuilder<TModel>: SqlBuilder<TModel> where TModel : class, new()
	{
		private Boolean _isVerifyModel;

		public InsertBuilder(TModel model, Boolean isVerifyModel = false) : base(model)
		{
			if (model == null)
			{
				throw new ArgumentException($@"{nameof(model)} is null");
			}

			_isVerifyModel = isVerifyModel;
		}

		public override String ParseToSql()
		{
			ResetBuilder();
			ResetParameters();

			if (_isVerifyModel)
			{
				VerifyModel();
			}

			Append($@" INSERT {ModelType.Name} ( {String.Join(",", GetFields())} )  VALUES ({String.Join(",", GetFields().Select(key => $@"@{key}"))})");

			foreach (var item in ModelInstance.GetType().GetProperties(BindingFlags.Instance | BindingFlags.Public).Where(w => w.PropertyType.Name != "IList`1" && w.CustomAttributes.Count() != 0))
			{
				Parameters.Add(new SqlParameter($@"@{item.Name}", ParseValue(item.GetValue(ModelInstance))));
			}

			return ToString();
		}

		protected override Object ParseValue(Object value)
		{
			if (value == null)
			{
				throw new ArgumentException($@"{nameof(value)} is null");
			}

			var objType = value.GetType();
			if (objType == typeof(Boolean))
			{
				return ((Boolean)value) ? 1 : 0;
			}
			else if (objType.BaseType == typeof(Enum))
			{
				return (Int32)value;
			}

			return value;
		}
	}
}

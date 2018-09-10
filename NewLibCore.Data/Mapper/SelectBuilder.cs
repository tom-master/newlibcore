using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using NewLibCore.Data.Mapper.PropertyExtension;

namespace NewLibCore.Data.Mapper
{
	internal class SelectBuilder<TModel> : SqlBuilder<TModel> where TModel : PropertyMonitor, new()
	{
		private Expression _where;

		private Expression<Func<TModel, Object>> _selectField;

		internal SelectBuilder(TModel model, Expression where, Expression<Func<TModel, Object>> fields) : base(model)
		{
			_where = where;
			_selectField = fields;
		}

		protected internal override BuildEntry<TModel> Build()
		{
			var buildEntry = new BuildEntry<TModel>(ModelInstance);
			var aliasName = typeof(TModel).Name.ToLower();
			buildEntry.AppendSqlPart($@" SELECT {String.Join(",", GetFields().Select(s => $@"{aliasName}.{s}"))} FROM {typeof(TModel).Name} AS {aliasName}");
			if (_where != null)
			{
				var builderWhere = new BuilderWhere<TModel>(aliasName);
				builderWhere.Where(_where);

				buildEntry.AppendSqlPart(builderWhere.ToString());
				buildEntry.ParameterMappers.AddRange(builderWhere.WhereParameters);
			}
			return buildEntry;
		}

		private String[] GetFields()
		{
			var exp = (LambdaExpression)_selectField;
			var newExp = (NewExpression)exp.Body;
			var a = newExp.Members.Select(s => s.Name).ToArray();
			return a;
		}
	}
}

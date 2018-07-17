using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Text;

namespace NewLibCore.Data.Mapper
{
	internal class SelectBuilder<TModel> : SqlBuilder<TModel> where TModel : class, new()
	{
		private StringBuilder _leftJoin;

		private StringBuilder _rightJoin;

		private StringBuilder _innerJoin;


		protected internal override BuildEntry<TModel> Build()
		{
			return null;
		}

		public SelectBuilder(TModel model) : base(model) { }

		public SelectBuilder<TModel> LeftJoin<SlaveModel>(Expression<Func<SlaveModel>> leftExpression) where SlaveModel : class, new()
		{
			return this;
		}

		public SelectBuilder<TModel> RightJoin<SlaveModel>(Expression<Func<SlaveModel>> rightExpression) where SlaveModel : class, new()
		{
			return this;
		}

		public SelectBuilder<TModel> InnerJoin<SlaveModel>(Expression<Func<SlaveModel>> innerExpression) where SlaveModel : class, new()
		{
			return this;
		}
	}
}

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

		
	}
}

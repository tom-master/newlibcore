using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using NewLibCore.Data.SQL.Mapper.Extension;
using NewLibCore.Data.SQL.Mapper.Store;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.Mapper.Handler
{

	public class QueryWrapper<TModel> where TModel : EntityBase, new()
	{
		private readonly ExpressionStore _store;

		private readonly HandlerBase _handlerBase;

		internal QueryWrapper(ExpressionStore store, HandlerBase handlerBase)
		{
			Parameter.Validate(store);
			Parameter.Validate(handlerBase);

			_store = store;
			_handlerBase = handlerBase;
		}

		public QueryWrapper<TModel> Query()
		{
			_store.AddFrom<TModel>();
			return this;
		}

		public QueryWrapper<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> join)
		where TRight : EntityBase, new()
		{
			Parameter.Validate(join);
			_store.AddJoin(join, JoinRelation.LEFT);
			return this;
		}

		public QueryWrapper<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
		where TLeft : EntityBase, new()
		where TRight : EntityBase, new()
		{
			Parameter.Validate(join);
			_store.AddJoin(join, JoinRelation.LEFT);

			return this;
		}

		public QueryWrapper<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> join)
		where TRight : EntityBase, new()
		{
			Parameter.Validate(join);
			_store.AddJoin(join, JoinRelation.LEFT);
			return this;
		}

		public QueryWrapper<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
		where TLeft : EntityBase, new()
		where TRight : EntityBase, new()
		{
			Parameter.Validate(join);
			_store.AddJoin(join, JoinRelation.RIGHT);

			return this;
		}

		public QueryWrapper<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> join)
		where TRight : EntityBase, new()
		{
			Parameter.Validate(join);
			_store.AddJoin(join, JoinRelation.INNER);
			return this;
		}

		public QueryWrapper<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
		where TLeft : EntityBase, new()
		where TRight : EntityBase, new()
		{
			Parameter.Validate(join);
			_store.AddJoin(join, JoinRelation.INNER);

			return this;
		}

		public QueryWrapper<TModel> Page(Int32 pageIndex, Int32 pageSize, Int32 maxKey = 0)
		{
			Parameter.Validate(pageIndex);
			Parameter.Validate(pageSize);

			_store.AddPage(pageIndex, pageSize, maxKey);
			return this;
		}

		public QueryWrapper<TModel> Select(Expression<Func<TModel, dynamic>> selector = null)
		{
			if (selector != null)
			{
				_store.AddSelect(selector);
			}

			return this;
		}

		public QueryWrapper<TModel> Select<TModel1>(Expression<Func<TModel, TModel1, dynamic>> selector = null)
		where TModel1 : EntityBase, new()
		{
			if (selector != null)
			{
				_store.AddSelect(selector);
			}
			return this;
		}

		public QueryWrapper<TModel> Select<TModel1, TModel2>(Expression<Func<TModel1, TModel2, dynamic>> selector = null)
		where TModel1 : EntityBase, new()
		where TModel2 : EntityBase, new()
		{
			if (selector != null)
			{
				_store.AddSelect(selector);
			}
			return this;
		}
		public QueryWrapper<TModel> Select<TModel1, TModel2, TModel3>(Expression<Func<TModel1, TModel2, TModel3, dynamic>> selector = null)
		where TModel1 : EntityBase, new()
		where TModel2 : EntityBase, new()
		where TModel3 : EntityBase, new()
		{
			if (selector != null)
			{
				_store.AddSelect(selector);
			}
			return this;
		}

		public QueryWrapper<TModel> Select<TModel1, TModel2, TModel3, TModel4>(Expression<Func<TModel1, TModel2, TModel3, TModel4, dynamic>> selector = null)
		where TModel1 : EntityBase, new()
		where TModel2 : EntityBase, new()
		where TModel3 : EntityBase, new()
		where TModel4 : EntityBase, new()
		{
			if (selector != null)
			{
				_store.AddSelect(selector);
			}
			return this;
		}

		public QueryWrapper<TModel> Select<TModel1, TModel2, TModel3, TModel4, TModel5>(Expression<Func<TModel1, TModel2, TModel3, TModel4, TModel5, dynamic>> selector = null)
		where TModel1 : EntityBase, new()
		where TModel2 : EntityBase, new()
		where TModel3 : EntityBase, new()
		where TModel4 : EntityBase, new()
		where TModel5 : EntityBase, new()
		{
			if (selector != null)
			{
				_store.AddSelect(selector);
			}
			return this;
		}

		public QueryWrapper<TModel> Where(Expression<Func<TModel, Boolean>> filter)
		{
			Parameter.Validate(filter);
			_store.AddWhere(filter);
			return this;
		}

		public QueryWrapper<TModel> Where<TModel1>(Expression<Func<TModel1, Boolean>> filter)
		where TModel1 : EntityBase, new()
		{
			Parameter.Validate(filter);
			_store.AddWhere(filter);
			return this;
		}

		public QueryWrapper<TModel> Where<TModel1>(Expression<Func<TModel, TModel1, Boolean>> filter)
		where TModel1 : EntityBase, new()
		{
			Parameter.Validate(filter);
			_store.AddWhere(filter);
			return this;
		}

		public QueryWrapper<TModel> Where<TModel1, TModel2>(Expression<Func<TModel, TModel1, TModel2, Boolean>> filter)
		where TModel1 : EntityBase, new()
		where TModel2 : EntityBase, new()
		{
			Parameter.Validate(filter);
			_store.AddWhere(filter);
			return this;
		}


		public QueryWrapper<TModel> Where<TModel1, TModel2, TModel3>(Expression<Func<TModel, TModel1, TModel2, TModel3, Boolean>> filter)
		where TModel1 : EntityBase, new()
		where TModel2 : EntityBase, new()
		where TModel3 : EntityBase, new()
		{
			Parameter.Validate(filter);
			_store.AddWhere(filter);
			return this;
		}

		public QueryWrapper<TModel> Where<TModel1, TModel2, TModel3, TModel4>(Expression<Func<TModel, TModel1, TModel2, TModel3, TModel4, Boolean>> filter)
		where TModel1 : EntityBase, new()
		where TModel2 : EntityBase, new()
		where TModel3 : EntityBase, new()
		where TModel4 : EntityBase, new()
		{
			Parameter.Validate(filter);
			_store.AddWhere(filter);
			return this;
		}


		public QueryWrapper<TModel> Where<TModel1, TModel2, TModel3, TModel4, TModel5>(Expression<Func<TModel, TModel1, TModel2, TModel3, TModel4, TModel5, Boolean>> filter)
		where TModel1 : EntityBase, new()
		where TModel2 : EntityBase, new()
		where TModel3 : EntityBase, new()
		where TModel4 : EntityBase, new()
		where TModel5 : EntityBase, new()
		{
			Parameter.Validate(filter);
			_store.AddWhere(filter);
			return this;
		}

		public QueryWrapper<TModel> ThenByDesc<TKey>(Expression<Func<TModel, TKey>> order)
		{
			Parameter.Validate(order);
			_store.AddOrderBy(order, OrderByType.DESC);
			return this;
		}

		public QueryWrapper<TModel> ThenByAsc<TKey>(Expression<Func<TModel, TKey>> order)
		{
			Parameter.Validate(order);
			_store.AddOrderBy(order, OrderByType.ASC);
			return this;
		}

		public QueryWrapper<TModel> Include<TModel1>(Expression<Func<TModel, TModel1>> include)
		where TModel1 : EntityBase, new()
		{
			Parameter.Validate(include);
			_store.AddInclude(include);
			return this;
		}

		public TModel FirstOrDefault()
		{
			return RunDiagnosis.Watch(() =>
			{
				return _handlerBase.Process(_store).FirstOrDefault<TModel>();
			});
		}

		public TResult FirstOrDefault<TResult>() where TResult : new()
		{
			return RunDiagnosis.Watch(() =>
			{
				return _handlerBase.Process(_store).FirstOrDefault<TResult>();
			});
		}

		public List<TModel> ToList()
		{
			return RunDiagnosis.Watch(() =>
			{
				return _handlerBase.Process(_store).ToList<TModel>();
			});
		}

		public List<TResult> ToList<TResult>() where TResult : new()
		{
			return RunDiagnosis.Watch(() =>
			{
				return _handlerBase.Process(_store).ToList<TResult>();
			});
		}

		public Int32 Count()
		{
			return RunDiagnosis.Watch(() =>
			{
				Select((a) => "COUNT(*)");
				return _handlerBase.Process(_store).FirstOrDefault<Int32>();
			});
		}
	}
}

using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using NewLibCore.Data.SQL.EMapper.Parser;
using NewLibCore.Data.SQL.Extension;
using NewLibCore.Data.SQL.Store;
using NewLibCore.Data.SQL.Template;
using NewLibCore.Data.SQL.Validate;
using NewLibCore.Validate;

namespace NewLibCore.Data.SQL.ProcessorFactory
{
    /// <summary>
    /// 查询处理类
    /// </summary>
    internal class QueryProcessor : Processor
    {

        public QueryProcessor(TemplateBase templateBase, ExpressionProcessor expressionProcessor) : base(templateBase, expressionProcessor)
        {
        }

        /// <summary>
        /// 执行查询操作的翻译
        /// </summary>
        /// <returns></returns>
        protected override ResultConvert Execute(ExpressionStore store)
        {
            Parameter.IfNullOrZero(store);
            if (!store.From.AliaNameMapper.Any())
            {
                throw new ArgumentException("没有指定From表");
            }

            var mainTable = store.From.AliaNameMapper[0];
            var result = _expressionProcessor.Processor(new ParseModel
            {
                Sql = _templateBase.CreateSelect(ExtractSelectFields(store), mainTable.Key, mainTable.Value),
                ExpressionStore = store
            });

            var aliasMapper = store.MergeAliasMapper();
            foreach (var aliasItem in aliasMapper)
            {
                result.Append($@"{PredicateType.AND} {aliasItem.Value.ToLower()}.{nameof(store.Model.IsDeleted)} = 0");
            }

            if (store.Pagination != null)
            {
                if (store.Order == null)
                {
                    throw new Exception("分页中没有指定排序字段");
                }
                var (fields, tableName) = ExtractOrderFields(store);
                var orderTemplate = _templateBase.CreateOrderBy(store.Order.OrderBy, $@"{tableName}.{fields}");

                var newSql = _templateBase.CreatePagination(store.Pagination, orderTemplate, result.ToString());
                result.ClearSql();
                result.Append(newSql);
            }
            else if (store.Order != null)
            {
                var (fields, tableName) = ExtractOrderFields(store);
                var orderTemplate = _templateBase.CreateOrderBy(store.Order.OrderBy, $@"{tableName}.{fields}");
                result.Append(orderTemplate);
            }

            return result.Execute();
        }

        /// <summary>
        /// 提取出排序字段
        /// </summary>
        /// <returns></returns>
        private (String Fields, String AliasName) ExtractOrderFields(ExpressionStore store)
        {
            Parameter.IfNullOrZero(store);
            var fields = (LambdaExpression)store.Order.Expression;
            if (fields.Body.NodeType == ExpressionType.MemberAccess)
            {
                var aliasName = fields.Parameters[0].Type.GetTableName().AliasName;
                var members = (fields.Body as MemberExpression);
                return (members.Member.Name, aliasName);
            }

            throw new Exception("不支持的 ORDER BY 表达式");
        }

        /// <summary>
        /// 提取出Select字段
        /// </summary>
        /// <returns></returns>
        private String ExtractSelectFields(ExpressionStore store)
        {
            Parameter.IfNullOrZero(store);
            var anonymousObjFields = new List<String>();

            if (store.Select != null)
            {
                var fields = (LambdaExpression)store.Select.Expression;
                if (fields.Body.NodeType == ExpressionType.Constant)
                {
                    var bodyArguments = (fields.Body as ConstantExpression);
                    anonymousObjFields.Add(bodyArguments.Value.ToString());
                }
                else
                {
                    var bodyArguments = (fields.Body as NewExpression).Arguments;
                    foreach (var item in bodyArguments)
                    {
                        var member = (MemberExpression)item;
                        var fieldName = ((ParameterExpression)member.Expression).Type.GetTableName().AliasName;
                        anonymousObjFields.Add($@"{fieldName}.{member.Member.Name}");
                    }
                }
            }
            else
            {
                var types = store.MergeParameterTypes();
                var tableNames = types.Select(s => new KeyValuePair<String, String>(s.Name, s.GetTableName().AliasName)).ToList();
                anonymousObjFields = types
                    .SelectMany(s => s.GetProperties(BindingFlags.Instance | BindingFlags.Public)
                    .Where(w => w.GetAttributes<PropertyValidateAttribute>().Any())
                  .Select(s1 => $@"{tableNames.FirstOrDefault(w => w.Key == s.Name).Value}.{s1.Name}")).Distinct().ToList();
            }

            return String.Join(",", anonymousObjFields);
        }
    }


    public class QueryWrapper<TModel> where TModel : EntityBase, new()
    {
        private readonly Processor _processor;

        private readonly ExpressionStore _store;

        internal QueryWrapper(ExpressionStore store, Processor processor)
        {
            Parameter.IfNullOrZero(store);
            Parameter.IfNullOrZero(processor);

            _store = store;
            _processor = processor;
        }

        public QueryWrapper<TModel> Query()
        {
            _store.AddFrom<TModel>();
            return this;
        }

        public QueryWrapper<TModel> LeftJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> join)
        where TRight : EntityBase, new()
        {
            Parameter.IfNullOrZero(join);
            _store.AddJoin(join, JoinRelation.LEFT);
            return this;
        }

        public QueryWrapper<TModel> LeftJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Parameter.IfNullOrZero(join);
            _store.AddJoin(join, JoinRelation.LEFT);

            return this;
        }

        public QueryWrapper<TModel> RightJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> join)
        where TRight : EntityBase, new()
        {
            Parameter.IfNullOrZero(join);
            _store.AddJoin(join, JoinRelation.LEFT);
            return this;
        }

        public QueryWrapper<TModel> RightJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Parameter.IfNullOrZero(join);
            _store.AddJoin(join, JoinRelation.RIGHT);

            return this;
        }

        public QueryWrapper<TModel> InnerJoin<TRight>(Expression<Func<TModel, TRight, Boolean>> join)
        where TRight : EntityBase, new()
        {
            Parameter.IfNullOrZero(join);
            _store.AddJoin(join, JoinRelation.INNER);
            return this;
        }

        public QueryWrapper<TModel> InnerJoin<TLeft, TRight>(Expression<Func<TLeft, TRight, Boolean>> join)
        where TLeft : EntityBase, new()
        where TRight : EntityBase, new()
        {
            Parameter.IfNullOrZero(join);
            _store.AddJoin(join, JoinRelation.INNER);

            return this;
        }

        public QueryWrapper<TModel> Page(Int32 pageIndex, Int32 pageSize, Int32 maxKey = 0)
        {
            Parameter.IfNullOrZero(pageIndex);
            Parameter.IfNullOrZero(pageSize);

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
            Parameter.IfNullOrZero(filter);
            _store.AddWhere(filter);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1>(Expression<Func<TModel1, Boolean>> filter)
        where TModel1 : EntityBase, new()
        {
            Parameter.IfNullOrZero(filter);
            _store.AddWhere(filter);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1>(Expression<Func<TModel, TModel1, Boolean>> filter)
        where TModel1 : EntityBase, new()
        {
            Parameter.IfNullOrZero(filter);
            _store.AddWhere(filter);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1, TModel2>(Expression<Func<TModel, TModel1, TModel2, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        {
            Parameter.IfNullOrZero(filter);
            _store.AddWhere(filter);
            return this;
        }


        public QueryWrapper<TModel> Where<TModel1, TModel2, TModel3>(Expression<Func<TModel, TModel1, TModel2, TModel3, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        {
            Parameter.IfNullOrZero(filter);
            _store.AddWhere(filter);
            return this;
        }

        public QueryWrapper<TModel> Where<TModel1, TModel2, TModel3, TModel4>(Expression<Func<TModel, TModel1, TModel2, TModel3, TModel4, Boolean>> filter)
        where TModel1 : EntityBase, new()
        where TModel2 : EntityBase, new()
        where TModel3 : EntityBase, new()
        where TModel4 : EntityBase, new()
        {
            Parameter.IfNullOrZero(filter);
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
            Parameter.IfNullOrZero(filter);
            _store.AddWhere(filter);
            return this;
        }

        public QueryWrapper<TModel> ThenByDesc<TKey>(Expression<Func<TModel, TKey>> order)
        {
            Parameter.IfNullOrZero(order);
            _store.AddOrderBy(order, OrderByType.DESC);
            return this;
        }

        public QueryWrapper<TModel> ThenByAsc<TKey>(Expression<Func<TModel, TKey>> order)
        {
            Parameter.IfNullOrZero(order);
            _store.AddOrderBy(order, OrderByType.ASC);
            return this;
        }

        public QueryWrapper<TModel> Include<TModel1>(Expression<Func<TModel, TModel1>> include)
        where TModel1 : EntityBase, new()
        {
            Parameter.IfNullOrZero(include);
            _store.AddInclude(include);
            return this;
        }

        public TModel FirstOrDefault()
        {
            return RunDiagnosis.Watch(() =>
            {
                return _processor.Process(_store).FirstOrDefault<TModel>();
            });
        }

        public TResult FirstOrDefault<TResult>() where TResult : new()
        {
            return RunDiagnosis.Watch(() =>
            {
                return _processor.Process(_store).FirstOrDefault<TResult>();
            });
        }

        public List<TModel> ToList()
        {
            return RunDiagnosis.Watch(() =>
            {
                return _processor.Process(_store).ToList<TModel>();
            });
        }

        public List<TResult> ToList<TResult>() where TResult : new()
        {
            return RunDiagnosis.Watch(() =>
            {
                return _processor.Process(_store).ToList<TResult>();
            });
        }

        public Int32 Count()
        {
            return RunDiagnosis.Watch(() =>
            {
                Select((a) => "COUNT(*)");
                return _processor.Process(_store).FirstOrDefault<Int32>();
            });
        }
    }
}

using System;
using System.Collections.Generic;
using System.Configuration;
using System.Linq;
using System.Linq.Expressions;
using System.Net;
using System.Threading;
using System.Threading.Tasks;
using NewLib.Data.Mongodb.Bson.ObjectModel;
using NewLib.Data.Mongodb.Core;
using NewLib.Data.Mongodb.Driver;
using Nito.AsyncEx;

namespace NewLib.Data.Mongodb
{

    public sealed class MongoServiceApi
    {
        private readonly MongoClient _server;

        private readonly String _databaseName;

        private readonly String _connectionString = ConfigurationManager.ConnectionStrings["MongoConnection"].ToString();

        private static readonly MongoUrlBuilder _mongoUrlBuilder = new MongoUrlBuilder();


        #region 初始化MongoServiceApi实例
        /// <summary>
        /// 初始化MongoServiceApi实例
        /// </summary>
        public MongoServiceApi(String connectionString = "")
        {
            if ((connectionString + "").Length != 0)
            {
                _connectionString = connectionString;
            }
            if ((_connectionString + "").Length == 0)
            {
                throw new ArgumentNullException(_connectionString);
            }
            var mongoConnection = ParseConnectionString(_connectionString);
            if (mongoConnection.MongoServerAddresses.Count == 1)
            {
                _mongoUrlBuilder.Server = mongoConnection.MongoServerAddresses.FirstOrDefault();
            }
            else
            {
                _mongoUrlBuilder.Servers = mongoConnection.MongoServerAddresses;
            }

            if ((mongoConnection.ReplicaSetName + "").Length != 0)
            {
                _mongoUrlBuilder.ConnectionMode = mongoConnection.ConnectionMode;
                _mongoUrlBuilder.ReadPreference = mongoConnection.ReadPreference;
                _mongoUrlBuilder.ReplicaSetName = mongoConnection.ReplicaSetName;
            }

            _mongoUrlBuilder.Username = mongoConnection.UserName;
            _mongoUrlBuilder.Password = mongoConnection.Password;
            _mongoUrlBuilder.DatabaseName = mongoConnection.DataBase;
            _databaseName = mongoConnection.DataBase;

            _server = new MongoClient(_mongoUrlBuilder.ToMongoUrl());
        }


        #endregion

        #region 静态构造
        static MongoServiceApi()
        {
            _mongoUrlBuilder.MaxConnectionLifeTime = TimeSpan.FromMinutes(30);
            _mongoUrlBuilder.MaxConnectionPoolSize = 256;
            _mongoUrlBuilder.MinConnectionPoolSize = 20;
        }
        #endregion

        #region mongo操作

        #region 异步

        #region 添加

        /// <summary>
        /// 添加
        /// </summary>
        /// <param name="entity">实体</param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        public async Task AddAsync<T>(T entity, CancellationToken cancellationToken = default(CancellationToken))
        {
            if (entity == null)
            {
                throw new ArgumentException("entity为空");
            }
            var dataBase = _server.GetDatabase(_databaseName);
            var collection = dataBase.GetCollection<T>(typeof(T).Name);
            await collection.InsertOneAsync(entity, null, cancellationToken).ConfigureAwait(false);
        }

        /// <summary>
        /// 添加
        /// </summary>
        /// <param name="entitys">实体集合</param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        public async Task AddAsync<T>(IEnumerable<T> entitys, CancellationToken cancellationToken = default(CancellationToken))
        {
            if (entitys == null)
            {
                throw new ArgumentException("entitys为空");
            }
            if (!entitys.Any())
            {
                throw new ArgumentException("entitys的大小为0");
            }
            var dataBase = _server.GetDatabase(_databaseName);

            var collection = dataBase.GetCollection<T>(typeof(T).Name);

            await collection.InsertManyAsync(entitys, null, cancellationToken).ConfigureAwait(false);
        }

        /// <summary>
        /// 添加
        /// </summary>
        /// <param name="entity"></param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        public async Task AddAsync<T>(BsonDocument entity, CancellationToken cancellationToken = default(CancellationToken))
        {
            var dataBase = _server.GetDatabase(_databaseName);

            var collection = dataBase.GetCollection<BsonDocument>(entity["CollectionName"].AsString);
            entity.Remove("CollectionName");
            await collection.InsertOneAsync(entity, null, cancellationToken).ConfigureAwait(false);
        }

        #endregion

        #region 查询

        /// <summary>
        /// 查询列表
        /// </summary>
        /// <param name="selector">查询表达式</param>
        /// <param name="pageIndex">页码</param>
        /// <param name="pageSize">条数</param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        public async Task<List<T>> FindAsync<T>(Expression<Func<T, Boolean>> selector, Int32 pageIndex, Int32 pageSize, CancellationToken cancellationToken = default(CancellationToken))
        {
            return await Task.Run(() =>
             {
                 if (selector == null)
                 {
                     throw new ArgumentException("selector为空");
                 }
                 if (pageIndex <= 0)
                 {
                     throw new ArgumentException("pageIndex小于或等于0");
                 }
                 if (pageSize <= 0)
                 {
                     throw new ArgumentException("pageSize小于或等于0");
                 }
                 var dataBase = _server.GetDatabase(_databaseName);

                 var collection = dataBase.GetCollection<T>(typeof(T).Name);

                 return collection.AsQueryable().Where(selector).Skip((pageIndex - 1) * pageSize).Take(pageSize).ToList();
             }, cancellationToken).ConfigureAwait(false);
        }

        /// <summary>
        /// 查询单个数据
        /// </summary>
        /// <param name="selector">查询表达式</param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        public async Task<T> FindOneAsync<T>(Expression<Func<T, Boolean>> selector, CancellationToken cancellationToken = default(CancellationToken))
        {
            if (selector == null)
            {
                throw new ArgumentException("selector为空");
            }
            var dataBase = _server.GetDatabase(_databaseName);

            var collection = dataBase.GetCollection<T>(typeof(T).Name);

            var result = await collection.FindAsync<T>(selector, null, cancellationToken).ConfigureAwait(false);

            return result.FirstOrDefault();
        }

        /// <summary>
        /// 根据查询表达式返回查询总量
        /// </summary>
        /// <param name="selector">查询表达式</param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        public async Task<Int64> TotalCountAsync<T>(Expression<Func<T, Boolean>> selector, CancellationToken cancellationToken = default(CancellationToken))
        {
            if (selector == null)
            {
                throw new ArgumentException("selector为空");
            }
            var dataBase = _server.GetDatabase(_databaseName);

            var collection = dataBase.GetCollection<T>(typeof(T).Name);

            var result = await collection.CountAsync<T>(selector, null, cancellationToken).ConfigureAwait(false);
            return result;
        }

        #endregion

        #region 更新

        /// <summary>
        /// 更新单条数据
        /// </summary>
        /// <param name="selector">查询表达式</param>
        /// <param name="fields">字段集合</param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        public async Task<Boolean> UpdateOneAsync<T>(Expression<Func<T, Boolean>> selector, IEnumerable<Tuple<dynamic, dynamic>> fields, CancellationToken cancellationToken = default(CancellationToken))
        {
            if (selector == null)
            {
                throw new ArgumentException("selector为空");
            }
            if (fields == null)
            {
                throw new ArgumentException("fields为空");
            }
            if (!fields.Any())
            {
                throw new ArgumentException("fields的大小为0");
            }
            var dataBase = _server.GetDatabase(_databaseName);

            var collection = dataBase.GetCollection<T>(typeof(T).Name);

            var updateDefinitions = fields.Select(field => Builders<T>.Update.Set(field.Item1, field.Item2)).Cast<UpdateDefinition<T>>().ToList();

            var result = await collection.UpdateOneAsync<T>(selector, Builders<T>.Update.Combine(updateDefinitions), null, cancellationToken).ConfigureAwait(false); ;

            return result.ModifiedCount != 0;
        }

        /// <summary>
        /// 更新单条数据
        /// </summary>
        /// <param name="selector">查询表达式</param>
        /// <param name="field">字段元组</param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        public async Task<Boolean> UpdateOneAsync<T>(Expression<Func<T, Boolean>> selector, Tuple<dynamic, dynamic> field, CancellationToken cancellationToken = default(CancellationToken))
        {
            if (selector == null)
            {
                throw new ArgumentException("selector为空");
            }
            if (field == null)
            {
                throw new ArgumentException("fields为空");
            }

            return await UpdateOneAsync(selector, new List<Tuple<dynamic, dynamic>> { field }, cancellationToken);
        }

        /// <summary>
        /// 更新多条数据
        /// </summary>
        /// <param name="selector">查询表达式</param>
        /// <param name="fields">字段集合</param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        public async Task<Boolean> UpdateManyAsync<T>(Expression<Func<T, Boolean>> selector, IEnumerable<Tuple<dynamic, dynamic>> fields, CancellationToken cancellationToken = default(CancellationToken))
        {
            if (selector == null)
            {
                throw new ArgumentException("selector为空");
            }
            if (fields == null)
            {
                throw new ArgumentException("fields为空");
            }
            if (!fields.Any())
            {
                throw new ArgumentException("fields的大小为0");
            }
            var dataBase = _server.GetDatabase(_databaseName);

            var collection = dataBase.GetCollection<T>(typeof(T).Name);
            var updateDefinitions = fields.Select(field => Builders<T>.Update.Set(field.Item1, field.Item2)).Cast<UpdateDefinition<T>>().ToList();
            var result = await collection.UpdateManyAsync<T>(selector, Builders<T>.Update.Combine(updateDefinitions), null, cancellationToken).ConfigureAwait(false); ;

            return result.ModifiedCount != 0;
        }

        /// <summary>
        /// 更新多条数据
        /// </summary>
        /// <param name="selector">查询表达式</param>
        /// <param name="field">字段元组</param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        public async Task<Boolean> UpdateManyAsync<T>(Expression<Func<T, Boolean>> selector, Tuple<dynamic, dynamic> field, CancellationToken cancellationToken = default(CancellationToken))
        {
            if (selector == null)
            {
                throw new ArgumentException("selector为空");
            }
            if (field == null)
            {
                throw new ArgumentException("fields为空");
            }

            return await UpdateManyAsync(selector, new List<Tuple<dynamic, dynamic>> { field }, cancellationToken);
        }

        #endregion

        #region 移除

        /// <summary>
        /// 移除
        /// </summary>
        /// <param name="selector">查询表达式</param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        public async Task<Boolean> RemoveAsync<T>(Expression<Func<T, Boolean>> selector, CancellationToken cancellationToken = default(CancellationToken))
        {
            if (selector == null)
            {
                throw new ArgumentException("selector为空");
            }
            var dataBase = _server.GetDatabase(_databaseName);
            var collection = dataBase.GetCollection<T>(typeof(T).Name);
            var result = await collection.DeleteManyAsync<T>(selector, cancellationToken).ConfigureAwait(false);
            return result.DeletedCount != 0;
        }
        #endregion

        #endregion

        #region 同步

        /// <summary>
        /// 查询列表
        /// </summary>
        /// <param name="selector">查询表达式</param>
        /// <param name="pageIndex">页码</param>
        /// <param name="pageSize">条数</param>
        /// <returns></returns>
        public List<T> Find<T>(Expression<Func<T, Boolean>> selector, Int32 pageIndex, Int32 pageSize)
        {
            if (selector == null)
            {
                throw new ArgumentException("selector为空");
            }
            if (pageIndex <= 0)
            {
                throw new ArgumentException("pageIndex小于或等于0");
            }
            if (pageSize <= 0)
            {
                throw new ArgumentException("pageSize小于或等于0");
            }
            return AsyncContext.Run(() => FindAsync(selector, pageIndex, pageSize));
        }

        /// <summary>
        /// 查询单个数据
        /// </summary>
        /// <param name="selector">查询表达式</param>
        /// <returns></returns>
        public T FindOne<T>(Expression<Func<T, Boolean>> selector)
        {
            if (selector == null)
            {
                throw new ArgumentException("selector为空");
            }

            return AsyncContext.Run(() => FindOneAsync(selector));
        }

        /// <summary>
        /// 查询全部数据
        /// </summary>
        /// <returns></returns>
        public IQueryable<T> FindAll<T>()
        {
            var dataBase = _server.GetDatabase(_databaseName);

            var collection = dataBase.GetCollection<T>(typeof(T).Name);

            // var result = await collection.FindAsync<T>(selector, null, cancellationToken).ConfigureAwait(false); ;
            return collection.AsQueryable();
        }

        /// <summary>
        /// 更新单条数据
        /// </summary>
        /// <param name="selector">查询表达式</param>
        /// <param name="field">字段元组</param>
        /// <returns></returns>
        public Boolean UpdateOne<T>(Expression<Func<T, Boolean>> selector, Tuple<dynamic, dynamic> field)
        {
            if (selector == null)
            {
                throw new ArgumentException("selector为空");
            }
            if (field == null)
            {
                throw new ArgumentException("fields为空");
            }

            return UpdateOne(selector, new List<Tuple<dynamic, dynamic>>
            {
                field
            });
        }

        /// <summary>
        /// 更新单条数据
        /// </summary>
        /// <param name="selector">查询表达式</param>
        /// <param name="fields">字段集合</param>
        /// <returns></returns>
        public Boolean UpdateOne<T>(Expression<Func<T, Boolean>> selector, IEnumerable<Tuple<dynamic, dynamic>> fields)
        {
            if (selector == null)
            {
                throw new ArgumentException("selector为空");
            }
            if (fields == null)
            {
                throw new ArgumentException("fields为空");
            }
            if (!fields.Any())
            {
                throw new ArgumentException("fields的大小为0");
            }
            return AsyncContext.Run(() => UpdateOneAsync(selector, fields));
        }

        /// <summary>
        /// 更新多条数据
        /// </summary>
        /// <param name="selector">查询表达式</param>
        /// <param name="fields">字段集合</param>
        /// <returns></returns>
        public Boolean UpdateMany<T>(Expression<Func<T, Boolean>> selector, IEnumerable<Tuple<dynamic, dynamic>> fields)
        {
            if (selector == null)
            {
                throw new ArgumentException("selector为空");
            }
            if (fields == null)
            {
                throw new ArgumentException("fields为空");
            }
            if (!fields.Any())
            {
                throw new ArgumentException("fields的大小为0");
            }
            return AsyncContext.Run(() => UpdateManyAsync(selector, fields));
        }

        /// <summary>
        /// 更新多条数据
        /// </summary>
        /// <param name="selector">查询表达式</param>
        /// <param name="field">字段元组</param>
        /// <returns></returns>
        public Boolean UpdateMany<T>(Expression<Func<T, Boolean>> selector, Tuple<dynamic, dynamic> field)
        {
            if (selector == null)
            {
                throw new ArgumentException("selector为空");
            }
            if (field == null)
            {
                throw new ArgumentException("fields为空");
            }

            return UpdateMany(selector, new List<Tuple<dynamic, dynamic>>
                {
                    field
                });
        }

        /// <summary>
        /// 移除
        /// </summary>
        /// <param name="selector">查询表达式</param>
        /// <returns></returns>
        public Boolean Remove<T>(Expression<Func<T, Boolean>> selector)
        {
            if (selector == null)
            {
                throw new ArgumentException("selector为空");
            }
            return AsyncContext.Run(() => RemoveAsync(selector));
        }

        /// <summary>
        /// 根据查询表达式返回查询总量
        /// </summary>
        /// <param name="selector">查询表达式</param>
        /// <returns></returns>
        public Int64 TotalCount<T>(Expression<Func<T, Boolean>> selector)
        {
            if (selector == null)
            {
                throw new ArgumentException("selector为空");
            }
            return AsyncContext.Run(() => TotalCountAsync(selector));
        }

        /// <summary>
        /// 添加
        /// </summary>
        /// <param name="entity"></param>
        public void Add<T>(T entity)
        {
            if (entity == null)
            {
                throw new ArgumentException("entity为空");
            }
            AsyncContext.Run(() => AddAsync(entity));
        }

        /// <summary>
        /// 添加
        /// </summary>
        /// <param name="entitys">实体集合</param>
        /// <returns></returns>
        public void Add<T>(IEnumerable<T> entitys)
        {
            if (entitys == null)
            {
                throw new ArgumentException("entitys为空");
            }
            if (!entitys.Any())
            {
                throw new ArgumentException("entitys的大小为0");
            }
            AsyncContext.Run(() => AddAsync(entitys));
        }

        /// <summary>
        /// 添加
        /// </summary>
        /// <param name="entity"></param>
        /// <returns></returns>
        public void Add<T>(BsonDocument entity)
        {
            if (entity == null)
            {
                throw new ArgumentException("entity为空");
            }
            AsyncContext.Run(() => AddAsync(entity));
        }

        #endregion

        #endregion

        #region 支持方法
        /// <summary>
        /// 解析链接字符串
        /// </summary>
        /// <param name="connection"></param>
        /// <returns></returns>
        private static ClientConnectionModule ParseConnectionString(String connection)
        {
            if ((connection + "").Length == 0)
            {
                throw new ArgumentNullException(connection);
            }
            var connectionKeyValueList = connection.Split(new[] { ';' }, StringSplitOptions.RemoveEmptyEntries).ToList();
            var clientConnectionModule = new ClientConnectionModule();
            connectionKeyValueList.ForEach(key =>
            {
                var value = key.Split('=');

                switch (value[0].ToLower())
                {
                    case "username":
                        if ((value[1] + "").Length == 0)
                        {
                            throw new ArgumentException("username为空");
                        }
                        clientConnectionModule.UserName = value[1];
                        break;
                    case "password":
                        if ((value[1] + "").Length == 0)
                        {
                            throw new ArgumentException("password为空");
                        }
                        clientConnectionModule.Password = value[1];
                        break;
                    case "database":
                        if ((value[1] + "").Length == 0)
                        {
                            throw new ArgumentException("database为空");
                        }
                        clientConnectionModule.DataBase = value[1];
                        break;
                    case "serverlist":
                        {
                            var serverArray = value[1].Split(new[] { ',' }, StringSplitOptions.RemoveEmptyEntries).ToArray();
                            if (!serverArray.Any())
                            {
                                throw new ArgumentException("Mongodb的数据库地址至少具有一个");
                            }
                            clientConnectionModule.MongoServerAddresses = serverArray.Select(server => new MongoServerAddress(ParseHost(server.Split(':')[0]), Int32.Parse(server.Split(':')[1]))).ToList();
                        }
                        break;
                    case "replicasetname":
                        {
                            clientConnectionModule.ReplicaSetName = value[1];
                            clientConnectionModule.ConnectionMode = ConnectionMode.ReplicaSet;
                            clientConnectionModule.ReadPreference = ReadPreference.SecondaryPreferred;
                            break;
                        }
                    default:
                        throw new ArgumentException(String.Format("不支持的操作节点：{0}", value[0]));
                }
            });
            return clientConnectionModule;
        }
        /// <summary>
        /// 将传入的域名转换为Ip地址
        /// </summary>
        /// <param name="hostNameOrAddress"></param>
        /// <returns></returns>
        private static String ParseHost(String hostNameOrAddress)
        {
            if ((hostNameOrAddress + "").Length == 0)
            {
                throw new ArgumentException("hostNameOrAddress为空");
            }
            var mongoAddress = Dns.GetHostAddresses(hostNameOrAddress);
            var @default = mongoAddress.FirstOrDefault();
            if (@default != null)
            {
                return @default.ToString();
            }
            throw new Exception(String.Format("{0}:可能是无效的域名或Ip地址", hostNameOrAddress));
        }
        #endregion
    }


    internal sealed class ClientConnectionModule
    {
        internal IList<MongoServerAddress> MongoServerAddresses { get; set; }
        internal String UserName { get; set; }
        internal String Password { get; set; }
        internal String DataBase { get; set; }
        internal String ReplicaSetName { get; set; }
        internal ConnectionMode ConnectionMode { get; set; }
        internal ReadPreference ReadPreference { get; set; }
    }
}

using Newtonsoft.Json;
using StackExchange.Redis;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using System.Threading.Tasks;

namespace NewLibCore.Storage.Redis.InternalHelper
{
    /// <summary>
    /// Redis操作
    /// </summary>
    public class DefaultRedisQueryProvider : ICacheQueryProvider
    {

        private readonly IDatabase _database = null;

        private readonly ConnectionMultiplexer _conn;

        #region 构造函数

        public DefaultRedisQueryProvider(int dbIndex, string connectionString)
        {
            if (string.IsNullOrEmpty(connectionString))
            {
                throw new ArgumentException("connectionString不能为空");
            }

            _conn = RedisConnectionHelp.GetConnection(connectionString);
            _database = _conn.GetDatabase(dbIndex);
        }

        #endregion 构造函数

        #region string

        #region 同步方法

        /// <summary>
        /// 保存单个key value
        /// </summary>
        public bool StringSet(string key, string value, TimeSpan? expiry = default)
        {
            return _database.StringSet(key, value, expiry);
        }

        /// <summary>
        /// 保存多个key value
        /// </summary>
        public bool StringSet(IEnumerable<KeyValuePair<RedisKey, RedisValue>> keyValues)
        {
            var newkeyValues = keyValues.Select(p => new KeyValuePair<RedisKey, RedisValue>(p.Key, p.Value)).ToList();
            return _database.StringSet(newkeyValues.ToArray());
        }

        /// <summary>
        /// 保存一个对象
        /// </summary>
        public bool StringSet<T>(string key, T obj, TimeSpan? expiry = default)
        {
            return _database.StringSet(key, ConvertJson(obj), expiry);
        }

        /// <summary>
        /// 获取单个key的值
        /// </summary>
        public string StringGet(string key)
        {
            return _database.StringGet(key);
        }

        /// <summary>
        /// 获取多个Key
        /// </summary>
        public RedisValue[] StringGet(IEnumerable<string> listKey)
        {
            return _database.StringGet(ConvertRedisKeys(listKey));
        }

        /// <summary>
        /// 获取一个key的对象
        /// </summary>
        public T StringGet<T>(string key)
        {
            return ConvertObj<T>(_database.StringGet(key));
        }

        /// <summary>
        /// 为数字增长val
        /// </summary>
        public Double StringIncrement(string key, Double val = 1)
        {
            return _database.StringIncrement(key, val);
        }

        /// <summary>
        /// 为数字减少val
        /// </summary>
        public Double StringDecrement(string key, Double val = 1)
        {
            return _database.StringDecrement(key, val);
        }

        #endregion 同步方法

        #region 异步方法

        /// <summary>
        /// 保存单个key value
        /// </summary>
        public async Task<bool> StringSetAsync(string key, string value, TimeSpan? expiry = default)
        {
            return await _database.StringSetAsync(key, value, expiry);
        }

        /// <summary>
        /// 保存多个key value
        /// </summary>
        public async Task<bool> StringSetAsync(IEnumerable<KeyValuePair<RedisKey, RedisValue>> keyValues)
        {
            var newkeyValues = keyValues.Select(p => new KeyValuePair<RedisKey, RedisValue>(p.Key, p.Value)).ToList();
            return await _database.StringSetAsync(newkeyValues.ToArray());
        }

        /// <summary>
        /// 保存一个对象
        /// </summary>
        public async Task<bool> StringSetAsync<T>(string key, T obj, TimeSpan? expiry = default)
        {
            return await _database.StringSetAsync(key, ConvertJson(obj), expiry);
        }

        /// <summary>
        /// 获取单个key的值
        /// </summary>
        public async Task<string> StringGetAsync(string key)
        {
            return await _database.StringGetAsync(key);
        }

        /// <summary>
        /// 获取多个Key
        /// </summary>
        public async Task<RedisValue[]> StringGetAsync(IEnumerable<string> listKey)
        {
            return await _database.StringGetAsync(ConvertRedisKeys(listKey));
        }

        /// <summary>
        /// 获取一个key的对象
        /// </summary>
        public async Task<T> StringGetAsync<T>(string key)
        {
            var result = await _database.StringGetAsync(key);
            return ConvertObj<T>(result);
        }

        /// <summary>
        /// 为数字增长val
        /// </summary>
        public async Task<Double> StringIncrementAsync(string key, Double val = 1)
        {
            return await _database.StringIncrementAsync(key, val);
        }

        /// <summary>
        /// 为数字减少val
        /// </summary>
        public async Task<Double> StringDecrementAsync(string key, Double val = 1)
        {
            return await _database.StringDecrementAsync(key, val);
        }

        #endregion 异步方法

        #endregion string

        #region Hash

        #region 同步方法

        /// <summary>
        /// 判断某个数据是否已经被缓存
        /// </summary>
        public bool HashExists(string key, string dataKey)
        {
            return _database.HashExists(key, dataKey);
        }

        /// <summary>
        /// 存储数据到hash表
        /// </summary>
        public bool HashSet<T>(string key, string dataKey, T t)
        {
            return _database.HashSet(key, dataKey, ConvertJson(t));
        }

        /// <summary>
        /// 移除hash中的某值
        /// </summary>
        public bool HashDelete(string key, string dataKey)
        {
            return _database.HashDelete(key, dataKey);
        }

        /// <summary>
        /// 移除hash中的多个值
        /// </summary>
        public Int64 HashDelete(string key, IEnumerable<RedisValue> dataKeys)
        {
            return _database.HashDelete(key, dataKeys.ToArray());
        }

        /// <summary>
        /// 从hash表获取数据
        /// </summary>
        public T HashGet<T>(string key, string dataKey)
        {
            return ConvertObj<T>(_database.HashGet(key, dataKey));
        }

        /// <summary>
        /// 为数字增长val
        /// </summary>
        public Double HashIncrement(string key, string dataKey, Double val = 1)
        {
            return _database.HashIncrement(key, dataKey, val);
        }

        /// <summary>
        /// 为数字减少val
        /// </summary>
        public Double HashDecrement(string key, string dataKey, Double val = 1)
        {
            return _database.HashDecrement(key, dataKey, val);
        }

        /// <summary>
        /// 获取hashkey所有Redis key
        /// </summary>
        public List<T> HashKeys<T>(string key)
        {
            return ConvetList<T>(_database.HashKeys(key));
        }

        #endregion 同步方法

        #region 异步方法

        /// <summary>
        /// 判断某个数据是否已经被缓存
        /// </summary>
        public async Task<bool> HashExistsAsync(string key, string dataKey)
        {
            return await _database.HashExistsAsync(key, dataKey);
        }

        /// <summary>
        /// 存储数据到hash表
        /// </summary>
        public async Task<bool> HashSetAsync<T>(string key, string dataKey, T t)
        {
            return await _database.HashSetAsync(key, dataKey, ConvertJson(t));
        }

        /// <summary>
        /// 移除hash中的某值
        /// </summary>
        public async Task<bool> HashDeleteAsync(string key, string dataKey)
        {
            return await _database.HashDeleteAsync(key, dataKey);
        }

        /// <summary>
        /// 移除hash中的多个值
        /// </summary>
        public async Task<Int64> HashDeleteAsync(string key, IEnumerable<RedisValue> dataKeys)
        {
            return await _database.HashDeleteAsync(key, dataKeys.ToArray());
        }

        /// <summary>
        /// 从hash表获取数据
        /// </summary>
        public async Task<T> HashGeAsync<T>(string key, string dataKey)
        {
            string value = await _database.HashGetAsync(key, dataKey);
            return ConvertObj<T>(value);
        }

        /// <summary>
        /// 为数字增长val
        /// </summary>
        public async Task<Double> HashIncrementAsync(string key, string dataKey, Double val = 1)
        {
            return await _database.HashIncrementAsync(key, dataKey, val);
        }

        /// <summary>
        /// 为数字减少val
        /// </summary>
        public async Task<Double> HashDecrementAsync(string key, string dataKey, Double val = 1)
        {
            return await _database.HashDecrementAsync(key, dataKey, val);
        }

        /// <summary>
        /// 获取hashkey所有Redis key
        /// </summary>
        public async Task<List<T>> HashKeysAsync<T>(string key)
        {
            return ConvetList<T>(await _database.HashKeysAsync(key));
        }

        #endregion 异步方法

        #endregion Hash

        #region List

        #region 同步方法

        /// <summary>
        /// 移除指定ListId的内部List的值
        /// </summary>
        public void ListRemove<T>(string key, T value)
        {
            _database.ListRemove(key, ConvertJson(value));
        }

        /// <summary>
        /// 获取指定key的List
        /// </summary>
        public List<T> ListRange<T>(string key)
        {
            var values = _database.ListRange(key);
            return ConvetList<T>(values);
        }

        /// <summary>
        /// 获取指定key的List
        /// </summary>
        public List<T> ListRange<T>(string key, int start, int end)
        {
            var values = _database.ListRange(key, start, end);
            return ConvetList<T>(values);
        }

        /// <summary>
        /// 入队
        /// </summary>
        public void ListRightPush<T>(string key, T value)
        {
            _database.ListRightPush(key, ConvertJson(value));
        }

        /// <summary>
        /// 出队
        /// </summary>
        public T ListRightPop<T>(string key)
        {
            var value = _database.ListRightPop(key);
            return ConvertObj<T>(value);
        }

        /// <summary>
        /// 入栈
        /// </summary>
        public void ListLeftPush<T>(string key, T value)
        {
            _database.ListLeftPush(key, ConvertJson(value));
        }

        /// <summary>
        /// 出栈
        /// </summary>
        public T ListLeftPop<T>(string key)
        {
            var value = _database.ListLeftPop(key);
            return ConvertObj<T>(value);
        }

        /// <summary>
        /// 获取集合中的数量
        /// </summary>
        public Int64 ListLength(string key)
        {
            return _database.ListLength(key);
        }

        #endregion 同步方法

        #region 异步方法

        /// <summary>
        /// 移除指定ListId的内部List的值
        /// </summary>
        public async Task<Int64> ListRemoveAsync<T>(string key, T value)
        {
            return await _database.ListRemoveAsync(key, ConvertJson(value));
        }

        /// <summary>
        /// 获取指定key的List
        /// </summary>
        public async Task<List<T>> ListRangeAsync<T>(string key)
        {
            var values = await _database.ListRangeAsync(key);
            return ConvetList<T>(values);
        }

        /// <summary>
        /// 入队
        /// </summary>
        public async Task<Int64> ListRightPushAsync<T>(string key, T value)
        {
            return await _database.ListRightPushAsync(key, ConvertJson(value));
        }

        /// <summary>
        /// 出队
        /// </summary>
        public async Task<T> ListRightPopAsync<T>(string key)
        {
            var value = await _database.ListRightPopAsync(key);
            return ConvertObj<T>(value);
        }

        /// <summary>
        /// 入栈
        /// </summary>
        public async Task<Int64> ListLeftPushAsync<T>(string key, T value)
        {
            return await _database.ListLeftPushAsync(key, ConvertJson(value));
        }

        /// <summary>
        /// 出栈
        /// </summary>
        public async Task<T> ListLeftPopAsync<T>(string key)
        {
            var value = await _database.ListLeftPopAsync(key);
            return ConvertObj<T>(value);
        }

        /// <summary>
        /// 获取集合中的数量
        /// </summary>
        public async Task<Int64> ListLengthAsync(string key)
        {
            return await _database.ListLengthAsync(key);
        }

        #endregion 异步方法

        #endregion List

        #region SortedSet 有序集合

        #region 同步方法

        /// <summary>
        /// 添加
        /// </summary>
        public bool SortedSetAdd<T>(string key, T value, Double score)
        {
            return _database.SortedSetAdd(key, ConvertJson(value), score);
        }

        /// <summary>
        /// 删除
        /// </summary>
        public bool SortedSetRemove<T>(string key, T value)
        {
            return _database.SortedSetRemove(key, ConvertJson(value));
        }

        /// <summary>
        /// 获取全部
        /// </summary>
        public List<T> SortedSetRangeByRank<T>(string key)
        {
            var values = _database.SortedSetRangeByRank(key);
            return ConvetList<T>(values);
        }

        /// <summary>
        /// 获取集合中的数量
        /// </summary>
        public Int64 SortedSetLength(string key)
        {
            return _database.SortedSetLength(key);
        }

        #endregion 同步方法

        #region 异步方法

        /// <summary>
        /// 添加
        /// </summary>
        public async Task<bool> SortedSetAddAsync<T>(string key, T value, Double score)
        {
            return await _database.SortedSetAddAsync(key, ConvertJson(value), score);
        }

        /// <summary>
        /// 删除
        /// </summary>
        public async Task<bool> SortedSetRemoveAsync<T>(string key, T value)
        {
            return await _database.SortedSetRemoveAsync(key, ConvertJson(value));
        }

        /// <summary>
        /// 获取全部
        /// </summary>
        public async Task<List<T>> SortedSetRangeByRankAsync<T>(string key)
        {
            var values = await _database.SortedSetRangeByRankAsync(key);
            return ConvetList<T>(values);
        }

        /// <summary>
        /// 获取集合中的数量
        /// </summary>
        public async Task<Int64> SortedSetLengthAsync(string key)
        {
            return await _database.SortedSetLengthAsync(key);
        }

        #endregion 异步方法

        #endregion SortedSet 有序集合

        #region key

        /// <summary>
        /// 删除单个key
        /// </summary>
        public bool KeyDelete(string key)
        {
            return _database.KeyDelete(key);
        }

        /// <summary>
        /// 删除多个key
        /// </summary>
        public Int64 KeyDelete(IEnumerable<string> keys)
        {
            return _database.KeyDelete(ConvertRedisKeys(keys));
        }

        /// <summary>
        /// 判断key是否存储
        /// </summary>
        public bool KeyExists(string key)
        {
            return _database.KeyExists(key);
        }

        /// <summary>
        /// 重新命名key
        /// </summary>
        public bool KeyRename(string key, string newKey)
        {
            return _database.KeyRename(key, newKey);
        }

        /// <summary>
        /// 设置Key的时间
        /// </summary>
        public bool KeyExpire(string key, TimeSpan? expiry = default)
        {
            return _database.KeyExpire(key, expiry);
        }

        #endregion key

        #region 发布订阅

        /// <summary>
        /// Redis发布订阅  订阅
        /// </summary>
        public void Subscribe(string subChannel, Action<RedisChannel, RedisValue> handler = null)
        {
            if (string.IsNullOrEmpty(subChannel))
            {
                throw new ArgumentException("subChannel不能为空");
            }

            var sub = _conn.GetSubscriber();
            sub.Subscribe(subChannel, (channel, message) =>
            {
                if (handler == null)
                {
                    Console.WriteLine(subChannel + " 订阅收到消息：" + message);
                }
                else
                {
                    handler(channel, message);
                }
            });
        }

        /// <summary>
        /// Redis发布订阅  发布
        /// </summary>
        public Int64 Publish<T>(string channel, T msg)
        {
            if (string.IsNullOrEmpty(channel))
            {
                throw new ArgumentException("channel不能为空");
            }

            var sub = _conn.GetSubscriber();
            return sub.Publish(channel, ConvertJson(msg));
        }

        /// <summary>
        /// Redis发布订阅  取消订阅
        /// </summary>
        public void Unsubscribe(string channel)
        {
            if (string.IsNullOrEmpty(channel))
            {
                throw new ArgumentException("channel不能为空");
            }

            var sub = _conn.GetSubscriber();
            sub.Unsubscribe(channel);
        }

        /// <summary>
        /// Redis发布订阅  取消全部订阅
        /// </summary>
        public void UnsubscribeAll()
        {
            var sub = _conn.GetSubscriber();
            sub.UnsubscribeAll();
        }

        #endregion 发布订阅

        #region 其他

        public ITransaction CreateTransaction()
        {
            return GetDatabase().CreateTransaction();
        }

        public IDatabase GetDatabase()
        {
            return _conn.GetDatabase(0);
        }

        public IServer GetServer(string hostAndPort)
        {
            if (string.IsNullOrEmpty(hostAndPort))
            {
                throw new ArgumentException("hostAndPort不能为空");
            }

            return _conn.GetServer(hostAndPort);
        }

        #endregion 其他

        #region 辅助方法

        private string ConvertJson<T>(T value)
        {
            var result = value is string ? value.ToString() : JsonConvert.SerializeObject(value);
            return result;
        }

        private T ConvertObj<T>(RedisValue value)
        {
            if (value.IsNull)
            {
                return default;
            }
            var r = JsonConvert.DeserializeObject<T>(value, new JsonSerializerSettings
            {
                ContractResolver = new PrivateSetterContractResolver()
            });
            return r;
        }

        private List<T> ConvetList<T>(RedisValue[] values)
        {
            var result = new List<T>();
            return values.Select(s => JsonConvert.DeserializeObject<T>(s, new JsonSerializerSettings
            {
                ContractResolver = new PrivateSetterContractResolver()
            })).ToList();
        }

        private RedisKey[] ConvertRedisKeys(IEnumerable<string> redisKeys)
        {
            return redisKeys.Select(redisKey => (RedisKey)$@"{redisKey}").ToArray();
        }

        #endregion 辅助方法

        public RedisType GetKeyType(string key)
        {
            return _database.KeyType(key);
        }
    }
}
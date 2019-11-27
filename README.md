# EntityMapper v0.01
#### 注：文档中相应的API与调用方式可能会在后续版本中出现变更

### 简介
**EntityMapper**是一个可以将简单的表达式树翻译为对应的SQL语句的类库，支持**单表查询**，**多表连接查询**，**分页查询**，**原生SQL执行**和**排序**等基本操作，后续还会继续支持**外键表**，**聚合函数**等翻译操作
# 数据实体变化
* 特性
    > Require:被修饰的属性将是不可为空的        
    ```C#
    [Require]
    public String UserName { get; set; }
    ```
    > InputRange:被修饰的属性将会被限制输入长度    
    ```C#
    /// <summary>
    /// 当InputRange只被输入一个参数时表示被修饰的属性最大只能输入50的长度
    /// </summary>
    [InputRange(50)]
    public String UserName { get; set; }
    
    /// <summary>
    /// 当InputRange被输入两个参数时表示被修饰属性最小不能小于10最大不能大于20的长度
    /// </summary>
    [InputRange(10,20)]
    public String UserName { get; set; }
    ```
    > DefaultValue:被修饰的属性将可为NULL或0,但在插入数据库时会被赋值为对应的默认值   
    ```C#
    
    /// <summary>
    /// DefaultValue可被输入自定义的默认值
    /// </summary>
    [DefaultValue("游客")]
    public String UserName { get; set; }
    
    /// <summary>
    /// DefaultValue可根据被修饰的属性的类型来确定默认值,0或NULL
    /// </summary>
    [DefaultValue(typeof(String))]
    public String UserName { get; set; }
    ```
    >DateTimeDefaultValue:专门用于时间类型的属性默认值   
    
    ```C#
    /// <summary>
    /// 被DateTimeDefaultValue修饰的DateTime属性将在插入数据库时具有默认值,默认值为当前时间:DateTime.Now
    /// </summary>
    [DateTimeDefaultValue]
    public DateTime CreateTime { get; set; }
    ```
    >TableName:用于表示被修饰的数据实体在数据库中的表名称    
    ```C#
    /// <summary>
    /// 当TableName只被输入单个参数时表示被修饰的实体在数据库中名为参数值
    /// </summary>
    [TableName("newcrm_user")]
    public class UserEntity
    {
      ...
    }
    
    /// <summary>
    /// TableName第一个参数为在数据库中的表名称,第二个参数表示被修饰的类在Expression被翻译成sql语句时,表的别名
    /// 例如 SELECT a1.UserName,a1.Password,a1.CreateTime FROM newcrm_user AS a1 WHERE a1.IsActive=1
    /// </summary>
    [TableName("newcrm_user","a1")]
    public class UserEntity
    {
      ...
    }
    ```
 * 数据实体基类
   > EntityBase:只有数据实体继承与EntityBase后才会被EntityMapper类库所识别    
   ```C#
   public class UserEntity:EntityBase
   {
    ...
   }
   
   public class UserRole:EntityBase
   {
    ...
   }
   ```


# 基本操作示例

  1.初始化EntityMapper的默认设置和实例
  ```C#
  /// <summary>
  /// 初始化EntityConfig的默认设置:mysql实体映射，将查询缓存保存在内存中，将日志输出到控制台，事物隔离级别未指定
  /// </summary>
  MapperConfig.InitDefaultSetting();
  
  /// <summary>
  /// 设置连接字符串
  /// </summary>
  MapperConfig.ConnectionStringName = "NewDbContext";
  
  //使用mysql的实体映射
  MapperConfig.UseMySql();
  
  //使用mssql的实体映射
  MapperConfig.UseMsSql();
  
  //设置自定义的查询结果缓存
  MapperConfig.SetCache(new CustomCache());
  
  //设置自定义的保存日志记录的方式
  MapperConfig.SetLogger(new FileLogger());
  
  //设置 当使用事物时，事物将运行在指定的隔离级别
  MapperConfig.SetTransactionLevel(IsolationLevel.Unspecified);
  
  //初始化EntityMapper类的新实例
  var mapper = EntityMapper.CreateMapper();
  ```
  2.添加操作
  ```C#
  using(var mapper = EntityMapper.CreateMapper())
  {
      var user = new User("test123","test123");
      user = mapper.Add(user);
  }
  ```
  3.更新操作
  ```C#
  using(var mapper = EntityMapper.CreateMapper())
  {
      /**
       更新操作暂只支持指定字段更新，可在EntityBase子类中调用OnChanged()来通知基类有实体属性值被修改。
       OnChanged()方法传入被更新的属性名称，例如：OnChanged(nameof(UserName))                
     */
      var user = mapper.Query<User>().Where(w=>w.Id==4).FirstOrDefault();
      user.ModifyName("test456");
      var success = mapper.Update(user,u=>u.Id==1);
      if(success)
      {
        '成功'
      }
      else
      {
        '失败'
      }
      
      
  }
  ```
  4. 查询操作
  ```C#
  using(var mapper = EntityMapper.CreateMapper())
  {
      //注：在没有调用Select()方法时进行查询，会将指定表中所有的字段全部查出并返回。
    
      //查询单表操作
      var result = mapper.Query<User>().FirstOrDefault();
      var result = mapper.Query<User>().ToList();

      //从单表中查询出指定字段
      var result = mapper.Query<User>().Select(u => new { u.Id, u.Name, u.LoginPassword }).FirstOrDefault();
      var result = mapper.Query<User>().Select(u => new { u.Id, u.Name, u.LoginPassword }).ToList();

      //内连接查询
      var result = mapper.Query<Config>().InnerJoin<User>((c, u) => c.UserId == u.Id).FirstOrDefault();
      var result = mapper.Query<Config>().InnerJoin<User>((c, u) => c.UserId == u.Id).ToList();

      //左连接查询
      var result = mapper.Query<App>().LeftJoin<Member>((a, m) => a.Id == m.AppId).FirstOrDefault();
      var result = mapper.Query<App>().LeftJoin<Member>((a, m) => a.Id == m.AppId).ToList();

      //右连接查询
      var result = mapper.Query<App>().RightJoin<Member>((a, m) => a.Id == m.AppId).FirstOrDefault();
      var result = mapper.Query<App>().RightJoin<Member>((a, m) => a.Id == m.AppId).ToList();
      
      //分页查询,如果在查询分页时不指定排序字段则抛出异常
      var result = mapper.Query<App>().LeftJoin<Member>((a,m)=>a.Id==m.AppId).ThenByDesc(w=>w.AddTime).Page(1,15).ToList();
    
      //返回元组形式的返回值，元组中项的个数需要与Select方法中指定查询的字段的个数一致，且顺序也需一致。
      var result = mapper.Query<Log>().ToList<(Int32 Id,String Name)>();
      
      //注意：当返回值为元组形式时，需首先调用Select方法将需要返回的字段指定。若整张表的字段不超过8个则可以不用调用Select方法。
      var result = mapper.Query<App>().Select(s => new { s.Id,s.Name }).ToList<(Int32 Id,String Name)>();
  }
  ```
  5. 事物操作
  ```C#
  using(var mapper = EntityMapper.CreateMapper())
  {             
      try
      {
          mapper.OpenTransaction();

          var user = mapper.Query<User>()
          .Where(w => w.Id == 4 && w.Name == "xiaofan")
          .Select(s => new { s.Id, s.LoginPassword, s.Name })
          .FirstOrDefault();

          user.ModifyLoginPassword("xiaofan123123");

          var userRole = mapper.Query<UserRole>().Where(w => w.UserId == user.Id).FirstOrDefault();
          userRole.Remove();
          var result = mapper.Update(userRole, w => w.UserId == user.Id);
          if (!result)
          {
              mapper.Rollback();
              return;
          }
          mapper.Commit();
      }
      catch (System.Exception)
      {
          mapper.Rollback();
      }
  }
```


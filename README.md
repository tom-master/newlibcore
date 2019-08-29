# EntityMapper简介(持续更新...)
**EntityMapper**是一个可以将简单的**Expression**翻译为对应的**SQL**语句，支持连接查询以及排序等操作，后续还会继续支持复杂**SQL**语句翻译操作
# 基本操作示例

  1.初始化EntityMapper实例
  ```
  MapperConfig.InitMapper();
  ```
  2.添加操作
  ```
  using(var mapper = EntityMapper.CreateMapper())
  {
    var user = new User("test123","test123");
    user = mapper.Add(user);
  }
  ```
  3.更新操作
  ```
  using(var mapper = EntityMapper.CreateMapper())
  {
    var user = '从数据库查询出的对象';
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
  ```
  using(var mapper = EntityMapper.CreateMapper())
  {
    var users = mapper.Select<User>().Where(user=>user.Id==1).ToList();
  }
  ```

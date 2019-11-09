# EntityMapper(持续更新...)

### 简介
**EntityMapper**是一个可以将简单的**Expression**翻译为对应的**SQL**语句，支持连接查询以及排序等操作，后续还会继续支持复杂**SQL**语句翻译操作
# 基本操作示例

  1.初始化EntityMapper实例
  ```C#
  MapperConfig.InitMapper();
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
  }
  ```
  5. 事物操作
  ```C#
  using(var mapper = EntityMapper.CreateMapper())
  {try
                {
                    mapper.OpenTransaction();

                    var user = mapper.Query<User>().Where(w => w.Id == 4 && w.Name == "xiaofan").Select(s => new { s.Id, s.LoginPassword, s.Name }).FirstOrDefault();
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

using System;
using System.ComponentModel;
using NewLibCore.Data.Mapper.InternalDataStore;
using NewLibCore.Data.Mapper.MapperExtension;
using NewLibCore.Data.Mapper.PropertyExtension;

namespace NewLibCore.Run
{
	class Program
	{
		static void Main(string[] args)
		{

			using (var dataStore = new DataStore("Server=39.106.106.137;Database=NewCrmContext;Uid=root;Pwd=xiaofan@.1;port=6033;SslMode=none;Pooling=True;Min Pool Size=5;Max Pool Size=10;Treat Tiny As Boolean=false"))
			{
				var ids = "应用审核通过通知";
				var notify = new Notify();
				//notify.Read();
				//dataStore.Modify(notify, n => n.Title.Contains(ids));
				//var a1 = dataStore.Find<Notify>(a => a.Id == 3, a => new { a.Id, a.Title, a.Content });
			}
		}
	}

	[Serializable, Description("消息")]
	public partial class Notify : DomainModelBase
	{
		[PropertyRequired, InputRange(4, 10)]
		public String Title { get; private set; }

		[PropertyRequired, InputRange(1, 20)]
		public String Content { get; private set; }

		[PropertyDefaultValue(typeof(Boolean), false)]
		public Boolean IsNotify { get; private set; }

		[PropertyDefaultValue(typeof(Boolean), false)]
		public Boolean IsRead { get; private set; }

		[PropertyRequired]
		public Int32 AccountId { get; private set; }

		[PropertyRequired]
		public Int32 ToAccountId { get; private set; }

		public Notify(String title, String content, Int32 accountId, Int32 toAccountId)
		{
			Title = title;
			Content = content;
			AccountId = accountId;
			ToAccountId = toAccountId;

			IsNotify = true;
			IsRead = false;
		}

		public Notify() { }
	}

	public partial class Notify
	{
		public Notify Read()
		{
			IsRead = true;
			OnPropertyChanged(new PropertyArgs(nameof(IsRead), IsRead));
			return this;
		}
	}

	[Serializable]
	public abstract class DomainModelBase : PropertyMonitor
	{
		protected DomainModelBase()
		{
			IsDeleted = false;
		}

		public Int32 Id { get; protected set; }

		[PropertyDefaultValue(typeof(Boolean), false)]
		public Boolean IsDeleted { get; protected set; }

		[DateTimeDefaultValue]
		public DateTime AddTime { get; protected set; }

		[DateTimeDefaultValue]
		public DateTime LastModifyTime { get; protected set; }

		public void Remove()
		{
			IsDeleted = true;
			OnPropertyChanged(new PropertyArgs(nameof(IsDeleted), IsDeleted));
		}
	}
}

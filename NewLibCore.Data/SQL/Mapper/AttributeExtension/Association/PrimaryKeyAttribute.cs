using System; 

namespace NewLibCore.Data.SQL.Mapper.AttributeExtension.Association
{
	/// <summary>
	/// 主键特性
	/// </summary>
	[AttributeUsage(AttributeTargets.Property, AllowMultiple = false, Inherited = true)]
	public class PrimaryKeyAttribute : PropertyValidate
	{
	}
}

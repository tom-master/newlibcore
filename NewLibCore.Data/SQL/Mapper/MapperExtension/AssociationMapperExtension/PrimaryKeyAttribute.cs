using System;
using NewLibCore.Data.SQL.Mapper.Extension.PropertyExtension;

namespace NewLibCore.Data.SQL.Mapper.Extension.AssociationMapperExtension
{
	/// <summary>
	/// 主键特性
	/// </summary>
	[AttributeUsage(AttributeTargets.Property, AllowMultiple = false, Inherited = true)]
	public class PrimaryKeyAttribute : PropertyValidate
	{
	}
}

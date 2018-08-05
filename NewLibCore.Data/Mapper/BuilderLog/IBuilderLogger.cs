using System;
using System.Threading.Tasks;
namespace NewLibCore.Data.Mapper.BuilderLog
{
	internal interface IBuilderLogger
	{
		Task WriteLogAsync(Object model);
	}
}
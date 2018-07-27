using System.Threading.Tasks;
namespace NewLibCore.Data.Mapper.BuilderLog
{
    internal interface IBuilderLog
    {
        Task WriteLogAsync(LogModel model);
    }
}
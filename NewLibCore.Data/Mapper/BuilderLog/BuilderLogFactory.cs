using System;
using System.Collections.Generic;
using System.Threading;

namespace NewLibCore.Data.Mapper.BuilderLog
{
    internal class BuilderLogFactory
    {
        private static IList<IBuilderLog> _builderLogs = new List<IBuilderLog>();
        
        internal static void Register(IBuilderLog builderLog)
        {
            _builderLogs.Add(builderLog);
        }
    }
}

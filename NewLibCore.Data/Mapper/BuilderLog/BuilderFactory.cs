using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;

namespace NewLibCore.Data.Mapper.BuilderLog
{
	internal class BuilderFactory
	{
		private static IList<IBuilderLogger> _builderLoggers = new List<IBuilderLogger>();

		internal static void RegisterLogger(IBuilderLogger builderLog)
		{
			_builderLoggers.Add(builderLog);
		}

		internal static IBuilderLogger GetLogInstance()
		{
			return _builderLoggers.FirstOrDefault();
		}

		internal static BuilderLoggerConfig GetDefaultLoggerConfig()
		{
			return new BuilderLoggerConfig
			{

			};
		}
	}
}

using System;
using System.Collections.Generic;
using System.Text;

namespace NewLibCore
{
	public class Host
	{
		/// <summary>
		/// 获取环境变量
		/// </summary>
		/// <returns></returns>
		public String GetHostVar(String varName)
		{
			var v1 = Environment.GetEnvironmentVariable(varName, EnvironmentVariableTarget.Machine);
			if (!String.IsNullOrEmpty(v1))
			{
				return v1;
			}

			v1 = Environment.GetEnvironmentVariable(varName, EnvironmentVariableTarget.Process);
			if (!String.IsNullOrEmpty(v1))
			{
				return v1;
			}

			v1 = Environment.GetEnvironmentVariable(varName, EnvironmentVariableTarget.User);
			if (!String.IsNullOrEmpty(v1))
			{
				return v1;
			}

			throw new Exception($@"没有找到设置的{varName}环境变量");
		}
	}
}

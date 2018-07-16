using System;
using NewCrmCore.Infrastructure;

namespace ConsoleApp1
{
	class Program
	{
		static void Main(string[] args)
		{
			Console.WriteLine(Appsetting.Database);
			Console.ReadKey();
		}
	}
}

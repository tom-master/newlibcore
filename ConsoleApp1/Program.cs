using System;
using NewCrmCore.Infrastructure;

namespace ConsoleApp1
{
	class Program
	{
		static void Main(string[] args)
		{
			int[] a = { 49, 38, 65, 97, 76, 13, 27, 49, 78, 34, 12, 64, 1 };
			Console.WriteLine(String.Join(",", a));
			for (int i = 1; i < a.Length; i++)
			{
				var temp = a[i];
				var j = 0;
				for (j = i - 1; j >= 0; j--)
				{
					if (a[j] > temp)
					{
						a[j + 1] = a[j];
					}
					else
					{
						break;
					}
				}
				a[j + 1] = temp;
			}
			Console.WriteLine(String.Join(",", a));
			Console.ReadKey();
		}
	}
}

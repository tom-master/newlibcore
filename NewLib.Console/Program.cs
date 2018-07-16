using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Http;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using NewLib.Data.Mapper;
using NewLib.Data.Mapper.InternalDataStore;
using NewLib.Data.Mapper.MapperExtension;
using NewLib.Security;

namespace NewLib.Console
{
	class Program
	{
		static void Main(string[] args)
		{
			var sourceStr = $@"DfkMZgoYZAdq8NzZaxNIZy0rEYViBTGctpF8yRIX1X1yYfnKWPtlc/c2wUshExkEIzFBCEyPD2kg0McmK8EFNHPtpk09rpIlf8i5NYFxIIWkdKxXxW8L8ZsA418pjGBCDEG9MPFwPGZ2KBWnrl2vx4hs6HFa3MpVq0HYYRlEz4E=";
			var str = SensitiveDataSafetyProvider.Decrypt(sourceStr);
		}
	}

}

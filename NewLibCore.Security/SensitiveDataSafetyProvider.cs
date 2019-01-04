using System;
using System.IO;
using System.Security.Cryptography;
using System.Text;

namespace NewLibCore.Security
{
	public class SensitiveDataSafetyProvider
	{

		private readonly static String _safetyString = "因缺斯汀啦啦啦";

		public static String Encrypt(String source)
		{
			try
            {
                if (String.IsNullOrEmpty(source))
                {
                    throw new ArgumentException("source不能为空");
                }

				var aes = new AesCryptoServiceProvider();
				var md5 = new MD5CryptoServiceProvider();
				var sha256 = new SHA256CryptoServiceProvider();
				byte[] key = sha256.ComputeHash(Encoding.UTF8.GetBytes(_safetyString));
				byte[] iv = md5.ComputeHash(Encoding.UTF8.GetBytes(_safetyString));
				aes.Key = key;
				aes.IV = iv;

				byte[] dataByteArray = Encoding.UTF8.GetBytes(source);
				using (var ms = new MemoryStream())
				using (var cs = new CryptoStream(ms, aes.CreateEncryptor(), CryptoStreamMode.Write))
				{
					cs.Write(dataByteArray, 0, dataByteArray.Length);
					cs.FlushFinalBlock();
					return Convert.ToBase64String(ms.ToArray());
				}
			}
			catch (Exception e)
			{
				throw new ArgumentException(e.Message);
			}
		}

		public static String Decrypt(String source)
		{
			try
			{
                if (String.IsNullOrEmpty(source))
                {
                    throw new ArgumentException("source不能为空");
                }

                var aes = new AesCryptoServiceProvider();
				var md5 = new MD5CryptoServiceProvider();
				var sha256 = new SHA256CryptoServiceProvider();
				byte[] key = sha256.ComputeHash(Encoding.UTF8.GetBytes(_safetyString));
				byte[] iv = md5.ComputeHash(Encoding.UTF8.GetBytes(_safetyString));
				aes.Key = key;
				aes.IV = iv;

				byte[] dataByteArray = Convert.FromBase64String(source);
				using (var ms = new MemoryStream())
				using (var cs = new CryptoStream(ms, aes.CreateDecryptor(), CryptoStreamMode.Write))
				{
					cs.Write(dataByteArray, 0, dataByteArray.Length);
					cs.FlushFinalBlock();
					return Encoding.UTF8.GetString(ms.ToArray());
				}
			}
			catch (Exception e)
			{
				throw new ArgumentException(e.Message);
			}
		}
	}
}

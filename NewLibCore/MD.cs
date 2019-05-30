using System;
using System.IO;
using System.Security.Cryptography;
using System.Text;
using NewLibCore.Validate;

namespace NewLibCore
{
    public class MD
    {
        public static String GetMD5(Stream stream)
        {
            Parameter.Validate(stream);

            var md5 = new MD5CryptoServiceProvider();
            md5.ComputeHash(stream);
            var b = md5.Hash;
            md5.Clear();
            var sb = new StringBuilder(32);
            foreach (var t in b)
            {
                sb.Append(t.ToString("X2"));
            }

            if (stream.CanSeek)
            {
                stream.Position = 0;
            }

            return sb.ToString();
        }

        public static String GetMD5(String input)
        {
            if (input == null)
            {
                return null;
            }

            var md5Hash = MD5.Create();
            var data = md5Hash.ComputeHash(Encoding.UTF8.GetBytes(input));

            var sBuilder = new StringBuilder();
            for (var i = 0; i < data.Length; i++)
            {
                sBuilder.Append(data[i].ToString("x2"));
            }

            return sBuilder.ToString();
        }
    }
}
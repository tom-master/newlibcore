using System;
using System.IO;
using System.Security.Cryptography;
using System.Text;
using NewLibCore.Validate;

namespace NewLibCore
{
    /// <summary>
    /// 提供计算MD5的操作类 
    /// </summary>
    public static class MD
    {

        public static String GetMD5(Stream stream)
        {
            var bs = new Byte[stream.Length];
            stream.Write(bs, 0, bs.Length);
            return InternalMd5(bs);
        }

        public static String GetMD5(String input)
        {
            return InternalMd5(Encoding.Default.GetBytes(input));
        }

        private static String InternalMd5(Byte[] bs)
        {
            Parameter.Validate(bs);

            var md5 = new MD5CryptoServiceProvider();
            md5.ComputeHash(bs);
            var b = md5.Hash;
            md5.Clear();
            var sb = new StringBuilder(32);
            foreach (var t in b)
            {
                sb.Append(t.ToString("X2"));
            }
            return sb.ToString();
        }
    }
}
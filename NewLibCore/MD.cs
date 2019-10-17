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
        /// <summary>
        /// 获取输入流的MD5值
        /// </summary>
        /// <param name="stream"></param>
        /// <returns></returns>
        public static String GetMD5(Stream stream)
        {
            var bs = new Byte[stream.Length];
            stream.Write(bs, 0, bs.Length);
            return InternalMd5(bs);
        }

        /// <summary>
        /// 获取输入的字符串的MD5值
        /// </summary>
        /// <param name="input"></param>
        /// <returns></returns>
        public static String GetMD5(String input)
        {
            return InternalMd5(Encoding.Default.GetBytes(input));
        }

        /// <summary>
        /// 获取输入的字节数组的MD5值
        /// </summary>
        /// <param name="bs"></param>
        /// <returns></returns>
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
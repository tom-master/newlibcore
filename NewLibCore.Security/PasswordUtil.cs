using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using NewLibCore.Validate;

namespace NewLibCore.Security
{
    public sealed class PasswordUtil
    {
        private const Int32 _saltLength = 4;

        /// <summary>
        /// 比较两个密码是否相等
        /// </summary>
        /// <param name="dbPassword"></param>
        /// <param name="userPassword"></param>
        /// <returns></returns>
        public static Boolean ComparePasswords(String dbPassword, String userPassword)
        {
            Parameter.IfNullOrZero(dbPassword);
            Parameter.IfNullOrZero(userPassword);

            var dbPwd = Convert.FromBase64String(dbPassword);
            var hashedPwd = HashString(userPassword);

            if (dbPwd.Length == 0 || hashedPwd.Length == 0 || dbPwd.Length != hashedPwd.Length + _saltLength)
            {
                return false;
            }

            var saltValue = new Byte[_saltLength];
            var saltOffset = hashedPwd.Length;
            for (var i = 0; i < _saltLength; i++)
            {
                saltValue[i] = dbPwd[saltOffset + i];
            }

            var saltedPassword = CreateSaltedPassword(saltValue, hashedPwd);
            var result = CompareByteArray(dbPwd, saltedPassword);

            return result;
        }

        /// <summary>
        /// 创建数据库密码
        /// </summary>
        /// <param name="userPassword"></param>
        /// <returns></returns>
        public static String CreateDbPassword(String userPassword)
        {
            Parameter.IfNullOrZero(userPassword);

            if (String.IsNullOrEmpty(userPassword))
            {
                throw new ArgumentException("userPassword不能为空");
            }

            var unsaltedPassword = HashString(userPassword);
            var saltValue = new Byte[_saltLength];
            var rng = new RNGCryptoServiceProvider();
            rng.GetBytes(saltValue);

            var saltedPassword = CreateSaltedPassword(saltValue, unsaltedPassword);
            return Convert.ToBase64String(saltedPassword);
        }

        /// <summary>
        /// 计算输入的字符串的Hash
        /// </summary>
        /// <param name="str"></param>
        /// <returns></returns>
        private static Byte[] HashString(String str)
        {
            Parameter.IfNullOrZero(str);

            if (String.IsNullOrEmpty(str))
            {
                throw new ArgumentException("str不能为空");
            }

            var pwd = Encoding.UTF8.GetBytes(str);

            var sha1 = SHA1.Create();
            var saltedPassword = sha1.ComputeHash(pwd);
            return saltedPassword;
        }

        /// <summary>
        /// 比较字节数组是否相等
        /// </summary>
        /// <param name="array1"></param>
        /// <param name="array2"></param>
        /// <returns></returns>
        private static Boolean CompareByteArray(ICollection<Byte> array1, IList<Byte> array2 = null)
        {
            Parameter.IfNullOrZero(array1);
            Parameter.IfNullOrZero(array2);

            if (array2 == null)
            {
                throw new ArgumentNullException($@"{nameof(array2)} is null");
            }
            if (array1.Count != array2.Count)
            {
                return false;
            }
            return !array1.Where((t, i) => t != array2[i]).Any();
        }

        /// <summary>
        /// 创建加盐密码
        /// </summary>
        /// <param name="saltValue"></param>
        /// <param name="unsaltedPassword"></param>
        /// <returns></returns>
        private static Byte[] CreateSaltedPassword(Byte[] saltValue, Byte[] unsaltedPassword)
        {
            Parameter.IfNullOrZero(saltValue);
            Parameter.IfNullOrZero(unsaltedPassword);

            var rawSalted = new Byte[unsaltedPassword.Length + saltValue.Length];
            unsaltedPassword.CopyTo(rawSalted, 0);
            saltValue.CopyTo(rawSalted, unsaltedPassword.Length);

            var sha1 = SHA1.Create();
            var saltedPassword = sha1.ComputeHash(rawSalted);

            var dbPassword = new Byte[saltedPassword.Length + saltValue.Length];
            saltedPassword.CopyTo(dbPassword, 0);
            saltValue.CopyTo(dbPassword, saltedPassword.Length);

            return dbPassword;
        }
    }
}

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
            Parameter.Validate(dbPassword);
            Parameter.Validate(userPassword);

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

        public static String CreateDbPassword(String userPassword)
        {
            Parameter.Validate(userPassword);

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

        private static Byte[] HashString(String str)
        {
            Parameter.Validate(str);

            if (String.IsNullOrEmpty(str))
            {
                throw new ArgumentException("str不能为空");
            }

            var pwd = Encoding.UTF8.GetBytes(str);

            var sha1 = SHA1.Create();
            var saltedPassword = sha1.ComputeHash(pwd);
            return saltedPassword;
        }

        private static Boolean CompareByteArray(ICollection<Byte> array1, IList<Byte> array2 = null)
        {
            Parameter.Validate(array1);
            Parameter.Validate(array2);

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

        private static Byte[] CreateSaltedPassword(Byte[] saltValue, Byte[] unsaltedPassword)
        {
            Parameter.Validate(saltValue);
            Parameter.Validate(unsaltedPassword);

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

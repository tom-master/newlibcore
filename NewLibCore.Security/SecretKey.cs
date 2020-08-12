using System;
using System.IO;
using System.Security.Cryptography;
using System.Text;
using NewLibCore.Validate;

namespace NewLibCore.Security
{
    public class SecretKey
    {
        public static String Encrypt(String source, String saltValue)
        {
            Parameter.IfNullOrZero(source);
            Parameter.IfNullOrZero(saltValue);
            try
            {
                var aes = new AesCryptoServiceProvider();
                var md5 = new MD5CryptoServiceProvider();
                var sha256 = new SHA256CryptoServiceProvider();
                var key = sha256.ComputeHash(Encoding.UTF8.GetBytes(saltValue));
                var iv = md5.ComputeHash(Encoding.UTF8.GetBytes(saltValue));
                aes.Key = key;
                aes.IV = iv;

                var dataByteArray = Encoding.UTF8.GetBytes(source);
                using (var ms = new MemoryStream())
                using (var cs = new CryptoStream(ms, aes.CreateEncryptor(), CryptoStreamMode.Write))
                {
                    cs.Write(dataByteArray, 0, dataByteArray.Length);
                    cs.FlushFinalBlock();
                    return Convert.ToBase64String(ms.ToArray());
                }
            }
            catch (Exception ex)
            {
                throw ex;
            }
        }

        public static String Decrypt(String source, String saltValue)
        {
            Parameter.IfNullOrZero(source);
            Parameter.IfNullOrZero(saltValue);
            try
            {

                var aes = new AesCryptoServiceProvider();
                var md5 = new MD5CryptoServiceProvider();
                var sha256 = new SHA256CryptoServiceProvider();
                var key = sha256.ComputeHash(Encoding.UTF8.GetBytes(saltValue));
                var iv = md5.ComputeHash(Encoding.UTF8.GetBytes(saltValue));
                aes.Key = key;
                aes.IV = iv;

                var dataByteArray = Convert.FromBase64String(source);
                using (var ms = new MemoryStream())
                using (var cs = new CryptoStream(ms, aes.CreateDecryptor(), CryptoStreamMode.Write))
                {
                    cs.Write(dataByteArray, 0, dataByteArray.Length);
                    cs.FlushFinalBlock();
                    return Encoding.UTF8.GetString(ms.ToArray());
                }
            }
            catch (Exception ex)
            {
                throw ex;
            }
        }
    }
}

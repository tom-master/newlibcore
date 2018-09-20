using System;
using System.Text.RegularExpressions;

namespace NewLibCore.Security
{
    public class UnlegalChatDetection
    {
        public static string FilterBadChat(string value)
        {
            if (value == null)
            {
                return "";
            }
            else
            {
                //删除脚本
                value = Regex.Replace(value, @"<script[^>]*?>.*?</script>", "", RegexOptions.IgnoreCase);

                //删除HTML
                value = Regex.Replace(value, @"<(.[^>]*)>", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, @"([\r\n])[\s]+", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, @"-->", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, @"<!--.*", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, @"&(quot|#34);", "\"", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, @"&(amp|#38);", "&", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, @"&(lt|#60);", "<", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, @"&(gt|#62);", ">", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, @"&(nbsp|#160);", " ", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, @"&(iexcl|#161);", "\xa1", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, @"&(cent|#162);", "\xa2", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, @"&(pound|#163);", "\xa3", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, @"&(copy|#169);", "\xa9", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, @"&#(\d+);", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, "xp_cmdshell", "", RegexOptions.IgnoreCase);

                //删除与数据库相关的词
                value = Regex.Replace(value, "select", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, "insert", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, "delete from", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, "count''", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, "drop table", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, "truncate", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, "asc", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, "mid", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, "char", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, "xp_cmdshell", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, "exec master", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, "net localgroup administrators", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, "and", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, "net user", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, "or", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, "net", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, "-", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, "delete", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, "drop", "", RegexOptions.IgnoreCase);
                value = Regex.Replace(value, "script", "", RegexOptions.IgnoreCase);

                //特殊的字符
                value = value.Replace("<", "");
                value = value.Replace(">", "");
                value = value.Replace("*", "");
                value = value.Replace("-", "");
                value = value.Replace("?", "");
                value = value.Replace("'", "''");
                value = value.Replace(",", "");
                value = value.Replace("/", "");
                value = value.Replace(";", "");
                value = value.Replace("*/", "");
                value = value.Replace("\r\n", "");

                return value;
            }
        }
    }
}
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

                //特殊的字符
                value = value.Replace("<", "");
                value = value.Replace(">", "");
                value = value.Replace("*", "");
                value = value.Replace("-", "");
                value = value.Replace("?", "");
                value = value.Replace("'", "''");
                value = value.Replace(",", "");
                //value = value.Replace("/", "");
                value = value.Replace(";", "");
                value = value.Replace("*/", "");
                value = value.Replace("\r\n", "");

                return value;
            }
        }
    }
}
using System;

namespace Helper
{
    public static class StrUtils
    {
        public static bool StartsWithAt(string subject, string @object, int pos)
        {
            if (subject.Length - pos < @object.Length)
            {
                return false;

            }

            int n = @object.Length;
            for (int i = 0; i < n; ++i)
            {
                if (!subject[pos + i].Equals(@object[i]))
                    return false;
            }
            return true;
        }

        public static int StringCount(string subject, string @object)
        {
            int sub_len = @object.Length;
            if (sub_len == 0)
            {
                return 0;
            }

            int count = 0;
            int n = subject.Length - sub_len + 1;
            for (int i = 0; i < n; ++i)
            {
                if (StartsWithAt(subject, @object, i))
                {
                    count++;
                }
            }
            return count;
        }
        public static int StringCount(string subject, char @object)
        {
            int count = 0;
            int n = subject.Length;
            for (int i = 0; i < n; ++i)
            {
                if (subject[i] == @object)
                {
                    count++;
                }
            }
            return count;
        }

        public static int StringFindIndexRight(string subject, char @object)
        {
            int n = subject.Length;
            if (n == 0)
            {
                return -1;
            }
            for (int i = n - 1; i >= 0; --i)
            {
                if (subject[i] == @object)
                {
                    return i;
                }
            }
            return -1;
        }
    }
}

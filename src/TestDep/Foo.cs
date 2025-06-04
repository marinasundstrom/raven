namespace TestDep;

using System;

public class Foo
{
    public static void Test([TypeUnion(typeof(int), typeof(bool))] object y)
    {
        if (y is int o)
        {
            Console.WriteLine(o);
        }
        else if (y is bool z)
        {
            Console.WriteLine(z);
        }
    }

    [return: TypeUnion(typeof(int), typeof(bool))]
    public static object Test2(bool flag)
    {
        return flag ? 42 : false;
    }

    [return: TypeUnion(typeof(int), typeof(bool))]
    public static object Test3(bool flag)
    {
        if (flag)
        {
            return 1;
        }

        return 2;
    }
}

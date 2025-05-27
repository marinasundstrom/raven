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
        return false ? 42 : false;
    }
}

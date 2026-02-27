namespace TestDep;

using System;

public class Foo
{
    public static void Test(object? y)
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

    public static object TestProp { get; set; } = default!;

    public static object Test2(bool flag)
    {
        return flag ? 42 : false;
        //return flag ? 42 : "str";
    }

    public static object Test3(bool flag)
    {
        if (flag)
        {
            return 1;
        }

        return true;
        //return 2f;
    }

    public static object? Test4(bool flag)
    {
        return flag ? 42 : null;
    }

    public static void Test5(object v)
    {

    }

    public static void Test6(string? v)
    {

    }
}

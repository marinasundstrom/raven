namespace TestDep;

using System;

public class Foo
{
    public static void Test([TypeUnion(typeof(int), typeof(bool))] object? y)
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

    [TypeUnion(typeof(string), typeof(int))]
    public static object TestProp { get; set; }

    [return: TypeUnion(typeof(int), typeof(bool))]
    public static object Test2(bool flag)
    {
        return flag ? 42 : false;
        //return flag ? 42 : "str";
    }

    [return: TypeUnion(typeof(int), typeof(bool))]
    public static object Test3(bool flag)
    {
        if (flag)
        {
            return 1;
        }

        return true;
        //return 2f;
    }

    [return: TypeUnion(typeof(int), typeof(Null))]
    public static object? Test4(bool flag)
    {
        return flag ? 42 : null;
    }

    public static void Test5([TypeUnion("yes", "no")] object v)
    {

    }

    public static void Test6([TypeUnion("yes", "no", typeof(Null))] string? v)
    {

    }
}

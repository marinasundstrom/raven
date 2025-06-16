namespace TestDep;

class Test
{
    public void Hello()
    {
        Foo.Test(true);

        var x = Foo.Test2(true);

        Foo.TestProp = 2;
    }

    public static void World()
    {
        List<int> list = new List<int>();
        list.Add(2);
        list.Add(3);
        Console.WriteLine(list.Count);
        for (int i = 0; i < list.Count; i++)
        {
            Console.WriteLine(list[i].ToString());
        }
    }
}
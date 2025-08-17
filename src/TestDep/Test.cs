namespace TestDep;

class Test
{
    public void Hello()
    {
        Foo.Test(1);
        //Foo.Test("");

        var x = Foo.Test2(true);

        Foo.TestProp = 1;
        //Foo.TestProp = false;
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
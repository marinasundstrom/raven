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

        var y = Foo.Test4(true);
    }

    public static void World()
    {
        List<int> list = [2, 3];
        Console.WriteLine(list.Count);
        for (int i = 0; i < list.Count; i++)
        {
            Console.WriteLine(list[i].ToString());
        }
    }
}

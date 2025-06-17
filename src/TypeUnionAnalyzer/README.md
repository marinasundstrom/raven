# Type Union Analyzer for C#

The purpose of this analyzer is to enable the `TypeUnionAttribute` for C#, and to allow for basic type checking. 

It will help interop with Raven's type unions.

For every usage of an attribute with signature `[TypeUnionAttribute(params Type[] types)]`, diagnostics will be reported so to discover them more easily.

Since type unions are built on `object`, similar to type `dynamic`, it is not possible to overload on parameter representing type unions.

If you want to, you can use `dynamic`, because it is basically `object`.

## Samples

### Method

Parameters:

```csharp
class Test 
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
}
```

Return parameter:

```csharp
class Test 
{
    [return: TypeUnion(typeof(int), typeof(bool))]
    public static object Method(bool flag)
    {
        if (flag)
        {
            return 1;
        }

        return true;
    }
}
```

You can of course do both at the same time.

### Property

```csharp
class Test 
{
    [TypeUnion(typeof(string), typeof(int))]
    public static object Prop { get; set; }
}
```

### Field

```csharp
class Test 
{
    [TypeUnion(typeof(string), typeof(int))]
    public static object Field = 1;
}
```
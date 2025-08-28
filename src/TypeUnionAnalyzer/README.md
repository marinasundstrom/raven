# Type Union Analyzer for C#

This Roslyn analyzer runs inside C# projects to validate usages of Raven's
`[TypeUnion]` attribute and provide basic type checking. It helps interop
with type unions declared in Raven.

Unlike `MissingReturnTypeAnnotationAnalyzer`, which targets the Raven language
and serves as a reference implementation for Raven analyzers, `TypeUnionAnalyzer`
focuses solely on C# code that consumes Raven-generated unions.

For every usage of an attribute with signature `[TypeUnionAttribute(params Type[] types)]`, diagnostics will be reported so to discover them more easily.

Since type unions are built on `object`, similar to type `dynamic`, it is not possible to overload on parameter representing type unions.

If you want to, you can use `dynamic`, because it is basically `object`.

## Union semantics

`TypeUnionAttribute` lists the set of concrete types that a value may take. Any parameter,
field, property, or return annotated with `[TypeUnion]` must have a CLR type of `object` or
`dynamic`. At each usage site the analyzer verifies that the value assigned, returned, or
matched against the member implicitly converts to at least one of the allowed types. Pattern
matching and switch sections are checked in the same way. `null` is only permitted when
`typeof(void)` (representing the `null` type) is explicitly included in the attribute's type
list.

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

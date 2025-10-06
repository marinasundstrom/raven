namespace System.Reflection2;

using System;

internal readonly struct MetadataCustomModifiers
{
    public static readonly MetadataCustomModifiers Empty = new(Type.EmptyTypes, Type.EmptyTypes);

    public MetadataCustomModifiers(Type[] required, Type[] optional)
    {
        Required = required.Length == 0 ? Type.EmptyTypes : required;
        Optional = optional.Length == 0 ? Type.EmptyTypes : optional;
    }

    public Type[] Required { get; }

    public Type[] Optional { get; }
}

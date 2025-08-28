namespace TestDep;

using System;

[AttributeUsage(AttributeTargets.Parameter | AttributeTargets.Field | AttributeTargets.ReturnValue | AttributeTargets.Property)]
public sealed class TypeUnionAttribute : global::System.Attribute
{
    private readonly object[] _types;

    public object[] Types => _types;

    public TypeUnionAttribute(params object[] types)
    {
        _types = types;
    }
}

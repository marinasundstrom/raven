namespace TestDep;

using System;

[AttributeUsage(AttributeTargets.Parameter | AttributeTargets.Field | AttributeTargets.ReturnValue | AttributeTargets.Property)]
public sealed class TypeUnionAttribute : global::System.Attribute
{
    private readonly Type[] _types;

    public Type[] Types => _types;

    public TypeUnionAttribute(params Type[] types)
    {
        _types = types;
    }
}

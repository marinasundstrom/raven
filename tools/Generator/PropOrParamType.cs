using Microsoft.CodeAnalysis;

namespace Generator;

public class PropOrParamType(string name, string type, string? targetName = null, ITypeSymbol? typeSymbol = null)
{
    public string Name { get; } = name;
    public string? TargetName { get; } = targetName;
    public ITypeSymbol? TypeSymbol { get; } = typeSymbol;
    public string Type { get; } = type;
}
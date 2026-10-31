using Raven.CodeAnalysis.Targets;

namespace Raven.CodeAnalysis.Symbols;

// A structural member, not a nominal interface constraint or a source declaration.
internal sealed class StructuralFunctionPropertySymbol : SourcePropertySymbol
{
    internal StructuralFunctionPropertySymbol(ITypeSymbol owner, RuntimeTypeOfBinding binding)
        : base("Function", binding.Type, owner, owner as INamedTypeSymbol, null, [], [],
            declaredAccessibility: Accessibility.Public)
    {
        Binding = binding;
        var getter = new SourceMethodSymbol("get_Function", binding.Type, [], this,
            owner as INamedTypeSymbol, null, [], [], isStatic: false,
            declaredAccessibility: Accessibility.Public);
        SetAccessors(getter, null);
    }

    internal RuntimeTypeOfBinding Binding { get; }
    public override bool IsImplicitlyDeclared => true;
}

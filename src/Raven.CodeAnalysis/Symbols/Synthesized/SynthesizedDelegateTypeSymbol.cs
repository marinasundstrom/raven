using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class SynthesizedDelegateTypeSymbol : SourceNamedTypeSymbol
{
    private static readonly Location[] s_emptyLocations = Array.Empty<Location>();
    private static readonly SyntaxReference[] s_emptySyntax = Array.Empty<SyntaxReference>();

    public SynthesizedDelegateTypeSymbol(
        Compilation compilation,
        string name,
        ImmutableArray<ITypeSymbol> parameterTypes,
        ImmutableArray<RefKind> refKinds,
        ITypeSymbol returnType,
        SourceNamespaceSymbol containingNamespace)
        : base(
            name,
            (INamedTypeSymbol)compilation.GetSpecialType(SpecialType.System_MulticastDelegate),
            TypeKind.Delegate,
            containingNamespace,
            null,
            containingNamespace,
            s_emptyLocations,
            s_emptySyntax,
            isSealed: true)
    {
        ReturnType = returnType;
        ParameterTypes = parameterTypes;
        ParameterRefKinds = refKinds;

        Constructor = CreateConstructor(compilation, containingNamespace);
        InvokeMethod = CreateInvokeMethod(returnType, containingNamespace, parameterTypes, refKinds);
    }

    public SourceMethodSymbol Constructor { get; }

    public SourceMethodSymbol InvokeMethod { get; }

    public ITypeSymbol ReturnType { get; }

    public ImmutableArray<ITypeSymbol> ParameterTypes { get; }

    public ImmutableArray<RefKind> ParameterRefKinds { get; }

    public override Accessibility DeclaredAccessibility => Accessibility.Internal;

    public override bool IsImplicitlyDeclared => true;

    private SourceMethodSymbol CreateConstructor(Compilation compilation, SourceNamespaceSymbol containingNamespace)
    {
        var constructor = new SourceMethodSymbol(
            ".ctor",
            compilation.GetSpecialType(SpecialType.System_Void),
            ImmutableArray<SourceParameterSymbol>.Empty,
            this,
            this,
            containingNamespace,
            s_emptyLocations,
            s_emptySyntax,
            isStatic: false,
            methodKind: MethodKind.Constructor);

        var parameters = new List<SourceParameterSymbol>
        {
            new SourceParameterSymbol(
                "target",
                compilation.GetSpecialType(SpecialType.System_Object),
                constructor,
                this,
                containingNamespace,
                s_emptyLocations,
                s_emptySyntax),
            new SourceParameterSymbol(
                "method",
                compilation.GetSpecialType(SpecialType.System_IntPtr),
                constructor,
                this,
                containingNamespace,
                s_emptyLocations,
                s_emptySyntax)
        };

        constructor.SetParameters(parameters);
        return constructor;
    }

    private SourceMethodSymbol CreateInvokeMethod(
        ITypeSymbol returnType,
        SourceNamespaceSymbol containingNamespace,
        ImmutableArray<ITypeSymbol> parameterTypes,
        ImmutableArray<RefKind> refKinds)
    {
        var invoke = new SourceMethodSymbol(
            "Invoke",
            returnType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            this,
            this,
            containingNamespace,
            s_emptyLocations,
            s_emptySyntax,
            isStatic: false,
            methodKind: MethodKind.Ordinary,
            isVirtual: true);

        if (!parameterTypes.IsDefaultOrEmpty)
        {
            var parameters = new List<SourceParameterSymbol>(parameterTypes.Length);
            for (var i = 0; i < parameterTypes.Length; i++)
            {
                parameters.Add(new SourceParameterSymbol(
                    $"arg{i}",
                    parameterTypes[i],
                    invoke,
                    this,
                    containingNamespace,
                    s_emptyLocations,
                    s_emptySyntax,
                    refKinds.IsDefaultOrEmpty ? RefKind.None : refKinds[i]));
            }

            invoke.SetParameters(parameters);
        }

        return invoke;
    }
}

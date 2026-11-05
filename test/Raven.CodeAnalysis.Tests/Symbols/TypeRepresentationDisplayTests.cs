using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class TypeRepresentationDisplayTests
{
    [Fact]
    public void NullableAndByRefViewsFormatWithoutConcreteWrapperClasses()
    {
        var compilation = CreateCompilation();
        var integer = compilation.GetSpecialType(SpecialType.System_Int32);
        var nullable = new NullableView(compilation, integer);
        Assert.Equal(integer.GetNullableType().ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat),
            nullable.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Same(integer, nullable.GetNullableUnderlyingType());
        Assert.Equal(new RefTypeSymbol(integer).ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat),
            new ByRefView(compilation, integer).ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void LiteralViewsPreserveDisplayAndUnderlyingErrorQueries()
    {
        var compilation = CreateCompilation();
        var literal = new LiteralView(compilation, compilation.ErrorTypeSymbol);
        Assert.Equal("42", literal.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.True(literal.ContainsErrorType());
    }

    [Fact]
    public void SynthesizedDelegateViewsFormatTheirSignatureWithoutConcreteStorage()
    {
        var compilation = CreateCompilation();
        var integer = compilation.GetSpecialType(SpecialType.System_Int32);
        var boolean = compilation.GetSpecialType(SpecialType.System_Boolean);
        var expected = new SynthesizedDelegateTypeSymbol(compilation, "Expected", [integer], [RefKind.None], boolean, compilation.SourceGlobalNamespace);
        var actual = new DelegateView(compilation, integer, boolean);
        Assert.Equal(expected.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat),
            actual.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    private static Compilation CreateCompilation()
        => Compilation.Create("DisplayViews", [], TestMetadataReferences.Default, CompilationOptions.DotNet);

    private abstract class View(Compilation compilation, string name, TypeKind kind)
        : SourceNamedTypeSymbol(name, compilation.GetSpecialType(SpecialType.System_Object), kind,
            compilation.Assembly, null, compilation.SourceGlobalNamespace, [], [], addAsMember: false);

    private sealed class NullableView(Compilation compilation, TypeSymbol underlyingType)
        : View(compilation, "NullableView", TypeKind.Class), INullableTypeIdentity
    {
        public TypeSymbol UnderlyingType { get; } = underlyingType;
    }

    private sealed class ByRefView(Compilation compilation, TypeSymbol elementType)
        : View(compilation, "ByRefView", TypeKind.Class), IByRefTypeInfo
    {
        public TypeSymbol ElementType { get; } = elementType;
    }

    private sealed class LiteralView(Compilation compilation, TypeSymbol underlyingType)
        : View(compilation, "42", TypeKind.Struct), ILiteralTypeInfo
    {
        public TypeSymbol UnderlyingType { get; } = underlyingType;
    }

    private sealed class DelegateView(Compilation compilation, TypeSymbol parameter, TypeSymbol result)
        : View(compilation, "DelegateView", TypeKind.Delegate), ISynthesizedDelegateSignature
    {
        public ImmutableArray<TypeSymbol> ParameterTypes { get; } = [parameter];
        public ImmutableArray<RefKind> ParameterRefKinds { get; } = [RefKind.None];
        public TypeSymbol ReturnType { get; } = result;
    }
}

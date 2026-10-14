using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Symbols;

public sealed class GenericCompositionSymbolTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ClosingOwner_SubstitutesNestedArgumentsAndCollapsesDuplicateBounds(bool intersection)
    {
        var c = Create();
        var owner = c.GetTypeByMetadataName("Owner`1")!;
        var face = c.GetTypeByMetadataName("Face`1")!;
        var box = c.GetTypeByMetadataName("Box`1")!;
        var text = c.GetSpecialType(SpecialType.System_String);
        var open = face.Construct(box.Construct(owner.TypeParameters[0]));
        var closed = face.Construct(box.Construct(text));
        var shape = Compose(c, intersection, open, closed);
        var constructed = Assert.IsType<ConstructedNamedTypeSymbol>(owner.Construct(text));
        Assert.True(SymbolEqualityComparer.Default.Equals(closed, constructed.Substitute(shape)));
        var bounds = intersection
            ? ((IIntersectionTypeSymbol)shape).ConstituentTypes
            : ((IStructuralUnionTypeSymbol)shape).ConstituentTypes;
        Assert.Equal(open, bounds[0]);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ClosingMethod_SubstitutesResultShape(bool intersection)
    {
        var c = Create();
        var owner = c.GetTypeByMetadataName("Anchor")!;
        var face = c.GetTypeByMetadataName("Face`1")!;
        var text = c.GetSpecialType(SpecialType.System_String);
        var definition = new SourceMethodSymbol("Project", owner, [], owner.ContainingNamespace!, null, null, [], []);
        var parameter = new SourceTypeParameterSymbol("T", definition, null, null, [], [], 0,
            TypeParameterConstraintKind.None, [], VarianceKind.None);
        definition.SetTypeParameters([parameter]);
        var shape = Compose(c, intersection, face.Construct(parameter), face.Construct(text));
        definition.SetReturnType(shape);
        var constructed = new ConstructedMethodSymbol(definition, [text]);
        Assert.True(SymbolEqualityComparer.Default.Equals(face.Construct(text), constructed.ReturnType));
        Assert.Same(shape, definition.ReturnType);
    }

    [Fact]
    public void OpenUnion_IdentityIncludesParameterBinderAndUnchangedSubstitutionReusesSymbol()
    {
        var c = Create();
        var face = c.GetTypeByMetadataName("Face`1")!;
        var first = c.GetTypeByMetadataName("Owner`1")!.TypeParameters[0];
        var second = c.GetTypeByMetadataName("Box`1")!.TypeParameters[0];
        var anchor = c.GetTypeByMetadataName("Anchor")!;
        var left = (IStructuralUnionTypeSymbol)c.CreateStructuralUnionTypeSymbol(face.Construct(first), anchor);
        var right = c.CreateStructuralUnionTypeSymbol(face.Construct(second), anchor);
        Assert.False(SymbolEqualityComparer.Default.Equals(left, right));
        Assert.Same(left, TypeSubstitution.SubstituteStructuralUnion(left, type => type));
        Assert.IsAssignableFrom<IStructuralUnionTypeSymbol>(c.CreateStructuralUnionTypeSymbol(first, anchor));
    }

    [Theory]
    [InlineData(null, false)]
    [InlineData("Request`1", false)]
    [InlineData(null, true)]
    [InlineData("Response`1", true)]
    public void AuthorExamples_SubstituteValueArgumentsInsideAndOutsideUnion(string? wrapper, bool task)
    {
        var c = Create();
        var owner = c.GetTypeByMetadataName("Owner`1")!;
        var parameter = owner.TypeParameters[0];
        var http = c.GetTypeByMetadataName("HttpError")!;
        var json = c.GetTypeByMetadataName("JsonError")!;
        var request = wrapper is not null;
        var requestType = wrapper is null ? null : c.GetTypeByMetadataName(wrapper)!;
        ITypeSymbol first = request ? requestType!.Construct(parameter) : parameter;
        var union = c.CreateStructuralUnionTypeSymbol(first, http, json);
        var taskType = c.GetTypeByMetadataName("System.Threading.Tasks.Task`1")!;
        ITypeSymbol expression = task ? taskType.Construct(union) : union;
        var integer = c.GetSpecialType(SpecialType.System_Int32);
        var substituted = ((ConstructedNamedTypeSymbol)owner.Construct(integer)).Substitute(expression);
        var result = Assert.IsAssignableFrom<IStructuralUnionTypeSymbol>(task
            ? Assert.IsAssignableFrom<INamedTypeSymbol>(substituted).TypeArguments[0] : substituted);
        ITypeSymbol expected = request ? requestType!.Construct(integer) : integer;
        Assert.True(SymbolEqualityComparer.Default.Equals(expected, result.ConstituentTypes[0]));
        Assert.Equal(new[] { http, json }, result.ConstituentTypes.Skip(1));
        Assert.Contains("HttpError | JsonError", expression.ToDisplayString());
        Assert.False(union.IsReferenceType);
        if (request) Assert.False(result.IsValueType); // Mixed reference/value alternatives have no chosen storage category.
        else Assert.True(result.IsValueType);
        // Closing T over an existing alternative deduplicates at any nesting level.
        if (!request)
        {
            var repeated = ((ConstructedNamedTypeSymbol)owner.Construct(http)).Substitute(expression);
            var reduced = Assert.IsAssignableFrom<IStructuralUnionTypeSymbol>(task
                ? Assert.IsAssignableFrom<INamedTypeSymbol>(repeated).TypeArguments[0] : repeated);
            Assert.Equal(2, reduced.ConstituentTypes.Length);
        }
    }

    private static ITypeSymbol Compose(Compilation c, bool intersection, params ITypeSymbol[] bounds) =>
        intersection ? c.CreateIntersectionTypeSymbol(bounds) : c.CreateStructuralUnionTypeSymbol(bounds);

    private static Compilation Create()
    {
        var c = Compilation.Create("generic-compositions", [SyntaxTree.ParseText("""
            interface Face<T> {}
            interface Anchor {}
            class Owner<T> {}
            class Box<T> {}
            class Request<T> {}
            class Response<T> {}
            struct HttpError {}
            struct JsonError {}
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(c.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        return c;
    }
}

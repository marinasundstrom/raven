using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class IteratorCancellationContractTests
{
    [Fact]
    public void CandidatesRetainParameterOrderAndRequireTokenType()
    {
        var (services, token) = Context();
        var first = Parameter(token);
        var marked = Parameter(token, "EnumeratorCancellation");
        var wrongType = Parameter(Make<TypeSymbol>(SymbolKind.Type, "Other"), "EnumeratorCancellation");
        var method = Method(true, first, wrongType, marked);
        Assert.Equal(new[] { first, marked }, AsyncIteratorCancellationUtilities.GetCancellationTokenParameters(services, method));
        Assert.Same(marked, Assert.Single(AsyncIteratorCancellationUtilities.GetEnumeratorCancellationParameters(services, method)));
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(2)]
    public void EffectiveParameterRequiresExactlyOneCandidate(int count)
    {
        var (services, token) = Context();
        var parameters = Enumerable.Range(0, count).Select(_ => Parameter(token, "EnumeratorCancellation")).ToArray();
        var result = AsyncIteratorCancellationUtilities.GetEffectiveEnumeratorCancellationParameter(services, Method(true, parameters), IteratorMethodKind.AsyncEnumerable);
        Assert.Same(count == 1 ? parameters[0] : null, result);
    }

    [Theory]
    [InlineData(false, IteratorMethodKind.AsyncEnumerable)]
    [InlineData(true, IteratorMethodKind.None)]
    [InlineData(false, IteratorMethodKind.None)]
    public void IneligibleMethodsDoNotSelectOrWarn(bool async, IteratorMethodKind kind)
    {
        var (services, token) = Context();
        var method = Method(async, Parameter(token));
        Assert.Null(AsyncIteratorCancellationUtilities.GetEffectiveEnumeratorCancellationParameter(services, method, kind));
        Assert.False(AsyncIteratorCancellationUtilities.ShouldWarnAboutMissingEnumeratorCancellation(services, method, kind));
    }

    [Fact]
    public void WarningRequiresUnannotatedTokenAndNoCandidate()
    {
        var (services, token) = Context();
        Assert.True(AsyncIteratorCancellationUtilities.ShouldWarnAboutMissingEnumeratorCancellation(services, Method(true, Parameter(token)), IteratorMethodKind.AsyncEnumerable));
        Assert.False(AsyncIteratorCancellationUtilities.ShouldWarnAboutMissingEnumeratorCancellation(services, Method(true), IteratorMethodKind.AsyncEnumerable));
        Assert.False(AsyncIteratorCancellationUtilities.ShouldWarnAboutMissingEnumeratorCancellation(services, Method(true, Parameter(token), Parameter(token, "EnumeratorCancellation")), IteratorMethodKind.AsyncEnumerable));
        Set(services, "GetTypeByMetadataName", null);
        Assert.Empty(AsyncIteratorCancellationUtilities.GetCancellationTokenParameters(services, Method(true, Parameter(token))));
    }

    [Theory]
    [InlineData("EnumeratorCancellation", true)]
    [InlineData("EnumeratorCancellationAttribute", true)]
    [InlineData("System.Runtime.CompilerServices.EnumeratorCancellation", true)]
    [InlineData("System.Runtime.CompilerServices.EnumeratorCancellationAttribute", true)]
    [InlineData("Other", false)]
    public void SyntaxFallbackRecognizesExistingSpellings(string attribute, bool expected)
    {
        var (_, token) = Context();
        Assert.Equal(expected, AsyncIteratorCancellationUtilities.HasEnumeratorCancellationAttribute(Parameter(token, attribute)));
    }

    [Fact]
    public void BoundAttributeUsesQualifiedMetadataIdentity()
    {
        var (_, token) = Context();
        var attribute = Make<NamedTypeSymbol>(SymbolKind.Type, "EnumeratorCancellationAttribute");
        Set(attribute, "get_ContainingNamespace", Namespace("System.Runtime.CompilerServices"));
        var parameter = Parameter(token);
        Set(parameter, "GetAttributes", ImmutableArray.Create(new AttributeData(attribute, Make<MethodSymbol>(SymbolKind.Method, ".ctor"), [], [], null)));
        Assert.True(AsyncIteratorCancellationUtilities.HasEnumeratorCancellationAttribute(parameter));
        Set(attribute, "get_ContainingNamespace", Namespace("Other"));
        Assert.False(AsyncIteratorCancellationUtilities.HasEnumeratorCancellationAttribute(parameter));
    }

    [Fact]
    public void AsyncEnumerableUsesDefinitionAndExactNamespace()
    {
        var (services, _) = Context();
        var definition = Make<NamedTypeSymbol>(SymbolKind.Type, "IAsyncEnumerable`1");
        Set(definition, "get_ContainingNamespace", Namespace("System.Collections.Generic"));
        var constructed = Make<NamedTypeSymbol>(SymbolKind.Type, "Constructed");
        Set(constructed, "get_ConstructedFrom", definition);
        Assert.True(AsyncIteratorCancellationUtilities.IsAsyncEnumerable(services, constructed));
        Set(definition, "get_ContainingNamespace", Namespace("Other.System.Collections.Generic"));
        Assert.False(AsyncIteratorCancellationUtilities.IsAsyncEnumerable(services, constructed));
        Set(definition, "get_ContainingNamespace", null);
        Assert.False(AsyncIteratorCancellationUtilities.IsAsyncEnumerable(services, constructed));
        Assert.False(AsyncIteratorCancellationUtilities.IsAsyncEnumerable(services, Make<TypeSymbol>(SymbolKind.Type, "Other")));
    }

    private static (AttributeValidationServices Services, NamedTypeSymbol Token) Context()
    {
        var token = Make<NamedTypeSymbol>(SymbolKind.Type, "CancellationToken", TypeKind.Struct);
        var services = Make<AttributeValidationServices>(SymbolKind.Namespace, "Services");
        Set(services, "GetTypeByMetadataName", (Func<object?[]?, object?>)(args =>
        {
            Assert.Equal("System.Threading.CancellationToken", args![0]);
            return token;
        }));
        return (services, token);
    }

    private static ParameterSymbol Parameter(TypeSymbol type, string? attribute = null)
    {
        var parameter = Make<ParameterSymbol>(SymbolKind.Parameter, "token");
        Set(parameter, "get_Type", type);
        Set(parameter, "GetAttributes", ImmutableArray<AttributeData>.Empty);
        var references = ImmutableArray<SyntaxReference>.Empty;
        if (attribute is not null)
        {
            var tree = SyntaxTree.ParseText($"func F([{attribute}] token: Token) {{}}");
            Assert.Empty(tree.GetDiagnostics());
            references = [tree.GetRoot().DescendantNodes().OfType<ParameterSyntax>().Single().GetReference()];
        }
        Set(parameter, "get_DeclaringSyntaxReferences", references);
        return parameter;
    }

    private static MethodSymbol Method(bool async, params ParameterSymbol[] parameters)
    {
        var method = Make<MethodSymbol>(SymbolKind.Method, "Stream");
        Set(method, "get_IsAsync", async);
        Set(method, "get_Parameters", parameters.ToImmutableArray());
        return method;
    }

    private static NamespaceSymbol Namespace(string name)
    {
        var current = Make<NamespaceSymbol>(SymbolKind.Namespace, "");
        Set(current, "get_IsGlobalNamespace", true);
        Set(current, "get_ContainingNamespace", null);
        foreach (var part in name.Split('.'))
        {
            var next = Make<NamespaceSymbol>(SymbolKind.Namespace, part);
            Set(next, "get_IsGlobalNamespace", false);
            Set(next, "get_ContainingNamespace", current);
            current = next;
        }
        return current;
    }
}

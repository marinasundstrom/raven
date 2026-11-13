using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class SourceMethodPortContractTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void TerminalPolicyComesFromDeclaringServices(bool terminal)
    {
        var services = Make<SourceMethodDeclarationServices>(SymbolKind.Namespace, "Services");
        var assembly = Make<DeclarationAssembly>(SymbolKind.Assembly, "Assembly");
        Set(assembly, "get_DeclarationServices", services);
        var ns = Make<NamespaceSymbol>(SymbolKind.Namespace, "Root");
        Set(ns, "get_ContainingAssembly", assembly);
        var method = new SourceMethodSymbol("Run", Named("Test", "Result"), [], assembly, null, ns, [], []);
        Set(services, "IsTerminalRuntimeCall", (Func<object?[]?, object?>)(arguments =>
        {
            Assert.Same(method, arguments![0]);
            return terminal;
        }));
        Assert.Equal(terminal, method.IsTerminalRuntimeCall);
    }

    internal interface DeclarationAssembly : AssemblySymbol, SourceDeclarationServiceProvider { }

    [Fact]
    public void CapturesFollowLateBoundCalleesAndTerminateCycles()
    {
        var first = Method("First");
        var second = Method("Second");
        var capture = Local("outer");
        first.SetLocalFunctionCaptures([], [second]);
        Assert.Empty(first.CapturedVariables);
        second.SetLocalFunctionCaptures([capture], [first]);
        Assert.Same(capture, Assert.Single(first.CapturedVariables));
        Assert.Same(capture, Assert.Single(second.CapturedVariables));
        second.SetLocalFunctionCaptures([], []);
        Assert.Empty(first.CapturedVariables);
        Assert.False(first.HasCaptures);
    }

    [Fact]
    public void ForwardedCaptureExcludesLocalsDeclaredInsideCaller()
    {
        var first = Method("First");
        var second = Method("Second");
        var local = Local("own");
        Set(local, "get_ContainingSymbol", first);
        second.SetLocalFunctionCaptures([local], []);
        first.SetLocalFunctionCaptures([], [second]);
        Assert.Empty(first.CapturedVariables);
        Assert.Same(local, Assert.Single(second.CapturedVariables));
    }

    [Fact]
    public void RebindingEquivalentParametersRetainsTheirIdentity()
    {
        var method = Method("F");
        var type = Named("Test", "Argument");
        var reference = SyntaxTree.ParseText("func F(value: int) {}").GetRoot().DescendantNodes().OfType<ParameterSyntax>().Single().GetReference();
        var original = Parameter(method, type, reference);
        method.SetParameters([original]);
        var version = method.SignatureVersion;
        method.SetParameters([Parameter(method, type, reference)]);
        Assert.Same(original, Assert.Single(method.Parameters));
        Assert.Equal(version, method.SignatureVersion);
        var changed = Parameter(method, Named("Test", "Other"), reference);
        method.SetParameters([changed]);
        Assert.Same(changed, Assert.Single(method.Parameters));
        Assert.True(method.SignatureVersion > version);
    }

    [Fact]
    public void AsyncInferenceAndIteratorFlagsRetainTransitions()
    {
        var method = Method("F");
        Assert.False(method.ShouldDeferAsyncReturnDiagnostics);
        method.CompleteAsyncReturnTypeInference();
        Assert.False(method.AsyncReturnTypeInferenceComplete);
        method.RequireAsyncReturnTypeInference();
        Assert.True(method.ShouldDeferAsyncReturnDiagnostics);
        method.CompleteAsyncReturnTypeInference();
        Assert.True(method.AsyncReturnTypeInferenceComplete);
        Assert.False(method.ShouldDeferAsyncReturnDiagnostics);
        var element = Named("Test", "Item");
        method.MarkIterator(IteratorMethodKind.None, element);
        Assert.False(method.IsIterator);
        method.MarkIterator(IteratorMethodKind.Enumerable, element);
        Assert.True(method.IsIterator);
        Assert.Equal(IteratorMethodKind.Enumerable, method.IteratorKind);
        Assert.Same(element, method.IteratorElementType);
    }

    [Fact]
    public void EmptyReturnAttributesAndCachedClassificationRemainStable()
    {
        var method = Method("F");
        for (var i = 0; i < 2; i++)
        {
            Assert.Empty(method.GetReturnTypeAttributes());
            Assert.False(method.IsExtensionMethod);
            Assert.False(method.IsUnsafe);
            Assert.False(method.IsPrimaryConstructorSymbol);
            Assert.False(method.IsTerminalRuntimeCall);
        }
        method.MarkDeclaredInExtension();
        Assert.True(method.IsExtensionMethod);
    }

    [Fact]
    public void LambdaIdentityAndNestedNamePreserveContainingMethod()
    {
        var method = Method("Run");
        var result = Named("Test", "Result");
        var lambda = new SourceLambdaSymbol([], result, method, isAsync: true);
        var nested = new SourceLambdaSymbol([], result, lambda);
        Assert.Equal("<Run>b__0", lambda.Name);
        Assert.Equal("<Run>b__0_0", nested.Name);
        Assert.Equal(MethodKind.LambdaMethod, lambda.MethodKind);
        Assert.False(lambda.IsStatic);
        Assert.True(lambda.IsAsync);
        Assert.Same(lambda, lambda.OriginalDefinition);
        Assert.Same(lambda, lambda.ConstructedFrom);
        Assert.Empty(lambda.GetReturnTypeAttributes());
    }

    [Fact]
    public void LambdaNamesUseOrderedDeclarationLocations()
    {
        var tree = SyntaxTree.ParseText("""
            func Run() {
                let first = func() => 1
                let second = func() => 2
            }
            """);
        var declaration = Assert.Single(tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>());
        var expressions = declaration.DescendantNodes().OfType<FunctionExpressionSyntax>().ToArray();
        Assert.Equal(2, expressions.Length);
        var result = Named("Test", "Result");
        var method = new SourceMethodSymbol("Run", result, [], Make<Symbol>(SymbolKind.Namespace, "Root"),
            null, null, [], [declaration.GetReference()]);
        var first = new SourceLambdaSymbol([], result, method, locations: [expressions[0].GetLocation()]);
        var second = new SourceLambdaSymbol([], result, method, locations: [expressions[1].GetLocation()]);
        Assert.Equal("<Run>b__0", first.Name);
        Assert.Equal("<Run>b__1", second.Name);
    }

    [Fact]
    public void LambdaCapturesAndReturnTypeRemainExplicitlyMutable()
    {
        var method = Method("Run");
        var result = Named("Test", "Result");
        var lambda = new SourceLambdaSymbol([], result, method);
        var capture = Local("outer");
        lambda.SetCapturedVariables([capture]);
        Assert.True(lambda.HasCaptures);
        Assert.Same(capture, Assert.Single(lambda.CapturedVariables));
        lambda.SetCapturedVariables([]);
        Assert.False(lambda.HasCaptures);
        var replacement = Named("Test", "Replacement");
        lambda.SetReturnType(replacement);
        Assert.Same(replacement, lambda.ReturnType);
        lambda.SetDelegateType(result);
        Assert.Same(result, lambda.DelegateType);
        lambda.SetContainsAwait(true);
        Assert.True(lambda.ContainsAwait);
    }

    private static SourceMethodSymbol Method(string name)
        => new(name, Named("Test", "Result"), [], Make<Symbol>(SymbolKind.Namespace, "Root"), null, null, [], []);

    private static SourceParameterSymbol Parameter(SourceMethodSymbol method, TypeSymbol type, SyntaxReference reference)
        => new("value", type, method, null, null, [], [reference]);

    private static LocalSymbol Local(string name)
    {
        var local = Make<LocalSymbol>(SymbolKind.Local, name);
        var type = Make<TypeSymbol>(SymbolKind.Type, "int", TypeKind.Struct);
        Set(type, "get_SpecialType", SpecialType.System_Int32);
        Set(local, "get_Type", type);
        Set(local, "get_IsMutable", false);
        Set(local, "get_IsConst", false);
        Set(local, "get_Locations", ImmutableArray<Location>.Empty);
        Set(local, "get_DeclaringSyntaxReferences", ImmutableArray<SyntaxReference>.Empty);
        return local;
    }
}

using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class SourceMethodAttributeFactsTests
{
    [Theory]
    [InlineData("[Other][return: First] func F() -> [Second] int => 1", false)]
    [InlineData("class C { [Other][return: First] func F() -> [Second] int => 1 }", true)]
    public void ReturnListsPrecedeReturnClauseAndExcludeMethodAttributes(string source, bool method)
    {
        var reference = Declaration(source, method);
        var services = new Services();
        var result = SourceMethodAttributeFacts.BindReturnTypeAttributes(Owner(), [reference], services);
        Assert.Equal(new[] { "First", "Second" }, services.BoundNames);
        Assert.Equal(new[] { "First", "Second" }, result.Select(a => a.AttributeClass!.Name));
        Assert.All(result, a => Assert.Same(reference.SyntaxTree, a.ApplicationSyntaxReference!.SyntaxTree));
    }

    [Fact]
    public void MissingServicesOrDeclarationsDoNotBind()
    {
        var reference = Declaration("[return: First] func F() {}", false);
        Assert.Empty(SourceMethodAttributeFacts.BindReturnTypeAttributes(Owner(), [reference], null));
        var services = new Services();
        Assert.Empty(SourceMethodAttributeFacts.BindReturnTypeAttributes(Owner(), [], services));
        Assert.Empty(services.BoundNames);
    }

    [Fact]
    public void UnresolvedDeclarationsAreSkipped()
    {
        var services = new Services { Resolve = false };
        var reference = Declaration("[return: First] func F() {}", false);
        Assert.Empty(SourceMethodAttributeFacts.BindReturnTypeAttributes(Owner(), [reference], services));
        Assert.Empty(services.BoundNames);
    }

    [Fact]
    public void FailedBindingDoesNotPreventLaterAttributes()
    {
        var services = new Services();
        var reference = Declaration("[return: Missing, First] func F() {}", false);
        var result = SourceMethodAttributeFacts.BindReturnTypeAttributes(Owner(), [reference], services);
        Assert.Equal("First", Assert.Single(result).AttributeClass!.Name);
        Assert.Equal(new[] { "Missing", "First" }, services.BoundNames);
    }

    [Fact]
    public void DuplicatesAcrossDeclarationsRetainFirstAndReportDiagnostic()
    {
        var first = Declaration("[return: First] func F() {}", false);
        var second = Declaration("[return: First] func F() {}", false);
        var services = new Services();
        var result = SourceMethodAttributeFacts.BindReturnTypeAttributes(Owner(), [first, second], services);
        Assert.Same(first.SyntaxTree, Assert.Single(result).ApplicationSyntaxReference!.SyntaxTree);
        Assert.Equal(2, services.BoundNames.Count);
        Assert.Single(services.Diagnostics.SelectMany(b => b.AsEnumerable()));
    }

    private static MethodSymbol Owner()
    {
        var owner = Make<MethodSymbol>(SymbolKind.Method, "F");
        Set(owner, "get_MethodKind", MethodKind.Ordinary);
        return owner;
    }

    private static SyntaxReference Declaration(string source, bool method)
    {
        var tree = SyntaxTree.ParseText(source);
        Assert.Empty(tree.GetDiagnostics());
        return (method
            ? (SyntaxNode)tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single()
            : tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single()).GetReference();
    }

    private sealed class Services : SourceDeclarationServices
    {
        private readonly Dictionary<string, NamedTypeSymbol> _types = [];
        public List<string> BoundNames { get; } = [];
        public List<DiagnosticBag> Diagnostics { get; } = [];
        public bool Resolve { get; init; } = true;
        public ImmutableArray<Symbol> GetFunctionContractMembers(TypeSymbol owner) => [];
        public SyntaxTree[] SyntaxTrees => [];
        public bool UsesDeferredSourceAttributes => false;
        public bool SourceDeclarationsComplete => true;
        public SourceAttributeDeclarationContext GetAttributeDeclarationContext(Symbol owner) => default;
        public bool TryGetCurrentDeclaringSyntax(SyntaxReference reference, out SyntaxNode syntax)
        {
            syntax = reference.GetSyntax();
            return Resolve;
        }

        public SourceAttributeBinding? BindAttribute(Symbol owner, AttributeSyntax syntax)
        {
            var name = syntax.Name.ToString();
            BoundNames.Add(name);
            if (name == "Missing")
                return null;
            if (!_types.TryGetValue(name, out var type))
            {
                type = Named("Test", name);
                Set(type, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
                Set(type, "get_Arity", 0);
                _types.Add(name, type);
            }
            var diagnostics = new DiagnosticBag();
            Diagnostics.Add(diagnostics);
            return new(new AttributeData(type, Make<MethodSymbol>(SymbolKind.Method, ".ctor"), [], [], syntax.GetReference()), syntax, diagnostics);
        }

        public void PublishAttributeDiagnostics(SourceAttributeBinding attribute) => throw new InvalidOperationException();
        public NamedTypeSymbol? GetTypeByMetadataName(string metadataName) => null;
        public bool IsPrimaryConstructor(Symbol symbol) => false;
        public bool IsNamespaceContainer(Symbol symbol) => false;
        public NamedTypeSymbol GetSpecialType(SpecialType specialType) => throw new NotSupportedException();
        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination, bool includeUserDefined) => throw new NotSupportedException();
        public ImmutableArray<AttributeData> GetAttributesForUsageValidation(Symbol symbol) => [];
    }
}

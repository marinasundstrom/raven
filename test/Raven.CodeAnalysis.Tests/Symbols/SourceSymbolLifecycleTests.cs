using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class SourceSymbolLifecycleTests
{
    [Fact]
    public void BoundAttributesAreSharedWithValidationAndCached()
    {
        var services = new Services();
        var reference = Declaration("[Tag] func C() {}");
        var symbol = new Probe(services, [reference]);
        Assert.Single(symbol.GetAttributesForUsageValidation());
        Assert.Single(symbol.GetAttributes());
        Assert.Single(symbol.GetAttributes());
        Assert.Equal(1, services.BindCount);
    }

    [Fact]
    public void DeferredSourceCoreAttributesBecomeAvailableAfterDeclarationsComplete()
    {
        var services = new Services { UsesDeferredSourceAttributes = true, SourceDeclarationsComplete = false };
        var symbol = new Probe(services, [Declaration("[Tag] func C() {}")]);
        Assert.Empty(symbol.GetAttributes());
        Assert.Empty(symbol.GetAttributesForUsageValidation());
        Assert.Equal(0, services.BindCount);
        services.SourceDeclarationsComplete = true;
        Assert.Single(symbol.GetAttributes());
        Assert.Equal(1, services.BindCount);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void AddingDeclarationInvalidatesAttributesAndPreservesRequestedOrder(bool primary)
    {
        var services = new Services();
        var first = Declaration("[Tag] func C() {}", "first.rvn");
        var second = Declaration("[Tag] func C() {}", "second.rvn");
        var symbol = new Probe(services, [first]);
        Assert.Single(symbol.GetAttributes());
        symbol.AddDeclaration(second.GetSyntax().GetLocation(), second, primary);
        Assert.Equal(primary ? second : first, symbol.DeclaringSyntaxReferences[0]);
        // Same attribute class occurs twice: both bind, but validation keeps only the first.
        var attribute = Assert.Single(symbol.GetAttributes());
        Assert.Equal(primary ? second.SyntaxTree : first.SyntaxTree, attribute.ApplicationSyntaxReference!.SyntaxTree);
        Assert.Equal(3, services.BindCount);
    }

    [Fact]
    public void DuplicateSyntaxReferencesBindOnceAndUnresolvableDeclarationsAreSkipped()
    {
        var services = new Services();
        var reference = Declaration("[Tag] func C() {}");
        var symbol = new Probe(services, [reference, reference]);
        Assert.Single(symbol.GetAttributesForUsageValidation());
        Assert.Equal(1, services.BindCount);
        services.Resolve = false;
        symbol.AddDeclaration(reference.GetSyntax().GetLocation(), reference);
        Assert.Empty(symbol.GetAttributes());
        Assert.Equal(1, services.BindCount);
    }

    [Fact]
    public void EmptyDeclarationsAndMissingMarkerTypesReturnEmptyResults()
    {
        var services = new Services();
        var symbol = new Probe(services, []);
        Assert.Empty(symbol.GetAttributes());
        Assert.Null(symbol.CompilerGenerated());
        Assert.Null(symbol.RequiredMember());
        Assert.Null(symbol.ReadonlyMarker());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void AddingDeclarationRefreshesMergedDocumentation(bool primary)
    {
        var first = Declaration("/// First\nfunc C() {}", "first.rvn");
        var second = Declaration("/// Second\nfunc C() {}", "second.rvn");
        var symbol = new Probe(new Services(), [first]);
        var before = symbol.GetDocumentationComment();
        Assert.Contains("First", before!.Body);
        symbol.AddDeclaration(second.GetSyntax().GetLocation(), second, primary);
        var after = symbol.GetDocumentationComment();
        Assert.NotSame(before, after);
        Assert.Contains("First", after!.Body);
        Assert.Contains("Second", after.Body);
        Assert.Equal(primary, after.Body.IndexOf("Second", StringComparison.Ordinal) < after.Body.IndexOf("First", StringComparison.Ordinal));
        Assert.Equal(2, symbol.DocumentationComments.Length);
    }

    [Fact]
    public void MarkerFactoriesSelectParameterlessInstanceConstructorAndKeepSyntax()
    {
        var services = new Services { ProvideMarkers = true };
        var reference = Declaration("func C() {}");
        var symbol = new Probe(services, [reference]);
        foreach (var attribute in new[] { symbol.CompilerGenerated(), symbol.RequiredMember(), symbol.ReadonlyMarker() })
        {
            Assert.NotNull(attribute);
            Assert.Same(services.MarkerConstructor, attribute!.AttributeConstructor);
            Assert.Same(reference, attribute.ApplicationSyntaxReference);
            Assert.Empty(attribute.ConstructorArguments);
            Assert.Empty(attribute.NamedArguments);
        }
    }

    private static SyntaxReference Declaration(string source, string path = "test.rvn")
        => SyntaxTree.ParseText(source, path: path).GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single().GetReference();

    private sealed class Probe : SourceSymbol, SourceDeclarationServiceProvider
    {
        public Probe(Services services, SyntaxReference[] references)
            : base(SymbolKind.Namespace, "Probe", null, null, null, [], references, addAsMember: false)
            => DeclarationServices = services;

        public SourceDeclarationServices DeclarationServices { get; }
        public AttributeData? CompilerGenerated() => CreateCompilerGeneratedAttribute();
        public AttributeData? RequiredMember() => CreateRequiredMemberAttribute();
        public AttributeData? ReadonlyMarker() => CreateIsReadOnlyAttribute();
        public override void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);
        public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
    }

    private sealed class Services : SourceDeclarationServices
    {
        private readonly NamedTypeSymbol _attributeType = Named("Test", "TagAttribute");
        private readonly MethodSymbol _constructor = Make<MethodSymbol>(SymbolKind.Method, ".ctor");

        public Services()
        {
            Set(_attributeType, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
            Set(_attributeType, "get_Arity", 0);
            var staticConstructor = Make<MethodSymbol>(SymbolKind.Method, ".cctor");
            Set(staticConstructor, "get_IsStatic", true);
            Set(_constructor, "get_IsStatic", false);
            Set(_constructor, "get_Parameters", ImmutableArray<ParameterSymbol>.Empty);
            Set(_attributeType, "get_Constructors", ImmutableArray.Create(staticConstructor, _constructor));
        }

        public bool ProvideMarkers { get; init; }
        public MethodSymbol MarkerConstructor => _constructor;
        public SyntaxTree[] SyntaxTrees => [];
        public bool UsesDeferredSourceAttributes { get; init; }
        public bool SourceDeclarationsComplete { get; set; } = true;
        public bool Resolve { get; set; } = true;
        public int BindCount { get; private set; }
        public SourceAttributeDeclarationContext GetAttributeDeclarationContext(Symbol owner) => default;
        public bool TryGetCurrentDeclaringSyntax(SyntaxReference reference, out SyntaxNode syntax)
        {
            syntax = reference.GetSyntax();
            return Resolve;
        }

        public SourceAttributeBinding? BindAttribute(Symbol owner, AttributeSyntax syntax)
        {
            BindCount++;
            return new(new AttributeData(_attributeType, _constructor, [], [], syntax.GetReference()), syntax, new DiagnosticBag());
        }

        public void PublishAttributeDiagnostics(SourceAttributeBinding attribute) => throw new InvalidOperationException();
        public NamedTypeSymbol? GetTypeByMetadataName(string metadataName)
            => ProvideMarkers && metadataName.StartsWith("System.Runtime.CompilerServices.", StringComparison.Ordinal) ? _attributeType : null;
        public bool IsPrimaryConstructor(Symbol symbol) => false;
        public bool IsNamespaceContainer(Symbol symbol) => false;
        public NamedTypeSymbol GetSpecialType(SpecialType specialType) => throw new NotSupportedException();
        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination, bool includeUserDefined) => throw new NotSupportedException();
        public ImmutableArray<AttributeData> GetAttributesForUsageValidation(Symbol symbol) => [];
    }
}

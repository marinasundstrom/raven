using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Completion;

public class IndexerMemberAccessTests
{
    [Theory]
    [InlineData("List<string>")]
    [InlineData("IList<string>")]
    [InlineData("IReadOnlyList<string>")]
    [InlineData("Bucket")]
    public void IndexersAreNotNamedMembers(string receiverType)
    {
        var (compilation, tree, _) = Create(receiverType, "return items.Item");
        var model = compilation.GetSemanticModel(tree);
        var access = tree.GetRoot().DescendantNodes().OfType<MemberAccessExpressionSyntax>()
            .Single(node => node.ToString() == "items.Item");
        Assert.Null(model.GetSymbolInfo(access).Symbol);
        Assert.Null(model.GetSymbolInfo(access.Name).Symbol);
        var accessType = model.GetTypeInfo(access).Type;
        Assert.True(accessType is null || accessType.TypeKind == TypeKind.Error);
        var receiver = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetTypeInfo(access.Expression).Type);
        var indexers = receiver.GetMembers().OfType<IPropertySymbol>().Where(property => property.IsIndexer).ToArray();
        Assert.NotEmpty(indexers);
        Assert.All(indexers, indexer => Assert.False(indexer.CanBeReferencedByName));
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error &&
            diagnostic.GetMessage().Contains("Item"));
        Assert.Null(model.GetSymbolInfo(access).Symbol);
    }

    [Theory]
    [InlineData("List<string>", "items.", "Count")]
    [InlineData("IList<string>", "items.", "Count")]
    [InlineData("IReadOnlyList<string>", "items.", "Count")]
    [InlineData("Bucket", "items.", "Count")]
    [InlineData("List<string>", "items[0].", "Length")]
    [InlineData("IList<string>", "items[0].", "Length")]
    [InlineData("IReadOnlyList<string>", "items[0].", "Length")]
    [InlineData("Bucket", "items[0].", "Length")]
    public void CompletionRequiresIndexedAccessForElementMembers(string receiverType, string expression, string expectedMember)
    {
        var (compilation, tree, position) = Create(receiverType, expression);
        var items = compilation.GetCompletions(tree, position).ToArray();
        Assert.Contains(items, item => item.DisplayText == expectedMember);
        Assert.DoesNotContain(items, item => item.Symbol is IPropertySymbol { IsIndexer: true });
    }

    [Theory]
    [InlineData("List<string>")]
    [InlineData("IList<string>")]
    [InlineData("IReadOnlyList<string>")]
    [InlineData("Bucket")]
    public void InvalidNamedIndexerDoesNotOfferElementMembers(string receiverType)
    {
        var (compilation, tree, position) = Create(receiverType, "items.Item.");
        Assert.DoesNotContain(compilation.GetCompletions(tree, position), item => item.DisplayText == "Length");
    }

    [Theory]
    [InlineData("List<string>")]
    [InlineData("IList<string>")]
    [InlineData("IReadOnlyList<string>")]
    [InlineData("Bucket")]
    public void IndexedAccessStillBindsToTheIndexer(string receiverType)
    {
        var (compilation, tree, _) = Create(receiverType, "return items[0]");
        var model = compilation.GetSemanticModel(tree);
        var access = tree.GetRoot().DescendantNodes().OfType<ElementAccessExpressionSyntax>().Single();
        Assert.Equal(SpecialType.System_String, model.GetTypeInfo(access).Type?.SpecialType);
        Assert.Empty(compilation.GetDiagnostics().Where(diagnostic => diagnostic.Severity == DiagnosticSeverity.Error));
    }

    [Fact]
    public void OrdinaryPropertyNamedItemRemainsAccessible()
    {
        var (compilation, tree, position) = Create("NamedProperty", "items.Item.");
        Assert.Contains(compilation.GetCompletions(tree, position), item => item.DisplayText == "Length");
    }

    private static (Compilation Compilation, SyntaxTree Tree, int Position) Create(string receiverType, string body)
    {
        var code = $$"""
            import System.Collections.Generic.*
            class Bucket {
                public val Count: int => 1
                public val self[index: int]: string {
                    get => "value"
                }
            }
            class NamedProperty {
                public val Item: string => "value"
            }
            func Inspect(items: {{receiverType}}) -> string {
                {{body}}
            }
            """;
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("indexer_access", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree).AddReferences(TestMetadataReferences.Default);
        return (compilation, tree, code.LastIndexOf(body, StringComparison.Ordinal) + body.Length);
    }
}

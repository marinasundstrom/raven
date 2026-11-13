using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class ConstructedSourceDeclarationContractTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void MemberCachingUsesProviderDeclarationState(bool mutable)
    {
        var definition = Make<SourceNamedTypeIdentity>(SymbolKind.Type, "Definition");
        Set(definition, "get_HasMutableMemberShape", mutable);
        Set(definition, "get_ContainingAssembly", null);
        var first = Make<Symbol>(SymbolKind.Label, "Member");
        var second = Make<Symbol>(SymbolKind.Label, "Member");
        ImmutableArray<Symbol> members = [first];
        Set(definition, "GetMembers", (Func<object?[]?, object?>)(_ => members));
        var constructed = new ConstructedNamedTypeSymbol(definition, []);
        Assert.Same(first, Assert.Single(constructed.GetMembers()));
        Assert.Same(first, Assert.Single(constructed.GetMembers("Member")));
        members = [second];
        Assert.Same(mutable ? second : first, Assert.Single(constructed.GetMembers()));
        Assert.Same(mutable ? second : first, Assert.Single(constructed.GetMembers("Member")));
    }

    [Fact]
    public void UnionAndCaseDeclarationsKeepMutableMemberShape()
    {
        var tree = SyntaxTree.ParseText("class Plain {}\nunion Result<T> { case Value(value: T) }");
        var compilation = Compilation.Create("SourceShape", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var ordinary = Assert.IsAssignableFrom<SourceNamedTypeIdentity>(compilation.GetTypeByMetadataName("Plain"));
        var union = Assert.IsAssignableFrom<SourceNamedTypeIdentity>(compilation.GetTypeByMetadataName("Result`1"));
        Assert.False(ordinary.HasMutableMemberShape);
        Assert.True(union.HasMutableMemberShape);
        var cases = Assert.IsAssignableFrom<UnionSymbol>(union).DeclaredCaseTypes;
        Assert.NotEmpty(cases);
        Assert.All(cases, item => Assert.True(Assert.IsAssignableFrom<SourceNamedTypeIdentity>(item).HasMutableMemberShape));
    }
}

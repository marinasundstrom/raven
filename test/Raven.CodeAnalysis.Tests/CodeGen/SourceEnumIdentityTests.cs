using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class SourceEnumIdentityTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void SourceEnumMatchingCompilerHostName_KeepsSourceSignatureIdentity(bool enumFirst)
    {
        var tokenTree = SyntaxTree.ParseText("""
            namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser
            import Raven.CodeAnalysis.Syntax.*
            public struct Token {
                val Kind: SyntaxKind
                init(kind: SyntaxKind) { Kind = kind }
            }
            """);
        var enumTree = SyntaxTree.ParseText("""
            namespace Raven.CodeAnalysis.Syntax
            public enum SyntaxKind { None, IdentifierToken }
            """);
        var compilation = Compilation.Create("EnumIdentity", enumFirst ? [enumTree, tokenTree] : [tokenTree, enumTree],
            [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(typeof(Compilation).Assembly.Location)],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var kind = compilation.GetTypeByMetadataName("Raven.CodeAnalysis.Syntax.SyntaxKind")!;
        var token = compilation.GetTypeByMetadataName("Raven.CodeAnalysis.Syntax.InternalSyntax.Parser.Token")!;
        var property = Assert.Single(token.GetMembers("Kind").OfType<PropertySymbol>());
        Assert.Same(kind, property.Type);
        Assert.Same(compilation.Assembly, kind.ContainingAssembly);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var emittedKind = loaded.Assembly.GetType("Raven.CodeAnalysis.Syntax.SyntaxKind")!;
        var emittedToken = loaded.Assembly.GetType("Raven.CodeAnalysis.Syntax.InternalSyntax.Parser.Token")!;
        Assert.Equal(emittedKind, emittedToken.GetProperty("Kind")!.PropertyType);
        Assert.Equal(emittedKind, Assert.Single(emittedToken.GetConstructors()).GetParameters()[0].ParameterType);
    }
}

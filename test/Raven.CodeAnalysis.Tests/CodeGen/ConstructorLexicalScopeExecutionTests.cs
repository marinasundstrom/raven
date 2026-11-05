using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class ConstructorLexicalScopeExecutionTests
{
    [Fact]
    public void StaticInitializerConstructsItsOwnTypeInsteadOfGlobalNamesake()
    {
        var tree = SyntaxTree.ParseText("""
            public struct Token {}
            namespace Outer.Inner {
                class Token {
                    static val Empty: Token = Token()
                    val Value: int => 42
                    static func Read() -> int => Empty.Value
                }
            }
            """);
        var compilation = Compilation.Create("LexicalConstructorExecution", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(42, loaded.Assembly.GetType("Outer.Inner.Token")!.GetMethod("Read")!.Invoke(null, null));
    }
}

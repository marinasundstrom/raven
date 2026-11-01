using System.Reflection;
using System.Runtime.Loader;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

namespace Raven.GeneratorParity.Tests;

[Trait("Target", "DotNet")]
public class RavenSyntaxTreeOutputTests
{
    [Fact]
    public void CompleteRepositoryOutputHasBothTreesAndParses()
    {
        var nodes = NodeEmitterInputs.Nodes();
        var hierarchies = NodeEmitterInputs.Hierarchies();
        var files = RavenSyntaxTreeGenerator.Generate(nodes, hierarchies, NodeEmitterInputs.Factories(), NodeEmitterInputs.Tokens(), NodeEmitterInputs.Kinds());
        Assert.Equal(nodes.Count * 2 + nodes.Count(n => !n.IsAbstract) * 2 + hierarchies.Count + 11, files.Count);
        foreach (var node in nodes)
        {
            Assert.Contains("generated/" + node.Name + "Syntax.g.rvn", files.Keys);
            Assert.Contains("InternalSyntax/generated/" + node.Name + "Syntax.g.rvn", files.Keys);
            Assert.Equal(!node.IsAbstract, files.ContainsKey("generated/" + node.Name + "Syntax.SyntaxFactory.g.rvn"));
        }
        foreach (var file in files)
        {
            Assert.EndsWith(".g.rvn", file.Key);
            var errors = SyntaxTree.ParseText(file.Value).GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error);
            Assert.True(!errors.Any(), file.Key + Environment.NewLine + string.Join(Environment.NewLine, errors));
        }
        var again = RavenSyntaxTreeGenerator.Generate(nodes, hierarchies, NodeEmitterInputs.Factories(), NodeEmitterInputs.Tokens(), NodeEmitterInputs.Kinds());
        Assert.Equal(files.OrderBy(p => p.Key), again.OrderBy(p => p.Key));
    }

    [Fact]
    public void RavenTokenFactoriesAndFactsPreserveLiteralTextAtRuntime()
    {
        const string literal = "$name ${value} \"\\\r\n\t";
        var tokens = new List<TokenKindModel> { new("Literal") { Text = literal }, new("Word") { Text = "word", IsReservedWord = true }, new("Empty") { Text = "" }, new("Dynamic"), new("Trivia") { Text = " ", IsTrivia = true } };
        var green = TokenGenerator.GenerateRavenFactory(tokens, true);
        var red = TokenGenerator.GenerateRavenFactory(tokens, false);
        Assert.DoesNotContain("SyntaxKind.Trivia", green);
        Assert.DoesNotContain("SyntaxKind.Dynamic", red);
        var sources = new[] { green, red, TokenGenerator.GenerateRavenSyntaxFacts(tokens), """
            namespace Raven.CodeAnalysis.Syntax {
                public enum SyntaxKind { None, Literal, Word, Empty, Dynamic, Trivia }
                public record struct SyntaxToken(Text: string) { }
                public static partial class SyntaxFactory {
                    static func Token(kind: SyntaxKind) -> SyntaxToken => SyntaxToken(SyntaxFacts.GetSyntaxTokenText(kind)!)
                }
                public static class Probe {
                    static func Read() -> string => InternalSyntax.SyntaxFactory.Literal.Text + SyntaxFactory.Literal.Text
                    static func Empty() -> string => InternalSyntax.SyntaxFactory.Empty.Text + SyntaxFactory.Empty.Text
                    static func Keyword() -> bool {
                        var kind = SyntaxKind.None
                        return SyntaxFacts.TryParseKeyword(InternalSyntax.SyntaxFactory.Word.Text, out kind) && kind == SyntaxKind.Word
                    }
                }
            }
            namespace Raven.CodeAnalysis.Syntax.InternalSyntax {
                alias SyntaxKind = Raven.CodeAnalysis.Syntax.SyntaxKind
                internal class SyntaxToken {
                    init(kind: SyntaxKind, text: string) { Text = text }
                    val Text: string { get; }
                }
            }
            """ };
        var runtime = Path.GetDirectoryName(typeof(object).Assembly.Location)!;
        var references = new[] { typeof(object).Assembly.Location, typeof(Enumerable).Assembly.Location,
            Path.Combine(runtime, "System.Runtime.dll"), Path.Combine(runtime, "System.Collections.dll") }
            .Distinct().Select(path => MetadataReference.CreateFromFile(path)).ToArray();
        var compilation = Compilation.Create("TokenOutput" + Guid.NewGuid().ToString("N"), sources.Select(s => SyntaxTree.ParseText(s)).ToArray(), references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithFrameworkProjectionMode(FrameworkProjectionMode.None));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        image.Position = 0;
        var context = new AssemblyLoadContext("TokenOutput", isCollectible: true);
        try
        {
            var type = context.LoadFromStream(image).GetType("Raven.CodeAnalysis.Syntax.Probe")!;
            Assert.Equal(literal + literal, type.GetMethod("Read", BindingFlags.Public | BindingFlags.Static)!.Invoke(null, null));
            Assert.Equal("", type.GetMethod("Empty", BindingFlags.Public | BindingFlags.Static)!.Invoke(null, null));
            Assert.Equal(true, type.GetMethod("Keyword", BindingFlags.Public | BindingFlags.Static)!.Invoke(null, null));
        }
        finally { context.Unload(); }
    }
}

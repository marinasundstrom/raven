using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class InaccessibleMemberHidingTests
{
    [Theory]
    [InlineData("private", 0)]
    [InlineData("protected", 2)]
    [InlineData("internal", 2)]
    [InlineData("public", 2)]
    public void FieldAndMethodHiding_MatchesCSharpAccessibility(string accessibility, int expected)
    {
        var source = $$"""
            open class Base {
                {{accessibility}} field cache: int = 1
                {{accessibility}} func Read() -> int => cache
            }
            class Derived : Base {
                private field cache: int = 2
                func Read() -> int => cache
            }
            """;
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("HidingAccessibility", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var warnings = diagnostics.Count(diagnostic => diagnostic.Id == CompilerDiagnostics.MemberHidesInheritedMember.Id);

        var csharpTree = Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText($$"""
            class Base {
                {{accessibility}} int cache = 1;
                {{accessibility}} int Read() => cache;
            }
            class Derived : Base {
                private int cache = 2;
                public int Read() => cache;
            }
            """);
        var oracle = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("HidingOracle", [csharpTree],
            TestMetadataReferences.Default.OfType<PortableExecutableReference>()
                .Select(reference => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(reference.FilePath)),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        var oracleDiagnostics = oracle.GetDiagnostics();
        Assert.DoesNotContain(oracleDiagnostics, diagnostic => diagnostic.Severity == Microsoft.CodeAnalysis.DiagnosticSeverity.Error);
        Assert.Equal(expected, oracleDiagnostics.Count(diagnostic => diagnostic.Id == "CS0108"));
        Assert.Equal(expected, warnings);
    }
}

[Trait("Target", "NeoCLR")]
public sealed class NeoClrInaccessibleMemberHidingTests
{
    [Fact]
    public void NeoClrWithHostReferences_PrivateBaseMembersDoNotProduceHidingWarnings()
    {
        var tree = SyntaxTree.ParseText("""
            open class Base {
                private field cache: int = 1
                private func Read() -> int => cache
            }
            class Derived : Base {
                private field cache: int = 2
                func Read() -> int => cache
            }
            """);
        var compilation = Compilation.Create("NeoClrHidingAccessibility", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(TargetPlatform.NeoCLR));
        var diagnostics = compilation.GetSemanticModel(tree).GetDiagnostics();
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == CompilerDiagnostics.MemberHidesInheritedMember.Id);
    }
}

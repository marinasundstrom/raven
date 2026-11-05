using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class ConstructorLexicalScopeTests
{
    public static IEnumerable<object[]> Cases()
    {
        var cases = new (string Source, string Expected)[]
        {
            ("namespace Outer.Inner { class Token { static val Empty: Token = Token() } }", "Outer.Inner.Token"),
            ("namespace Outer { class Token {} class Probe { static val Empty: Token = Token() } }", "Outer.Token"),
            ("namespace Outer { class Token {} } namespace Outer.Inner { class Probe { static val Empty: Token = Token() } }", "Outer.Token"),
            ("class Outer { class Token { static val Empty: Token = Token() } }", "Outer+Token"),
            ("namespace Outer { class Token {} class Token<T> { static val Empty: Token<T> = Token<T>() } }", "Outer.Token`1"),
            ("namespace Imported { class Token {} } namespace Outer { import Imported.*; class Token {} class Probe { static val Empty: Token = Token() } }", "Outer.Token"),
            ("namespace Imported { class Token {} } namespace Outer { class Token {} } namespace Outer.Inner { import Imported.*; class Probe { static val Empty: Token = Token() } }", "Imported.Token")
        };
        foreach (var (source, expected) in cases)
        {
            yield return ["public struct Token {} " + source, expected, false];
            yield return ["public struct Token {} " + source, expected, true];
        }
    }

    [Theory]
    [MemberData(nameof(Cases))]
    [Trait("Target", "DotNet")]
    public void DotNet_ConstructorUsesLexicalType(string source, string expected, bool diagnosticsFirst)
        => Verify(source, expected, diagnosticsFirst, TargetPlatform.DotNet);

    [Theory]
    [MemberData(nameof(Cases))]
    [Trait("Target", "NeoCLR")]
    public void NeoCLR_ConstructorUsesLexicalType(string source, string expected, bool diagnosticsFirst)
        => Verify(source, expected, diagnosticsFirst, TargetPlatform.NeoCLR);

    [Fact]
    [Trait("Target", "DotNet")]
    public void DotNet_LocalValueAndAliasRetainPrecedence()
    {
        var tree = SyntaxTree.ParseText("""
            class Token {}
            namespace Imported { class Token {} }
            namespace Outer {
                alias Token = Imported.Token
                class Probe {
                    static func Create() -> Token => Token()
                    static func Invoke(Token: System.Func<int>) -> int => Token()
                }
            }
            """);
        var compilation = Compilation.Create("LexicalValues", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        Assert.DoesNotContain(model.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var invocations = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().ToArray();
        Assert.Equal("Imported.Token", model.GetSymbolInfo(invocations[0]).Symbol!.ContainingType!.ToFullyQualifiedMetadataName());
        Assert.Equal(SpecialType.System_Int32, model.GetTypeInfo(invocations[1]).Type!.SpecialType);
        Assert.IsAssignableFrom<ParameterSymbol>(model.GetSymbolInfo(invocations[1].Expression).Symbol);
    }

    private static void Verify(string source, string expected, bool diagnosticsFirst, TargetPlatform target)
    {
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("LexicalConstructor", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        var model = compilation.GetSemanticModel(tree);
        if (diagnosticsFirst)
            Assert.DoesNotContain(model.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var constructor = Assert.IsAssignableFrom<MethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.Equal(MethodKind.Constructor, constructor.MethodKind);
        Assert.Equal(expected, constructor.ContainingType!.ToFullyQualifiedMetadataName());
        Assert.Equal(expected, model.GetTypeInfo(invocation).Type!.ToFullyQualifiedMetadataName());
        Assert.DoesNotContain(model.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }
}

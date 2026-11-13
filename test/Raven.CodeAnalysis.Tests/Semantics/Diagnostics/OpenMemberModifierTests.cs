using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class OpenMemberModifierTests
{
    [Fact]
    [Trait("Target", "DotNet")]
    public void DotNetRejectsOpenOnMembers() => AssertInvalidModifiers(TargetPlatform.DotNet);

    [Fact]
    [Trait("Target", "NeoCLR")]
    public void NeoClrRejectsOpenOnMembers() => AssertInvalidModifiers(TargetPlatform.NeoCLR);

    private static void AssertInvalidModifiers(TargetPlatform target)
    {
        var tree = SyntaxTree.ParseText("""
            open class Container {
                open func Method() -> int => 1
                open val Property: int => 2
                open val self[index: int]: int => index
                open event Changed: System.Action;
            }
            """);
        var compilation = Compilation.Create("OpenMembers", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        var errors = compilation.GetSemanticModel(tree).GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        Assert.Equal(4, errors.Length);
        Assert.All(errors, diagnostic =>
        {
            Assert.Equal("RAV0332", diagnostic.Id);
            Assert.Equal("open", tree.GetText().ToString(diagnostic.Location.SourceSpan));
        });
        Assert.Equal(4, errors.Select(d => d.Location.SourceSpan).Distinct().Count());
    }

    [Fact]
    [Trait("Target", "Shared")]
    public void OpenClassAndVirtualMembersRetainTheirDistinctRoles()
    {
        var tree = SyntaxTree.ParseText("""
            open class Base {
                virtual func Method() -> int => 1
                virtual val Property: int => 2
                virtual val self[index: int]: int => index
                virtual event Changed: System.Action;
            }
            class Derived : Base {
                override func Method() -> int => 3
                override val Property: int => 4
                override val self[index: int]: int => index + 1
                override event Changed: System.Action;
            }
            """);
        var compilation = Compilation.Create("VirtualMembers", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var declaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Last();
        var derived = Assert.IsAssignableFrom<NamedTypeSymbol>(compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));
        Assert.All(derived.GetMembers().OfType<MethodSymbol>().Where(m => m.MethodKind != MethodKind.Constructor),
            method => Assert.True(method.IsVirtual));
    }
}

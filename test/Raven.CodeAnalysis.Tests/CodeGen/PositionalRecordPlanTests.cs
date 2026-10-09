using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Semantics.Tests;

namespace Raven.CodeAnalysis.Tests;

public sealed class PositionalRecordPlanTests : CompilationTestBase
{
    private static EmissionCapabilities Capabilities(bool records) => new(
        Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
        Enum.GetValues<EmissionDeclarationKind>().Where(kind => records || kind != EmissionDeclarationKind.PositionalRecordStorage),
        Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
        allowsRootClassSignatures: true, allowsRootClassLocals: true, allowsManagedReferences: true,
        allowsConstructedFieldReferences: true, allowsGenericInstanceMethods: true,
        allowsGenericStaticOwners: true, allowsGenericClassOwners: true,
        allowsConstructedInterfaceImplementations: true, allowsExternalInterfaceDeclarations: true,
        allowsInterfaceSignatures: true, allowsGenericInterfaceDeclarations: true);

    [Fact]
    public void PositionalStorageDoesNotBypassUnsupportedInterfaceConstraints()
    {
        var (compilation, _) = CreateCompilation("record struct Pair<K, V>(val Key: K, val Value: V)",
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var tree = compilation.SyntaxTrees.Single();
        var syntax = tree.GetRoot().DescendantNodes().OfType<RecordDeclarationSyntax>().Single();
        var type = Assert.IsType<SourceNamedTypeSymbol>(compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax));
        Assert.False(SourceTypePlan.TryCreate(type, out _, Capabilities(false)));
        // Modern .NET records implement IEquatable<T> with an allows-ref-struct
        // constraint. Positional storage must not silently admit that extra contract.
        Assert.Contains(type.Interfaces, i => i.TypeParameters.Any(p => p.ConstraintKind != TypeParameterConstraintKind.None));
        Assert.False(SourceTypePlan.TryCreate(type, out _, Capabilities(true)));
    }

    [Fact]
    public void DeconstructionAssignmentUsesDeclaredMethodContract()
    {
        var (compilation, _) = CreateCompilation("""
            struct Pair {
                public field Key: int
                public field Value: int
                init(key: int, value: int) {
                    self.Key = key
                    self.Value = value
                }
                func Deconstruct(out key: int, out value: int) {
                    key = Key
                    value = Value
                }
            }
            class Program {
                static func Main() -> int {
                    let pair = Pair(10, 32)
                    let (key, value) = pair
                    return key + value
                }
            }
            """, new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var tree = compilation.SyntaxTrees.Single();
        var syntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single(m => m.Identifier.ValueText == "Main");
        var method = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, Capabilities(true)));
        Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, Capabilities(true)), failure?.Detail);
        Assert.True(plan.TryLowerBody(compilation, _ => false, out _, out var ordinaryFailure, Capabilities(false)), ordinaryFailure?.Detail);
    }
}

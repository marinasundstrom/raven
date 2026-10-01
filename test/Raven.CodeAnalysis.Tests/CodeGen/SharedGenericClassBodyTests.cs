using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class SharedGenericClassBodyTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void GenericClassStorageSharesPlanningAndExecutes(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            class Box<Element> {
                private var stored: Element = default(Element)
                init(value: Element) { stored = value }
                func Read() -> Element => stored
                func Write(value: Element) { stored = value }
                func Echo<Other>(value: Other) -> Other => value
            }
            func Identity<T>(value: T) -> T => value
            func Main() -> int {
                let box = Box<int>(1)
                let alias = Identity(box)
                alias.Write(42)
                let nested = Box<Box<int>>(box)
                if box.Echo<long>(5000000000L) != 5000000000L { return 1 }
                return nested.Read().Read()
            }
            """);
        var compilation = Compilation.Create("GenericClassStorage", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        // .NET constructors retain their general emitter; methods and creation expressions share planning.
        var declarations = tree.GetRoot().DescendantNodes().Where(n => n is MethodDeclarationSyntax or FunctionStatementSyntax);
        foreach (var syntax in declarations)
        {
            var method = (IMethodSymbol)model.GetDeclaredSymbol(syntax)!;
            Assert.True(SourceCallablePlan.TryCreate(method, out var plan, ReflectionEmitCapabilities.Shared));
            Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail);
            if (method.ContainingType?.Arity > 0)
            {
                var noClasses = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
                    Enum.GetValues<EmissionDeclarationKind>(), [Accessibility.Internal, Accessibility.Public],
                    [Accessibility.Public], [Accessibility.Internal], allowsGenericMethods: true, allowsGenericInstanceMethods: true, allowsGenericStaticOwners: true);
                Assert.False(plan.IsSupportedBy(noClasses));
            }
        }
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        Assert.Equal(42, Assembly.Load(image.ToArray()).EntryPoint!.Invoke(null, null));
    }
    [Fact]
    public void UnusedConstructorOwnerArgumentsStillRequireCapabilities()
    {
        var tree = SyntaxTree.ParseText("""
            class Marker<T> { }
            func Main() -> int {
                Marker<int[]>()
                return 42
            }
            """);
        var compilation = Compilation.Create("OwnerCapabilities", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        var method = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        var noArrays = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), [Accessibility.Internal, Accessibility.Public],
            [Accessibility.Public], [Accessibility.Internal], allowsRootClassSignatures: true, allowsGenericClassOwners: true);
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, noArrays));
        Assert.False(plan!.TryLowerBody(compilation, _ => false, out _, out _, noArrays));
    }
}

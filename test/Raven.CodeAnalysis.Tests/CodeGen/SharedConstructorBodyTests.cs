using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class SharedConstructorBodyTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void ExplicitRootBaseCallPreservesInitializersAndBodies(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            class Root {
                private var counter: Counter = Counter(40)
                var Number: int = counter.Next()
                init(): base() { Number = counter.Next() }
                init(extra: int): base() => Number = counter.Next() + extra
            }
            class Counter {
                var Number: int
                init(number: int) { Number = number }
                func Next() -> int { Number = Number + 1; return Number }
            }
            func Main() -> int {
                if Root().Number != 42 { return 1 }
                return Root(0).Number
            }
            """);
        var compilation = Compilation.Create("ExplicitRootBase", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        var storageSyntax = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single(p => p.Identifier.ValueText == "counter");
        var storage = (SourcePropertySymbol)model.GetDeclaredSymbol(storageSyntax)!;
        var field = storage.BackingField!;
        for (var iteration = 0; iteration < 3; iteration++)
        {
            _ = model.GetDeclaredSymbol(storageSyntax);
            Assert.Same(field, storage.BackingField);
            Assert.Same(field, Assert.Single(storage.ContainingType!.GetMembers(field.Name).OfType<IFieldSymbol>()));
            Assert.IsType<BoundObjectCreationExpression>(field.Initializer);
        }
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), [Accessibility.Public, Accessibility.Internal],
            [Accessibility.Public, Accessibility.Private], allowsRootClassLocals: true, allowsRootClassSignatures: true);
        foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<ConstructorDeclarationSyntax>())
        {
            var constructor = (IMethodSymbol)model.GetDeclaredSymbol(syntax)!;
            Assert.True(SourceCallablePlan.TryCreate(constructor, out var plan, capabilities));
            Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, capabilities), failure?.Detail);
        }
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        Assert.Equal(42, Assembly.Load(image.ToArray()).EntryPoint!.Invoke(null, null));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void InitializersRunBeforeExplicitBodiesAndInImplicitConstructors(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            class Implicit {
                private var first: int = 20
                var Number: int = 21
                val Sum: int => first + Number
            }
            class Explicit {
                private var first: int = 20
                var Number: int = 21
                init() { Number = first + Number + 1 }
            }
            func Main() -> int {
                if Implicit().Sum != 41 { return 1 }
                return Explicit().Number
            }
            """);
        var compilation = Compilation.Create("Initialization", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), [Accessibility.Public, Accessibility.Internal],
            [Accessibility.Public, Accessibility.Private]);
        foreach (var name in new[] { "Implicit", "Explicit" })
        {
            var constructor = compilation.GetTypeByMetadataName(name)!.GetMembers().OfType<IMethodSymbol>().Single(m => m.MethodKind == MethodKind.Constructor);
            Assert.True(SourceCallablePlan.TryCreate(constructor, out var plan, capabilities));
            Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, capabilities), failure?.Detail);
        }
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        Assert.Equal(42, Assembly.Load(image.ToArray()).EntryPoint!.Invoke(null, null));
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void ExpressionConstructorsPreserveOverloadsAndInitialization(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            class Created {
                var Number: int
                init(value: int) => Set(value)
                init(left: int, right: int) => Number = left * 10 + right
                private func Set(value: int) { Number = value + 1 }
            }
            func Main() -> int {
                if Created(4, 2).Number != 42 { return 1 }
                return Created(41).Number
            }
            """);
        var compilation = Compilation.Create("ConstructorBodies", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), [Accessibility.Public, Accessibility.Internal],
            [Accessibility.Public, Accessibility.Private]);
        foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<ConstructorDeclarationSyntax>())
        {
            var constructor = (IMethodSymbol)model.GetDeclaredSymbol(syntax)!;
            Assert.True(SourceCallablePlan.TryCreate(constructor, out var plan, capabilities));
            Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, capabilities), failure?.Detail);
        }
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var assembly = Assembly.Load(image.ToArray());
        Assert.Equal(42, assembly.EntryPoint!.Invoke(null, null));
        Assert.Equal(new[] { 1, 2 }, assembly.GetType("Created")!.GetConstructors().Select(c => c.GetParameters().Length).Order());
    }
}

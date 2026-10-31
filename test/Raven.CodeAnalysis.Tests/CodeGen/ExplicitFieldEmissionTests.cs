using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class ExplicitFieldEmissionTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void NominalStoragePreservesIdentityAndAliasing(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            class Holder {
                public field Value: Item
                private var stored: Item
                init(value: Item) { Value = value; stored = value }
                func Read() -> Item => stored
                func Replace(value: Item) { stored = value; Value = value }
            }
            class Item {
                var Number: int
                init(number: int) { Number = number }
            }
            func Main() -> int {
                let original = Item(41)
                let holder = Holder(original)
                holder.Value.Number = 42
                if holder.Read().Number != 42 { return 1 }
                if original.Number != 42 { return 2 }
                holder.Replace(Item(7))
                if holder.Value.Number != 7 { return 3 }
                return original.Number
            }
            """);
        var compilation = Compilation.Create("NominalFields", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in tree.GetRoot().DescendantNodes().Where(n => n is FunctionStatementSyntax or MethodDeclarationSyntax))
        {
            var method = (IMethodSymbol)model.GetDeclaredSymbol(syntax)!;
            Assert.True(SourceCallablePlan.TryCreate(method, out var plan, ReflectionEmitCapabilities.Shared));
            Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail);
        }
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var assembly = Assembly.Load(image.ToArray());
        Assert.Equal(42, assembly.EntryPoint!.Invoke(null, null));
        var holder = assembly.GetType("Holder")!;
        var fields = holder.GetFields(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
        Assert.Equal(2, fields.Length);
        Assert.All(fields, field => Assert.Equal(assembly.GetType("Item"), field.FieldType));
        Assert.Empty(holder.GetProperties());
    }

    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void ExplicitFieldsPreserveStorageAccessAndInitialization(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            class Storage {
                public field Number: int = 40
                internal field Wide: long = 5000000000L
                private field active: bool = true
                func Active() -> bool => self.active
            }
            func Main() -> int {
                let original = Storage()
                let alias = original
                alias.Number = alias.Number + 2
                alias.Wide = alias.Wide + 1L
                if !original.Active() { return 1 }
                if original.Wide != 5000000001L { return 2 }
                return original.Number
            }
            """);
        var compilation = Compilation.Create("ExplicitFields", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in tree.GetRoot().DescendantNodes().Where(n => n is FunctionStatementSyntax or MethodDeclarationSyntax))
        {
            var method = (IMethodSymbol)model.GetDeclaredSymbol(syntax)!;
            Assert.True(SourceCallablePlan.TryCreate(method, out var plan, ReflectionEmitCapabilities.Shared));
            Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail);
        }
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var assembly = Assembly.Load(image.ToArray());
        Assert.Equal(42, assembly.EntryPoint!.Invoke(null, null));
        var storage = assembly.GetType("Storage")!;
        var fields = storage.GetFields(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
        Assert.Equal(3, fields.Length);
        Assert.Empty(storage.GetProperties());
        Assert.True(fields.Single(f => f.Name == "Number").IsPublic);
        Assert.True(fields.Single(f => f.Name == "Wide").IsAssembly);
        Assert.True(fields.Single(f => f.Name == "active").IsPrivate);
    }
}

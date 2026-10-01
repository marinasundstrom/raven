using System.Reflection;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class NominalPropertyInitializerTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void ForwardDeclaredPropertyInitializersPreserveValuesAndIdentity(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            class Holder {
                var Value: Item = Item(7)
                var Explicit: Item {
                    get => field
                    set => field = value
                }
                val Current: Item => Value
                val Selected: Item {
                    get { return Value }
                    private set { Value = value }
                }
                init(value: Item) { Explicit = value }
                func Replace(value: Item) { Selected = value }
            }
            class Item {
                var Number: int
                init(number: int) { Number = number }
            }
            func Main() -> int {
                let original = Item(41)
                let holder = Holder(original)
                if holder.Current.Number != 7 { return 1 }
                holder.Explicit.Number = 42
                if original.Number != 42 { return 2 }
                holder.Replace(original)
                holder.Current.Number = 40
                if holder.Selected.Number != 40 { return 3 }
                holder.Value = Item(42)
                if original.Number != 40 { return 4 }
                return holder.Selected.Number
            }
            """);
        var compilation = Compilation.Create("NominalProperties", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        var initialized = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single(p => p.Identifier.ValueText == "Value");
        Assert.IsType<BoundObjectCreationExpression>(((SourcePropertySymbol)model.GetDeclaredSymbol(initialized)!).BackingField!.Initializer);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var assembly = Assembly.Load(image.ToArray());
        var holder = assembly.GetType("Holder")!;
        var item = Activator.CreateInstance(assembly.GetType("Item")!, [41]);
        var instance = Activator.CreateInstance(holder, [item]);
        Assert.NotNull(holder.GetProperty("Value")!.GetValue(instance));
        Assert.Same(item, holder.GetProperty("Explicit")!.GetValue(instance));
        Assert.NotNull(holder.GetProperty("Current")!.GetValue(instance));
        Assert.Equal(42, assembly.EntryPoint!.Invoke(null, null));
        var properties = holder.GetProperties();
        Assert.Equal(4, properties.Length);
        Assert.All(properties, property => Assert.Equal(assembly.GetType("Item"), property.PropertyType));
        Assert.Equal(2, holder.GetFields(BindingFlags.Instance | BindingFlags.NonPublic).Length);
        Assert.Null(holder.GetProperty("Current")!.SetMethod);
        Assert.True(holder.GetProperty("Selected")!.SetMethod!.IsPrivate);
    }

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
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        Assert.Equal(42, Assembly.Load(image.ToArray()).EntryPoint!.Invoke(null, null));
    }

}

using System.Reflection;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class AutoPropertySymbolStabilityTests
{
    [Fact]
    public void RepeatedBindingKeepsOneCanonicalAccessorPerProperty()
    {
        var tree = SyntaxTree.ParseText("""
            class Order {
                var Number: int
                var Pending: bool
                init(number: int, pending: bool) {
                    Number = number
                    Pending = pending
                }
            }
            """);
        var compilation = Compilation.Create("OrderSymbolStability", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var declaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var type = (INamedTypeSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration)!;
        var properties = type.GetMembers().OfType<SourcePropertySymbol>().ToArray();
        var fields = properties.Select(p => (Property: p, Field: p.BackingField!)).ToArray();
        var accessors = properties.Select(p => (Property: p, Get: p.GetMethod!, Set: p.SetMethod!, Value: p.SetMethod!.Parameters.Single())).ToArray();
        for (var iteration = 0; iteration < 3; iteration++)
        {
            Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
            foreach (var propertySyntax in tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>())
                _ = compilation.GetSemanticModel(tree).GetDeclaredSymbol(propertySyntax);
            foreach (var (property, field) in fields)
            {
                Assert.Same(field, property.BackingField);
                Assert.Same(field, Assert.Single(type.GetMembers(field.Name)));
                Assert.Same(property, field.AssociatedSymbol);
            }
            foreach (var (property, get, set, value) in accessors)
            {
                Assert.Same(get, property.GetMethod);
                Assert.Same(set, property.SetMethod);
                Assert.Same(value, property.SetMethod!.Parameters.Single());
                Assert.Same(get, Assert.Single(type.GetMembers(get.Name)));
                Assert.Same(set, Assert.Single(type.GetMembers(set.Name)));
            }
        }
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("; ", emitted.Diagnostics));
        var runtimeType = Assembly.Load(image.ToArray()).GetType("Order")!;
        var instance = Activator.CreateInstance(runtimeType, [42, true])!;
        Assert.Equal(42, runtimeType.GetProperty("Number")!.GetValue(instance));
        runtimeType.GetProperty("Pending")!.SetValue(instance, false);
        Assert.Equal(false, runtimeType.GetProperty("Pending")!.GetValue(instance));
        foreach (var (property, get, set, value) in accessors)
        {
            Assert.Same(get, property.GetMethod);
            Assert.Same(set, property.SetMethod);
            Assert.Same(value, property.SetMethod!.Parameters.Single());
            Assert.Same(set, Assert.Single(type.GetMembers(set.Name)));
        }
    }
}

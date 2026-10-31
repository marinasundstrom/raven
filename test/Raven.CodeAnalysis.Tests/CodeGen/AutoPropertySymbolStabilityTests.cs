using System.Reflection;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class AutoPropertySymbolStabilityTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release, "class")]
    [InlineData(OptimizationLevel.Release, "struct")]
    [InlineData(OptimizationLevel.Debug, "class")]
    [InlineData(OptimizationLevel.Debug, "struct")]
    public void RepeatedBindingKeepsOneCanonicalAccessorPerProperty(OptimizationLevel optimization, string kind)
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
            """.Replace("class Order", kind + " Order"));
        var compilation = Compilation.Create("OrderSymbolStability", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithOptimizationLevel(optimization));
        var declaration = tree.GetRoot().DescendantNodes().OfType<TypeDeclarationSyntax>().Single();
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
        foreach (var (_, get, set, _) in accessors)
        {
            foreach (var accessor in new[] { get, set })
            {
                if (kind == "struct")
                {
                    // The conservative .NET portable profile still delegates value owners
                    // to the ordinary generator; this native fix must not broaden it.
                    Assert.False(SourceCallablePlan.TryCreate(accessor, out _, ReflectionEmitCapabilities.Shared));
                    continue;
                }
                Assert.True(SourceCallablePlan.TryCreate(accessor, out var plan, ReflectionEmitCapabilities.Shared));
                Assert.Equal(EmissionDeclarationKind.PropertyAccessor, plan!.DeclarationKind);
                Assert.True(plan.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), failure?.Detail);
                var noFields = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(),
                    Enum.GetValues<LinearInstructionKind>().Where(k => k is not (LinearInstructionKind.LoadField or LinearInstructionKind.StoreField)),
                    [EmissionDeclarationKind.PropertyAccessor], methodVisibilities: [Accessibility.Public]);
                Assert.False(plan.TryLowerBody(compilation, _ => false, out _, out _, noFields));
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

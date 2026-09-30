using System.Reflection;

using Mono.Cecil;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class AttributeUsageInheritanceTests
{
    [Fact]
    public void ImportedAttributeTypeCanBeDisplayedWithoutRecursiveUnionClassification()
    {
        var compilation = Compilation.Create("AttributeDisplay", [], TestMetadataReferences.Default,
            CompilationOptions.DotNet);
        var attribute = compilation.GetTypeByMetadataName("System.AttributeUsageAttribute")!;
        Assert.NotEmpty(attribute.GetAttributes());
        Assert.False(attribute.IsUnion);
        Assert.Contains("AttributeUsageAttribute", attribute.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
    }

    [Fact]
    public void SourceAttributeInheritsUsageFromBaseType()
    {
        AssertUsageDiagnostics("""
            import System.*
            [AttributeUsage(AttributeTargets.Class, AllowMultiple: true)]
            open class BaseAttribute : Attribute { }
            class DerivedAttribute : BaseAttribute { }
            [Derived][Derived]
            class Example { }
            [Derived]
            struct Invalid { }
            """, TestMetadataReferences.Default, expectedInvalidTargets: 1, expectedDuplicates: 0);
    }

    [Fact]
    public void DirectUsageOverridesInheritedMultiplicityWithDefaultFalse()
    {
        AssertUsageDiagnostics("""
            import System.*
            [AttributeUsage(AttributeTargets.Class, AllowMultiple: true)]
            open class BaseAttribute : Attribute { }
            [AttributeUsage(AttributeTargets.Class)]
            class DerivedAttribute : BaseAttribute { }
            [Derived][Derived]
            class Example { }
            """, TestMetadataReferences.Default, expectedInvalidTargets: 0, expectedDuplicates: 1);
    }

    [Fact]
    public void ReferenceOnlyAttributeInheritsUsageWithoutHostExecution()
    {
        using var assembly = AssemblyDefinition.CreateAssembly(
            new AssemblyNameDefinition("UsageReference_" + Guid.NewGuid().ToString("N"), new Version(1, 0)),
            "UsageReference", ModuleKind.Dll);
        var module = assembly.MainModule;
        var referenceMarker = typeof(System.Runtime.CompilerServices.ReferenceAssemblyAttribute).GetConstructor(Type.EmptyTypes)!;
        assembly.CustomAttributes.Add(new CustomAttribute(module.ImportReference(referenceMarker)));
        var baseType = new TypeDefinition("Contracts", "BaseAttribute", Mono.Cecil.TypeAttributes.Public,
            module.ImportReference(typeof(Attribute)));
        module.Types.Add(baseType);
        var usage = new CustomAttribute(module.ImportReference(typeof(AttributeUsageAttribute).GetConstructor([typeof(AttributeTargets)])!));
        usage.ConstructorArguments.Add(new CustomAttributeArgument(module.ImportReference(typeof(AttributeTargets)), (int)AttributeTargets.Class));
        usage.Properties.Add(new Mono.Cecil.CustomAttributeNamedArgument("AllowMultiple", new CustomAttributeArgument(module.TypeSystem.Boolean, true)));
        baseType.CustomAttributes.Add(usage);
        var derived = new TypeDefinition("Contracts", "DerivedAttribute", Mono.Cecil.TypeAttributes.Public, baseType);
        module.Types.Add(derived);
        derived.Methods.Add(new MethodDefinition(".ctor",
            Mono.Cecil.MethodAttributes.Public | Mono.Cecil.MethodAttributes.SpecialName | Mono.Cecil.MethodAttributes.RTSpecialName,
            module.TypeSystem.Void));
        using var image = new MemoryStream();
        assembly.Write(image);
        var bytes = image.ToArray();
        Assert.Throws<BadImageFormatException>(() => Assembly.Load(bytes));
        var compilation = AssertUsageDiagnostics("""
            import Contracts.*
            [Derived][Derived]
            class Example { }
            [Derived]
            struct Invalid { }
            """, [MetadataReference.CreateFromFile(typeof(object).Assembly.Location), MetadataReference.CreateFromImage(bytes)],
            expectedInvalidTargets: 1, expectedDuplicates: 0);
        var importedBase = compilation.GetTypeByMetadataName("Contracts.BaseAttribute")!;
        var importedUsage = Assert.Single(importedBase.GetAttributes());
        Assert.Equal("AttributeUsageAttribute", importedUsage.AttributeClass!.Name);
        Assert.Equal((int)AttributeTargets.Class, Assert.Single(importedUsage.ConstructorArguments).Value);
        var namedArgument = Assert.Single(importedUsage.NamedArguments);
        Assert.Equal("AllowMultiple", namedArgument.Key);
        Assert.Equal(true, namedArgument.Value.Value);
        Assert.Same(importedUsage, Assert.Single(importedBase.GetAttributes()));
    }

    private static Compilation AssertUsageDiagnostics(string source, MetadataReference[] references, int expectedInvalidTargets, int expectedDuplicates)
    {
        var compilation = Compilation.Create("UsageConsumer", [SyntaxTree.ParseText(source)], references,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var tree = compilation.SyntaxTrees.Single();
        var model = compilation.GetSemanticModel(tree);
        foreach (var declaration in tree.GetRoot().DescendantNodes().Where(node => node is ClassDeclarationSyntax or StructDeclarationSyntax))
            _ = model.GetDeclaredSymbol(declaration)?.GetAttributes();
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        Assert.Equal(expectedInvalidTargets, errors.Count(d => d.Id == "RAV0502"));
        Assert.Equal(expectedDuplicates, errors.Count(d => d.Id == "RAV0503"));
        Assert.All(errors, d => Assert.Contains(d.Id, new[] { "RAV0502", "RAV0503" }));
        return compilation;
    }
}

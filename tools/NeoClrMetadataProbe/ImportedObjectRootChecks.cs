using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class ImportedObjectRootChecks
{
    internal static void Run(string corePath)
    {
        var core = AssemblyDefinition.ReadAssembly(File.ReadAllBytes(corePath), false);
        var graph = new AssemblyBuilder(new("ImportedRoot", new(1, 0, 0, 0)), core.Identity);
        var root = graph.AddNativeObjectRoot();
        var box = graph.AddGenericClass("Example", "Box", ["T"], root);
        var native = NeoClrMetadataReference.ReadAssembly(RuntimeAssemblyContainer.WriteLibraryBinary(graph));
        var references = new Raven.CodeAnalysis.MetadataReference[] { Raven.CodeAnalysis.MetadataReference.CreateFromFile(corePath), native };
        var imports = new MetadataImportOptions(core.Identity.Name).WithObjectAssemblyName("ImportedRoot");
        Compilation Create(MetadataImportOptions configuration, TargetPlatform target = TargetPlatform.NeoCLR) => Compilation.Create("Consumer",
            [SyntaxTree.ParseText("public class Item { }\npublic class Holder { public val Item: object? => null }")], references,
            CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary).WithRuntimeTypeOfContract(null)
                .WithMetadataImportOptions(configuration).WithTargetPlatform(target));
        var selected = Create(imports);
        var selectedRoot = selected.GetSpecialType(SpecialType.System_Object);
        if (selectedRoot.ContainingAssembly.Name != "ImportedRoot" || selectedRoot.BaseType is not null ||
            !SymbolEqualityComparer.Default.Equals(selected.GetTypeByMetadataName("System.Object"), selectedRoot) ||
            !SymbolEqualityComparer.Default.Equals(selected.GetTypeByMetadataName("Item")!.BaseType, selectedRoot) ||
            !SymbolEqualityComparer.Default.Equals(selected.GetTypeByMetadataName("Example.Box`1")!.BaseType, selectedRoot))
            throw new Exception("imported Object ownership lost across source and generic bases");
        var property = selected.GetTypeByMetadataName("Holder")!.GetMembers("Item").OfType<IPropertySymbol>().Single();
        if (!SymbolEqualityComparer.Default.Equals(property.Type.GetNonNullableType(), selectedRoot))
            throw new Exception("object keyword did not select imported root");
        var attribute = selected.GetTypeByMetadataName("System.Attribute");
        if (attribute is not null && !SymbolEqualityComparer.Default.Equals(attribute.BaseType, selectedRoot))
            throw new Exception("bootstrap base facts retained a competing Object root");
        var namespaceConsumer = Compilation.Create("NamespaceConsumer",
            [SyntaxTree.ParseText("namespace System.Data.Json\nimport System.*\npublic class NamedRoot {\npublic val Upper: Object? => null\npublic val Qualified: System.Object? => null\npublic val Keyword: object? => null\n}")],
            references, selected.Options);
        var namespaceRoot = namespaceConsumer.GetSpecialType(SpecialType.System_Object);
        foreach (var member in namespaceConsumer.GetTypeByMetadataName("System.Data.Json.NamedRoot")!.GetMembers().OfType<IPropertySymbol>())
            if (!SymbolEqualityComparer.Default.Equals(member.Type.GetNonNullableType(), namespaceRoot))
                throw new Exception("namespace type spelling retained bootstrap Object: " + member.Name + " / " + member.Type.ToDisplayString() + " / " + member.Type.ContainingAssembly?.Name + " / expected " + namespaceRoot.ContainingAssembly.Name);
        var missing = Create(imports.WithObjectAssemblyName("Missing"));
        if (missing.GetSpecialType(SpecialType.System_Object).TypeKind != TypeKind.Error)
            throw new Exception("missing root fell back to bootstrap");
        if (Create(imports.WithObjectAssemblyName(core.Identity.Name)).GetSpecialType(SpecialType.System_Object).TypeKind != TypeKind.Error)
            throw new Exception("CLI bootstrap accepted as native root provider");
        var invalidGraph = new AssemblyBuilder(new("WrongRoot", new(1, 0, 0, 0)), core.Identity);
        invalidGraph.AddClass("System", "Object");
        var invalidReference = NeoClrMetadataReference.ReadAssembly(RuntimeAssemblyContainer.WriteLibraryBinary(invalidGraph));
        var invalid = Compilation.Create("InvalidRootConsumer", [],
            [references[0], invalidReference], selected.Options.WithMetadataImportOptions(imports.WithObjectAssemblyName("WrongRoot")));
        try
        {
            invalid.GetSpecialType(SpecialType.System_Object);
            throw new Exception("non-root declaration accepted as native Object owner");
        }
        catch (InvalidDataException) { }
        var ordinary = Create(imports.WithObjectAssemblyName(null));
        if (ordinary.GetSpecialType(SpecialType.System_Object).ContainingAssembly.Name == "ImportedRoot")
            throw new Exception("unselected root acquired platform ownership");
        var dotnet = Create(imports, TargetPlatform.DotNet);
        if (dotnet.GetSpecialType(SpecialType.System_Object).TypeKind != TypeKind.Error)
            throw new Exception(".NET accepted native root selection");
        try
        {
            new MetadataImportOptions(core.Identity.Name, null, null, true).WithObjectAssemblyName("ImportedRoot");
            throw new Exception("source/imported root conflict accepted");
        }
        catch (ArgumentException) { }
        Console.WriteLine("PASS explicit imported Object semantic root, generic bases and selection rejection");
    }
}

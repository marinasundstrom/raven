using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;

namespace NeoClrMetadataProbe;

internal static class TypeAttributeChecks
{
    // Uses the compiler-emitted JSON consumer, built with its explicit ownership
    // manifest, to validate import independently of runtime attribute discovery.
    internal static void Run(string corePath, string dataPath, string consumerPath)
    {
        var catalog = NeoClrReferenceCatalog.ReadNative(corePath, [dataPath, consumerPath]);
        var options = CompilationOptions.NeoCLR.WithRuntimeTypeOfContract(null)
            .WithTargetCoreAssemblyName(catalog.CoreIdentity.Name)
            .WithMetadataImportOptions(new MetadataImportOptions(catalog.CoreIdentity.Name)
                .WithObjectAssemblyName(catalog.CoreIdentity.Name).WithNativeMetadata());
        var compilation = Compilation.Create("AttributeInspector", [], catalog.References.ToArray(), options);
        var type = compilation.GetTypeByMetadataName("JsonConvertersDemo.AnnotatedCode")!;
        var attribute = type.GetAttributes().Single();
        var argument = attribute.ConstructorArguments.Single();
        if (argument.Kind != TypedConstantKind.Type || argument.Value is not ITypeSymbol represented ||
            !SymbolEqualityComparer.Default.Equals(represented,
                compilation.GetTypeByMetadataName("JsonConvertersDemo.AnnotatedCodeConverter")))
            throw new Exception("Imported converter attribute lost its nominal type identity");
        var property = compilation.GetTypeByMetadataName("JsonConvertersDemo.AnnotatedCatalog")!
            .GetMembers("Featured").OfType<IPropertySymbol>().Single();
        var propertyArgument = property.GetAttributes().Single().ConstructorArguments.Single();
        if (propertyArgument.Kind != TypedConstantKind.Type || propertyArgument.Value is not ITypeSymbol propertyType ||
            !SymbolEqualityComparer.Default.Equals(propertyType,
                compilation.GetTypeByMetadataName("JsonConvertersDemo.ProductCodeConverter")))
            throw new Exception("Imported property converter attribute lost its nominal type identity");
        Console.WriteLine("PASS native typeof attribute symbol import: type and property converter identities");
    }
}

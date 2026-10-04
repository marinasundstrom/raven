using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;

namespace NeoClrMetadataProbe;

internal static class FlagsSymbolChecks
{
    internal static void Run(string corePath)
    {
        var core = AssemblyDefinition.ReadAssembly(File.ReadAllBytes(corePath), expectedExtended: false);
        var graph = new AssemblyBuilder(new("NativeFlags", new(1, 0, 0, 0)), core.Identity);
        var flags = graph.AddEnum("Example", "Options");
        flags.SetEnumFlags(); flags.AddEnumMember("One", 1);
        graph.AddEnum("Example", "Ordinary").AddEnumMember("One", 1);
        var reference = NeoClrMetadataReference.ReadAssembly(RuntimeAssemblyContainer.WriteLibraryBinary(graph));
        var compilation = Compilation.Create("FlagSymbols", [],
            [Raven.CodeAnalysis.MetadataReference.CreateFromFile(corePath), reference], CompilationOptions.NeoCLR);
        var symbol = compilation.GetTypeByMetadataName("Example.Options")!;
        var marker = symbol.GetAttributes().Single();
        var expected = compilation.GetTypeByMetadataName("System.FlagsAttribute");
        if (!SymbolEqualityComparer.Default.Equals(marker.AttributeClass, expected) || marker.AttributeConstructor.Parameters.Length != 0 ||
            marker.ConstructorArguments.Length != 0 || marker.NamedArguments.Length != 0 ||
            compilation.GetTypeByMetadataName("Example.Ordinary")!.GetAttributes().Length != 0)
            throw new Exception("native flags fact was not projected into the semantic attribute contract");
        Console.WriteLine("PASS native flags metadata-to-symbol attributes");
    }
}

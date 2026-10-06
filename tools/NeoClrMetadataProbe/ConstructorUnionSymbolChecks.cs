using NeoCLR.Metadata.Experimental.Model;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class ConstructorUnionSymbolChecks
{
    internal static void Run(string corePath, string seedPath, string libraryPath)
    {
        var catalog = NeoClrReferenceCatalog.Read(corePath, [libraryPath], seedPath);
        var bootstrap = catalog.Bootstrap;
        var options = CompilationOptions.NeoCLR.WithRuntimeTypeOfContract(null).WithMetadataImportOptions(new MetadataImportOptions(catalog.CoreIdentity.Name,
            new Dictionary<SpecialType, string> { [SpecialType.System_String] = "Numbers" }));
        var source = """
            public struct Left { }
            public struct Right { }
            public union Choice(Left | Right)
            public union Generic<T>(T | Right)
            """;
        var library = Compilation.Create("ConstructorUnions", [SyntaxTree.ParseText(source)], catalog.References.ToArray(),
            options.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var emit = NeoClrCompilationEmitter.EmitMetadataAssembly(library, stream,
            new(new("ConstructorUnions", new(1, 0, 0, 0)), catalog.CoreIdentity, catalog.Dependencies));
        Check(emit.Success, string.Join("; ", emit.Diagnostics));
        var reference = NeoClrMetadataReference.ReadAssembly(stream.ToArray(), bootstrap);
        var consumer = Compilation.Create("Consumer", [SyntaxTree.ParseText("func Main() -> unit { }")],
            [.. catalog.References, reference], options);
        Check(!consumer.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), string.Join("; ", consumer.GetDiagnostics()));
        var choice = (IUnionSymbol)consumer.GetTypeByMetadataName("Choice")!;
        Check(choice.DeclaredCaseTypes.IsEmpty, "constructor union invented named cases");
        Check(choice.MemberTypes.Select(t => t.Name).SequenceEqual(new[] { "Left", "Right" }), "constructor alternatives lost");
        var generic = (IUnionSymbol)consumer.GetTypeByMetadataName("Generic`1")!;
        Check(generic.MemberTypes[0].TypeKind == TypeKind.TypeParameter, "open generic scope lost");
        var constructed = (IUnionSymbol)((INamedTypeSymbol)generic).Construct(consumer.GetSpecialType(SpecialType.System_Int32));
        Check(constructed.MemberTypes[0].SpecialType == SpecialType.System_Int32, "constructed generic scope lost");
        Console.WriteLine("PASS native constructor union alternatives and generic scopes");
    }
    private static void Check(bool condition, string message) { if (!condition) throw new Exception(message); }
}

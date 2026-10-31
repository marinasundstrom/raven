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
        var libraryName = Path.GetFileNameWithoutExtension(libraryPath);
        var imports = new MetadataImportOptions(catalog.CoreIdentity.Name,
            new Dictionary<SpecialType, string> { [SpecialType.System_String] = libraryName });
        if (libraryName == "System.Runtime") imports = imports.WithObjectAssemblyName(libraryName);
        var options = CompilationOptions.NeoCLR.WithRuntimeTypeOfContract(null).WithMetadataImportOptions(imports);
        var source = """
            public struct Left { }
            public struct Right { }
            public union Choice(Left | Right)
            public union Generic<T>(T | Right)
            public union Generic<T, U>(T | U)
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
        var pair = (IUnionSymbol)consumer.GetTypeByMetadataName("Generic`2")!;
        Check(pair.MemberTypes.Length == 2 && pair.MemberTypes.All(t => t.TypeKind == TypeKind.TypeParameter),
            "second generic arity alternatives lost");
        Check(consumer.GetTypeByMetadataName("Generic") is null, "constructor union invented a case companion");
        Console.WriteLine("PASS native constructor union alternatives, generic scopes and multiple arities without companions");
    }
    private static void Check(bool condition, string message) { if (!condition) throw new Exception(message); }
}

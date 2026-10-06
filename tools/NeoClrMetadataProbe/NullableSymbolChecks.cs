using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NullableSymbolChecks
{
    internal static void Run(string corePath)
    {
        var coreDefinition = AssemblyDefinition.ReadAssembly(File.ReadAllBytes(corePath), expectedExtended: false);
        var core = coreDefinition.Identity;
        var source = """
            public interface Box<T> { }
            public static class Api {
                public static func Echo(value: object?) -> object? => value
                public static func Nested(value: object?[]) -> object?[] => value
                public static func Generic(value: Box<object?>) -> Box<object?> => value
                public static func Open<T>(value: T?) -> T? => value
                public static func Strict(value: object) -> object => value
            }
            """;
        var bootstrap = NeoClrPrimitiveBootstrap.ReadAssembly(File.ReadAllBytes(corePath));
        var coreReference = bootstrap.Reference;
        var library = Compilation.Create("NullableLibrary", [SyntaxTree.ParseText(source)], [coreReference],
            CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var emit = NeoClrCompilationEmitter.EmitMetadataAssembly(library, stream, new(new("NullableLibrary", new(1, 0, 0, 0)), core, [new NeoClrMetadataDependency(coreReference, coreDefinition, core)]));
        Check(emit.Success, string.Join("; ", emit.Diagnostics));
        var reference = NeoClrMetadataReference.ReadAssembly(stream.ToArray(), bootstrap);
        var consumer = Compilation.Create("NullableConsumer", [SyntaxTree.ParseText("func Main() -> unit { Api.Echo(null) }")],
            [coreReference, reference], CompilationOptions.NeoCLR);
        Check(!consumer.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), string.Join("; ", consumer.GetDiagnostics()));
        var api = consumer.GetTypeByMetadataName("Api")!;
        IMethodSymbol Method(string name) => api.GetMembers(name).OfType<IMethodSymbol>().Single();
        var echo = Method("Echo");
        Check(echo.Parameters[0].Type.IsNullable && echo.ReturnType.IsNullable, "reference annotations lost");
        var nested = Method("Nested");
        Check(!nested.ReturnType.IsNullable && ((IArrayTypeSymbol)nested.ReturnType).ElementType.IsNullable, "array element annotation lost");
        Check(((INamedTypeSymbol)Method("Generic").ReturnType).TypeArguments.Single().IsNullable, "generic argument annotation lost");
        Check(Method("Open").ReturnType.IsNullable && Method("Open").Parameters[0].Type.IsNullable, "method generic annotation lost");
        var constructed = Method("Open").Construct(consumer.GetSpecialType(SpecialType.System_String));
        Check(constructed.ReturnType.IsNullable && constructed.Parameters[0].Type.IsNullable, "constructed method lost annotations");
        Check(!Method("Strict").ReturnType.IsNullable, "unannotated type became nullable");
        var bad = Compilation.Create("InvalidNull", [SyntaxTree.ParseText("func Main() -> unit { Api.Strict(null) }")],
            [coreReference, reference], CompilationOptions.NeoCLR);
        Check(bad.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error), "nonnullable parameter accepted null");
        Console.WriteLine("PASS native callable nullable emission, nested signatures, generic scope and null admission");
    }
    private static void Check(bool condition, string message) { if (!condition) throw new Exception(message); }
}

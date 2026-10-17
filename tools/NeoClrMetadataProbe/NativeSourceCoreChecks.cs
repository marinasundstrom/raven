using NeoCLR.Metadata.Experimental.Model;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NativeSourceCoreChecks
{
    internal static void Run(string directory)
    {
        Directory.CreateDirectory(directory);
        const string name = "SourceCore";
        var source = """
            namespace System
            public abstract class Object {
                virtual func ToString() -> string => "object"
                virtual func GetHashCode() -> int => 0
                virtual func Equals(other: Object?) -> bool => false
            }
            public abstract class ValueType {}
            public struct Void {}
            public struct Boolean { private var m_value: bool }
            public struct Int32 { private var m_value: int }
            public struct Int64 { private var m_value: long }
            public class String { private var m_value: string }
            public abstract class Attribute {}
            public abstract class Enum {}
            public interface Sequence<T> { val Count: int { get; } }
            public interface Cursor<T> {}
            public class Array<T> : Sequence<T> {
                private var m_value: T[]
                private init() {}
                val Length: int => m_value.Length
                val Count: int => m_value.Length
            }
            public abstract class Array {}
            public extension Views<T> for Sequence<T> {
                func GetCount() -> int => self.Count
            }
            public class Example {
                static func Answer() -> int => Read([40, 2]) + 40
                static func Read(items: int[]) -> int {
                    let view: Sequence<int> = items
                    return view.GetCount()
                }
            }
            """;
        var ownershipPath = Path.Combine(directory, "ownership.json");
        File.WriteAllText(ownershipPath, System.Text.Json.JsonSerializer.Serialize(new {
            version = 1,
            libraries = new[] { new { assemblyName = name, sources = new[] { "Core.rvn" },
                types = new[] { "System.Int32", "System.Int64", "System.Boolean", "System.String", "System.Void", "System.Sequence`1", "System.Cursor`1", "System.Views" } } },
            nativePrimitives = new Dictionary<string, string> { ["System.Int32"] = name, ["System.Int64"] = name,
                ["System.Boolean"] = name, ["System.String"] = name },
            unit = new { assemblyName = name, typeName = "System.Void" },
            iteration = new { assemblyName = name, iterableTypeName = "System.Sequence`1", iteratorTypeName = "System.Cursor`1", arrayShapeTypeName = "System.Array`1" }
        }));
        var provider = new NeoClrProjectMetadataProvider();
        var projectPath = Path.Combine(directory, "Core.rvnproj");
        var properties = new Dictionary<string, string> { ["RavenNeoClrSourceCore"] = "true", ["RavenNeoClrBootstrapOwnership"] = ownershipPath };
        var configured = provider.Load(projectPath, name, CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary), properties, []);
        if (configured.References.Length != 0 || provider.GetConfiguration(projectPath).Catalog.CoreReference is not null)
            throw new Exception("Source core acquired a reference core.");
        var options = configured.Options;
        var tree = SyntaxTree.ParseText(source);
        var markers = SyntaxTree.ParseText("""
            namespace System.Runtime.CompilerServices
            public enum MethodImplOptions { InternalCall = 4096 }
            public class MethodImplAttribute : System.Attribute {
                init(options: MethodImplOptions) {}
            }
            """);
        var native = SyntaxTree.ParseText("""
            import System.Runtime.CompilerServices.*
            module neoCLR.Runtime
            [MethodImpl(MethodImplOptions.InternalCall)]
            internal extern func GCCollectionCount() -> long;
            """);
        var compilation = Compilation.Create(name, [native, tree, markers], [], options);
        provider.GetConfiguration(projectPath).Validate(compilation);
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (errors.Length != 0) throw new Exception(string.Join("\n", errors.Select(d => d.ToString())));
        foreach (var special in new[] { SpecialType.System_Object, SpecialType.System_Int32, SpecialType.System_Boolean, SpecialType.System_String })
            if (compilation.GetSpecialType(special).ContainingAssembly?.Name != name || compilation.GetSpecialType(special).SpecialType != special)
                throw new Exception("Source identity lost: " + special);
        var identity = new AssemblyIdentity(name, new Version(1, 0, 0, 0));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image, null, new EmitOptions().WithBackend(provider.GetConfiguration(projectPath).CreateEmissionBackend(name)));
        if (!emitted.Success) throw new Exception(string.Join("\n", emitted.Diagnostics));
        File.WriteAllBytes(Path.Combine(directory, "SourceCore.dll"), image.ToArray());
        var corePath = Path.Combine(directory, "SourceCore.dll");
        var catalog = NeoClrReferenceCatalog.ReadNative(corePath, []);
        var consumerOptions = options.WithOutputKind(OutputKind.ConsoleApplication)
            .WithMetadataImportOptions(new MetadataImportOptions(name).WithNativeMetadata().WithObjectAssemblyName(name));
        var consumerSource = "func Main() -> int => System.Example.Answer()";
        var consumer = Compilation.Create("Consumer", [SyntaxTree.ParseText(consumerSource)], catalog.References.ToArray(), consumerOptions);
        using var consumerImage = new MemoryStream();
        var consumerResult = consumer.Emit(consumerImage, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(
            new(new("Consumer", new Version(1, 0, 0, 0)), identity, catalog.Dependencies))));
        if (!consumerResult.Success) throw new Exception(string.Join("\n", consumerResult.Diagnostics));
        File.WriteAllBytes(Path.Combine(directory, "Consumer.dll"), consumerImage.ToArray());
        var completionSource = "func Main() -> int { System. }";
        var completionTree = SyntaxTree.ParseText(completionSource);
        var completion = Compilation.Create("Completion", [completionTree], catalog.References.ToArray(), consumerOptions);
        var names = new CompletionService().GetCompletions(completion, completionTree,
            completionSource.IndexOf("System.", StringComparison.Ordinal) + "System.".Length).Select(c => c.DisplayText).ToArray();
        if (!names.Contains("Int32") || names.Any(n => n.StartsWith("Action", StringComparison.Ordinal)))
            throw new Exception("Native source-core completion failed.");
        var invalid = Compilation.Create("WrongIdentity", [tree], [], options);
        if (!invalid.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error))
            throw new Exception("Mismatched source core identity was accepted.");
        foreach (var property in new[] { "RavenNeoClrCoreReference", "RavenNeoClrNativeCoreReference", "RavenNeoClrObjectLibrary", "RavenNeoClrBootstrapIntrinsics", "RavenMetadataCoreAssemblyName" })
        {
            var bad = new Dictionary<string, string>(properties) { [property] = "invalid" };
            try
            {
                provider.Load(projectPath, name, options, bad, []);
                throw new Exception("Source core accepted incompatible property: " + property);
            }
            catch (InvalidDataException) { }
        }
        var rejected = Compilation.Create(name, [native, tree, SyntaxTree.ParseText(markers.GetText().ToString().Replace("4096", "1", StringComparison.Ordinal))], [], options);
        using var rejectedImage = new MemoryStream();
        if (rejected.Emit(rejectedImage, null, new EmitOptions().WithBackend(provider.GetConfiguration(projectPath).CreateEmissionBackend(name))).Success || rejectedImage.Length != 0)
            throw new Exception("Invalid native-call flags were admitted.");
        Console.WriteLine("PASS reference-free source core, source native-call markers, array views, consumer emission/completion and configuration/signature rejection");
    }
}

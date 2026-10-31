using NeoCLR.Metadata.Experimental.Model;
using NeoCLR.Metadata.Experimental.Introspection;

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
                static func ReferenceEquals(left: Object?, right: Object?) -> bool => false
                virtual func ToString() -> string => "object"
                virtual func GetHashCode() -> int => 0
                virtual func Equals(other: Object?) -> bool => false
            }
            public abstract class ValueType {}
            public struct HashCode {
                static func Combine(left: int, right: int) -> int => left + right
            }
            public interface EquatableTo<in T> { func Equals(other: T) -> bool }
            public struct ImportedValue : EquatableTo<ImportedValue> {
                func Equals(other: ImportedValue) -> bool => true
                func GetHashCode() -> int => 7
            }
            public struct MissingHash : EquatableTo<MissingHash> {
                func Equals(other: MissingHash) -> bool => true
            }
            public struct Lookalike {
                func Equals(other: Lookalike) -> bool => true
                func GetHashCode() -> int => 7
            }
            public struct WrongSelf : EquatableTo<int> {
                func Equals(other: int) -> bool => true
                func Equals(other: WrongSelf) -> bool => true
                func GetHashCode() -> int => 7
            }
            public record struct NativeKey(Value: int)
            public union Choice<T> {
                case Some(T)
                case None
            }
            public record struct UnionKey(Value: Choice<int>)
            public struct Storage { val Value: int = 0 }
            public record struct GenericKey<T>(Value: int)
            public struct RuntimeTypeHandle {}
            public struct Void {}
            public struct Boolean { private var m_value: bool }
            public struct Byte { private var m_value: byte }
            public interface ZeroValue { static val Zero: Self { get; } }
            public struct Int32 : ZeroValue {
                private var m_value: int
                static val Zero: int => 0
            }
            public struct Int64 { private var m_value: long }
            public class String { private var m_value: string }
            public abstract class Attribute {}
            public abstract class Enum : ValueType {}
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
                static func Answer() -> int {
                    let items = neoCLR.Runtime.ReserveArray<int>(2)
                    items[0] = 40
                    items[1] = 2
                    var sum = 0
                    for item in items {
                        sum += item
                    }
                    return sum + Read(items) - 2
                }
                static func ReadUninitialized() -> int => neoCLR.Runtime.ReserveArray<int>(1)[0]
                static func NegativeLength() -> int => neoCLR.Runtime.ReserveArray<int>(-1).Length
                static func Read(items: int[]) -> int {
                    let view: Sequence<int> = items
                    return view.GetCount()
                }
            }
            """;
        var ownershipPath = Path.Combine(directory, "ownership.json");
        File.WriteAllText(ownershipPath, System.Text.Json.JsonSerializer.Serialize(new
        {
            version = 1,
            libraries = new[] { new { assemblyName = name, sources = new[] { "Core.rvn" },
                types = new[] { "System.RuntimeTypeHandle", "System.Int32", "System.Int64", "System.Boolean", "System.Byte", "System.String", "System.Void", "System.Sequence`1", "System.Cursor`1", "System.Views" } } },
            nativePrimitives = new Dictionary<string, string>
            {
                ["System.RuntimeTypeHandle"] = name,
                ["System.Int32"] = name,
                ["System.Int64"] = name,
                ["System.Boolean"] = name,
                ["System.Byte"] = name,
                ["System.String"] = name
            },
            unit = new { assemblyName = name, typeName = "System.Void" },
            self = new { assemblyName = name, typeName = "System.Runtime.CompilerServices.Self" },
            iteration = new { assemblyName = name, iterableTypeName = "System.Sequence`1", iteratorTypeName = "System.Cursor`1", arrayShapeTypeName = "System.Array`1" }
        }));
        var provider = new NeoClrProjectMetadataProvider();
        var projectPath = Path.Combine(directory, "Core.rvnproj");
        var properties = new Dictionary<string, string> { ["RavenNeoClrSourceCore"] = "true", ["RavenNeoClrRuntimeContracts"] = ownershipPath };
        var configured = provider.Load(projectPath, name, CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary), properties, []);
        if (configured.References.Length != 0 || provider.GetConfiguration(projectPath).Catalog.CoreReference is not null)
            throw new Exception("Source core acquired a reference core.");
        if (configured.Options.RuntimeDisposalContract?.AllowAsyncScopeExitCleanup == true)
            throw new Exception("Source core without an async provider enabled async cleanup.");
        properties["RavenNeoClrAsyncLibrary"] = name;
        var asyncConfigured = provider.Load(projectPath, name,
            CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary), properties, []);
        if (asyncConfigured.Options.RuntimeDisposalContract?.AllowAsyncScopeExitCleanup != true)
            throw new Exception("Source core with an async provider did not enable async cleanup.");
        properties.Remove("RavenNeoClrAsyncLibrary");
        // This metadata fixture does not provide the library formatting helpers.
        var options = configured.Options.WithSynthesizeStructuralToString(false);
        var tree = SyntaxTree.ParseText(source);
        var markers = SyntaxTree.ParseText("""
            namespace System.Runtime.CompilerServices
            public struct Self {}
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
            internal extern func ReserveArray<T>(count: int) -> T[];
            internal extern func TypeHandle<T>() -> System.RuntimeTypeHandle;
            """);
        var overloads = SyntaxTree.ParseText("""
            namespace Example
            public func Choose(value: int) -> int => value
            public func Choose(value: long) -> long => value
            """);
        foreach (var trees in new[] {
            new[] { overloads, native, markers, tree },
            new[] { tree, markers, native, overloads },
            new[] { native, overloads, tree, markers } })
        {
            var ordered = Compilation.Create(name, trees, [], options);
            var orderingErrors = ordered.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            if (orderingErrors.Length != 0) throw new Exception(string.Join("\n", orderingErrors.Select(d => d.ToString())));
            var service = ordered.GetSemanticModel(native).GetDeclaredSymbol(native.GetRoot()
                .DescendantNodes().OfType<FunctionStatementSyntax>().First()) as IMethodSymbol;
            if (service?.GetAttributes().SingleOrDefault()?.ConstructorArguments.SingleOrDefault().Value is not int flags || flags != 4096)
                throw new Exception("Source native-call marker depended on declaration order.");
        }
        var compilation = Compilation.Create(name, [overloads, native, tree, markers], [], options);
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
        var snapshot = AssemblyDefinition.ReadNativeAssembly(image.ToArray());
        var views = new MetadataLoadContext([snapshot]).Resolve(snapshot.Identity).GetTypes();
        foreach (var recordName in new[] { "NativeKey", "UnionKey", "GenericKey`1" })
        {
            var recordSymbol = compilation.GetTypeByMetadataName("System." + recordName)!;
            var equatable = compilation.GetTypeByMetadataName("System.EquatableTo`1")!;
            if (!recordSymbol.Interfaces.Any(i => SymbolEqualityComparer.Default.Equals(i, equatable.Construct(recordSymbol))))
                throw new Exception("Automatic native record interface missing: " + recordName);
            var recordMethods = snapshot.MainModule.Types.Single(t => t.Name == recordName).Methods;
            foreach (var member in new[] { "op_Equality", "op_Inequality", "GetHashCode" })
                if (recordMethods.Count(m => m.Name == member) != 1)
                    throw new Exception("Native record value member missing: " + member);
            if (recordMethods.Count(m => m.Name == "Equals") != 2)
                throw new Exception("Native record requires typed and Object equality.");
        }
        var selfContract = views.Single(t => t.Name == "ZeroValue").GetMethods().Single(m => m.Name == "get_Zero");
        var zeroImplementation = views.Single(t => t.Name == "Int32").GetMethods().Single(m => m.Name == "get_Zero");
        if (selfContract.ReturnType is not SelfTypeInfo || zeroImplementation.ReturnType is not PrimitiveTypeInfo { Kind: PrimitiveType.Int32 })
            throw new Exception("Source Self contract or concrete primitive implementation was erased.");
        File.WriteAllBytes(Path.Combine(directory, "SourceCore.dll"), image.ToArray());
        var corePath = Path.Combine(directory, "SourceCore.dll");
        var consumerProject = Path.Combine(directory, "Consumer.rvnproj");
        var nativeProperties = new Dictionary<string, string>
        {
            ["RavenNeoClrNativeCoreReference"] = corePath,
            ["RavenNeoClrRuntimeContracts"] = ownershipPath
        };
        var nativeConfiguration = provider.Load(consumerProject, "Consumer", CompilationOptions.NeoCLR.WithOutputKind(OutputKind.ConsoleApplication),
            nativeProperties, []);
        var nativeProject = provider.GetConfiguration(consumerProject);
        var catalog = nativeProject.Catalog;
        var consumerOptions = nativeConfiguration.Options;
        if (consumerOptions.RuntimeSelfTypeContract?.AssemblyName != name ||
            consumerOptions.RuntimeIterationContract?.AssemblyName != name ||
            !provider.GetInputPaths(consumerProject, nativeProperties).Contains(ownershipPath))
            throw new Exception("Native runtime contracts or editor inputs were lost.");
        NominalConstrainedCallChecks.Run(catalog.References.ToArray(), consumerOptions,
            assemblyName => new NeoClrEmissionBackend(new(new(assemblyName, new Version(1, 0, 0, 0)), identity, catalog.Dependencies)), directory);
        NativeSelfChecks.Run(catalog.References.ToArray(), consumerOptions,
            assemblyName => new NeoClrEmissionBackend(new(new(assemblyName, new Version(1, 0, 0, 0)), identity, catalog.Dependencies)),
            directory, snapshot);
        var consumerSource = "func Main() -> int => System.Example.Answer() + System.Int32.Zero";
        var consumer = Compilation.Create("Consumer", [SyntaxTree.ParseText(consumerSource)], catalog.References.ToArray(), consumerOptions);
        nativeProject.Validate(consumer);
        using var consumerImage = new MemoryStream();
        var consumerResult = consumer.Emit(consumerImage, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(
            new(new("Consumer", new Version(1, 0, 0, 0)), identity, catalog.Dependencies))));
        if (!consumerResult.Success) throw new Exception(string.Join("\n", consumerResult.Diagnostics));
        File.WriteAllBytes(Path.Combine(directory, "Consumer.dll"), consumerImage.ToArray());
        foreach (var component in new[] { "ImportedValue", "MissingHash", "Lookalike", "WrongSelf" })
        {
            var recordSource = $"record struct Wrapped(Value: System.{component})\nfunc Main() -> int {{ let value: System.{component} = default; let key = Wrapped(value); if key.Equals(key) && key.GetHashCode() == key.GetHashCode() {{ return 1 }}; return 0 }}";
            var recordConsumer = Compilation.Create("Record" + component, [SyntaxTree.ParseText(recordSource)],
                catalog.References.ToArray(), consumerOptions.WithSynthesizeStructuralToString(false));
            using var recordImage = new MemoryStream();
            var recordResult = recordConsumer.Emit(recordImage, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(
                new(new("Record" + component, new Version(1, 0, 0, 0)), identity, catalog.Dependencies))));
            if (component == "ImportedValue")
            {
                if (!recordResult.Success) throw new Exception(string.Join("\n", recordResult.Diagnostics));
            }
            else if (recordResult.Success || !recordResult.Diagnostics.Any(d => d.ToString().Contains(
                "record equality interface requires supported component equality and hashing", StringComparison.Ordinal)))
                throw new Exception("Imported record component contract was not rejected: " + component);
        }
        foreach (var component in new[] { "int", "System.Storage", "System.Choice<System.Storage>" })
        {
            var interfaceSource = $"record struct ContractKey(Value: {component})\nfunc Main() -> int {{ let key: ContractKey = default; let contract: System.EquatableTo<ContractKey> = key; if contract.Equals(key) {{ return 1 }}; return 0 }}";
            var interfaceConsumer = Compilation.Create("RecordInterface", [SyntaxTree.ParseText(interfaceSource)],
                catalog.References.ToArray(), consumerOptions.WithSynthesizeStructuralToString(false));
            using var interfaceImage = new MemoryStream();
            var interfaceResult = interfaceConsumer.Emit(interfaceImage, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(
                new(new("RecordInterface", new Version(1, 0, 0, 0)), identity, catalog.Dependencies))));
            if (component == "int")
            {
                if (!interfaceResult.Success) throw new Exception(string.Join("\n", interfaceResult.Diagnostics));
            }
            else if (interfaceResult.Success || !interfaceResult.Diagnostics.Any(d => d.ToString().Contains(
                "record equality interface requires supported component equality and hashing", StringComparison.Ordinal)))
                throw new Exception("Unsupported record interface did not produce the record contract diagnostic: " + string.Join("\n", interfaceResult.Diagnostics));
        }
        var genericConsumer = Compilation.Create("RecordGenericMissingPolicy",
            [SyntaxTree.ParseText("record struct GenericValue<T>(Value: T)\nfunc Main() -> int => 0")],
            catalog.References.ToArray(), consumerOptions.WithSynthesizeStructuralToString(false));
        using var genericImage = new MemoryStream();
        var genericResult = genericConsumer.Emit(genericImage, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(
            new(new("RecordGenericMissingPolicy", new Version(1, 0, 0, 0)), identity, catalog.Dependencies))));
        if (genericResult.Success || !genericResult.Diagnostics.Any(d => d.ToString().Contains(
            "record equality interface requires supported component equality and hashing", StringComparison.Ordinal)))
            throw new Exception("Generic equality without its native type policy was not clearly rejected: " + string.Join("\n", genericResult.Diagnostics));
        var displayConsumer = Compilation.Create("RecordDisplayMissingHelpers",
            [SyntaxTree.ParseText("record struct Label(Value: int)\nfunc Main() -> int { let text = Label(42).ToString(); return 0 }")],
            catalog.References.ToArray(), consumerOptions.WithSynthesizeStructuralToString(true));
        using var displayImage = new MemoryStream();
        var displayResult = displayConsumer.Emit(displayImage, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(
            new(new("RecordDisplayMissingHelpers", new Version(1, 0, 0, 0)), identity, catalog.Dependencies))));
        if (displayResult.Success || !displayResult.Diagnostics.Any(d => d.ToString().Contains(
            "record formatting requires supported scalar components and native text helpers", StringComparison.Ordinal)))
            throw new Exception("Record display without native text helpers was not clearly rejected: " + string.Join("\n", displayResult.Diagnostics));
        foreach (var operation in new[] { "ReadUninitialized", "NegativeLength" })
        {
            var faultConsumer = Compilation.Create(operation, [SyntaxTree.ParseText("func Main() -> int => System.Example." + operation + "()")], catalog.References.ToArray(), consumerOptions);
            using var faultImage = new MemoryStream();
            var faultResult = faultConsumer.Emit(faultImage, null, new EmitOptions().WithBackend(new NeoClrEmissionBackend(
                new(new(operation, new Version(1, 0, 0, 0)), identity, catalog.Dependencies))));
            if (!faultResult.Success) throw new Exception(string.Join("\n", faultResult.Diagnostics));
            File.WriteAllBytes(Path.Combine(directory, operation + ".dll"), faultImage.ToArray());
        }
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
        foreach (var property in new[] { "RavenNeoClrCoreReference", "RavenNeoClrNativeCoreReference", "RavenNeoClrObjectLibrary", "RavenNeoClrBootstrapIntrinsics", "RavenMetadataCoreAssemblyName", "RavenNeoClrBootstrapOwnership" })
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
        foreach (var invalidDeclaration in new[] {
            native.GetText().ToString().Replace("count: int", "count: long", StringComparison.Ordinal),
            native.GetText().ToString().Replace("-> T[]", "-> T[2]", StringComparison.Ordinal),
            native.GetText().ToString().Replace("TypeHandle<T>()", "TypeHandle<T>(value: T)", StringComparison.Ordinal),
            native.GetText().ToString().Replace("TypeHandle<T>() -> System.RuntimeTypeHandle", "TypeHandle<T>() -> int", StringComparison.Ordinal) })
        {
            var invalidStorage = Compilation.Create(name, [SyntaxTree.ParseText(invalidDeclaration), tree, markers], [], options);
            using var invalidStorageImage = new MemoryStream();
            if (invalidStorage.Emit(invalidStorageImage, null, new EmitOptions().WithBackend(provider.GetConfiguration(projectPath).CreateEmissionBackend(name))).Success || invalidStorageImage.Length != 0)
                throw new Exception("Invalid source intrinsic signature was admitted.");
        }
        Console.WriteLine("PASS reference-free source core, source native-call markers, managed array reservation, array views, consumer emission/completion and configuration/signature rejection");
    }
}

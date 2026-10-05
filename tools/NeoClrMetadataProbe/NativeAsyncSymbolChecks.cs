using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NativeAsyncSymbolChecks
{
    internal static void Run(string corePath, string libraryPath, string? seedPath = null)
    {
        var bootstrap = NeoClrPrimitiveBootstrap.ReadAssembly(File.ReadAllBytes(corePath));
        var native = NeoClrMetadataReference.ReadAssembly(File.ReadAllBytes(libraryPath), bootstrap);
        var core = AssemblyDefinition.ReadAssembly(File.ReadAllBytes(corePath), expectedExtended: false);
        var owner = native.Definition.Name;
        Compilation Create(string? selected, TargetPlatform platform = TargetPlatform.NeoCLR) => Compilation.Create("AsyncContract",
            [SyntaxTree.ParseText("""
                import System.Tasks.*
                async func Value() -> Task<int> { return 42 }
                async func Forward(value: Task<int>) -> Task<int> { return await value }
                class Worker {
                    public static async func Value() -> Task<int> { return 42 }
                }
                async func Captured() -> Task<int> {
                    let source = Promise<int>()
                    TaskQueue.Default.Post(() => { _ = source.Complete(41) })
                    let value = await source.Task
                    return value + 1
                }
                """)], [bootstrap.Reference, native], CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
                .WithRuntimeTypeOfContract(null).WithTargetPlatform(platform)
                .WithMetadataImportOptions(new MetadataImportOptions(core.Identity.Name).WithAsyncAssemblyName(selected)));
        var unselected = Create(null);
        if (unselected.GetTypeByMetadataName("System.Tasks.Task`1")!.SpecialType != SpecialType.None)
            throw new Exception("unselected native Task acquired a special identity");
        if (!unselected.GetDiagnostics().Any(d => d.Id == "RAV2704"))
            throw new Exception("unselected native async contract unexpectedly accepted");
        var selected = Create(owner);
        var task = selected.GetSpecialType(SpecialType.System_Threading_Tasks_Task_T);
        var builder = selected.GetSpecialType(SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder_T);
        if (task.ContainingAssembly.Name != owner || builder.ContainingAssembly.Name != owner ||
            !ReferenceEquals(task, ((IAssemblySymbol)selected.GetAssemblyOrModuleSymbol(native)!).GetTypeByMetadataName("System.Tasks.Task`1")))
            throw new Exception("native async selection used a different owner");
        var diagnostics = selected.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (diagnostics.Length != 0) throw new Exception(string.Join("\n", diagnostics.Select(d => d.ToString())));
        var constructed = task.Construct(selected.GetSpecialType(SpecialType.System_Int32));
        if (constructed.SpecialType != SpecialType.System_Threading_Tasks_Task_T ||
            constructed.GetMembers("GetResult").OfType<IMethodSymbol>().Single().ReturnType.SpecialType != SpecialType.System_Int32)
            throw new Exception("constructed native Task lost its result signature");
        if (seedPath is not null)
        {
            using var emitted = new MemoryStream();
            var emission = NeoClrCompilationEmitter.EmitMetadataAssembly(selected, emitted,
                new(new("AsyncContract", new(1, 0, 0, 0)), core.Identity,
                    [new NeoClrMetadataDependency(native, core.Identity),
                    new NeoClrMetadataDependency(bootstrap.Reference, core, core.Identity, NativeLibraryDefinition.ReadAssembly(File.ReadAllBytes(seedPath)))], bootstrapReference: bootstrap.Reference));
            if (!emission.Success) throw new Exception(string.Join("\n", emission.Diagnostics));
            var assembly = AssemblyDefinition.ReadNativeAssembly(emitted.ToArray());
            if (assembly.MainModule.Types.Count(t => t.Name.StartsWith("<>c__AsyncStateMachine", StringComparison.Ordinal)) != 4)
                throw new Exception("native async state-machine definitions missing");
            if (assembly.MainModule.Types.Count(t => t.Name.StartsWith("<>c__AsyncStateMachine", StringComparison.Ordinal) &&
                t.DeclaringType?.Name == "Worker") != 1)
                throw new Exception("class async state machine lost its declaring owner");
        }
        if (seedPath is not null)
        {
            var entry = Compilation.Create("AsyncEntry", [SyntaxTree.ParseText("""
                import System.Tasks.*
                async func Main() -> Task<int> { return 23 }
                """)], [bootstrap.Reference, native], selected.Options.WithOutputKind(OutputKind.ConsoleApplication));
            foreach (var includeRuntime in new[] { true, false })
            {
                using var output = new MemoryStream();
                if (!includeRuntime) output.WriteByte(42);
                var dependencies = new List<NeoClrMetadataDependency> { new(native, core.Identity) };
                if (includeRuntime) dependencies.Add(new(bootstrap.Reference, core, core.Identity,
                    NativeLibraryDefinition.ReadAssembly(File.ReadAllBytes(seedPath))));
                var result = NeoClrCompilationEmitter.EmitMetadataAssembly(entry, output,
                    new(new("AsyncEntry", new(1, 0, 0, 0)), core.Identity, dependencies, bootstrapReference: bootstrap.Reference));
                if (includeRuntime)
                {
                    if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
                    var assembly = AssemblyDefinition.ReadNativeAssembly(output.ToArray());
                    if (assembly.EntryPoint is not { } startup ||
                        !startup.TryGetStaticInt32Signature(out var parameters, out var returnsValue) || parameters != 0 || !returnsValue ||
                        startup.Name == "Main")
                        throw new Exception("async entry did not retain a separate Int32 startup contract");
                }
                else if (result.Success || !output.ToArray().SequenceEqual(new byte[] { 42 }) || output.Position != 1)
                    throw new Exception("missing async runtime binding published output");
            }
        }
        var unsupportedEntry = Compilation.Create("UnsupportedEntry", [SyntaxTree.ParseText("""
            import System.Tasks.*
            async func Main() -> Task<System.Result<int, string>> { return default }
            """)], [bootstrap.Reference, native], selected.Options.WithOutputKind(OutputKind.ConsoleApplication));
        using (var output = new MemoryStream())
        {
            output.WriteByte(42);
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(unsupportedEntry, output,
                new(new("UnsupportedEntry", new(1, 0, 0, 0)), core.Identity,
                    [new NeoClrMetadataDependency(native, core.Identity)], bootstrapReference: bootstrap.Reference));
            if (result.Success || !output.ToArray().SequenceEqual(new byte[] { 42 }) || output.Position != 1)
                throw new Exception("unsupported async entry result published output");
        }
        foreach (var source in new[]
        {
            "import System.Tasks.*\nclass Worker<T> { public async func Value(value: T) -> Task<T> { return value } }",
            "import System.Tasks.*\nasync func Value<T>(value: T) -> Task<T> { return value }"
        })
        {
            var unsupported = Compilation.Create("UnsupportedAsync", [SyntaxTree.ParseText(source)],
                [bootstrap.Reference, native], selected.Options);
            var errors = unsupported.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
            if (errors.Length != 0) throw new Exception(string.Join("\n", errors.Select(d => d.ToString())));
            using var output = new MemoryStream();
            output.WriteByte(42);
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(unsupported, output,
                new(new("UnsupportedAsync", new(1, 0, 0, 0)), core.Identity,
                    [new NeoClrMetadataDependency(native, core.Identity)], bootstrapReference: bootstrap.Reference));
            if (result.Success || !output.ToArray().SequenceEqual(new byte[] { 42 }) || output.Position != 1)
                throw new Exception("unsupported async shape published output");
        }
        var malformed = new AssemblyBuilder(new("MalformedAsync", new(1, 0, 0, 0)), core.Identity);
        malformed.AddGenericInterface("System.Tasks", "Task", ["T"]);
        malformed.AddGenericValueType("System.Runtime.CompilerServices", "AsyncTaskMethodBuilder", ["T"]);
        var malformedReference = NeoClrMetadataReference.ReadAssembly(RuntimeAssemblyContainer.WriteLibraryBinary(malformed), bootstrap);
        var malformedCompilation = Compilation.Create("MalformedConsumer", [], [bootstrap.Reference, malformedReference],
            CompilationOptions.NeoCLR.WithRuntimeTypeOfContract(null).WithMetadataImportOptions(
                new MetadataImportOptions(core.Identity.Name).WithAsyncAssemblyName("MalformedAsync")));
        if (!malformedCompilation.GetDiagnostics().Any(d => d.Id == "RAVT003"))
            throw new Exception("malformed native async declarations were accepted");
        foreach (var invalid in new[] { Create("Missing.Async"), Create(core.Identity.Name), Create(owner, TargetPlatform.DotNet) })
        {
            using var output = new MemoryStream();
            output.WriteByte(42);
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(invalid, output,
                new(new("AsyncContract", new(1, 0, 0, 0)), core.Identity,
                    [new NeoClrMetadataDependency(native, core.Identity)], bootstrapReference: bootstrap.Reference));
            if (result.Success || !output.ToArray().SequenceEqual(new byte[] { 42 }) || output.Position != 1 ||
                !result.Diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error))
                throw new Exception("unsupported async emission did not fail before publication");

        }
        Console.WriteLine("PASS explicit native Task/builder identity, async/await binding, substitution and rejection controls");
    }
}

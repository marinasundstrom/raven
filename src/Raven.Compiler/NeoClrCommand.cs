#if NEOCLR_METADATA
using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

using MetadataReference = Raven.CodeAnalysis.MetadataReference;

namespace Raven;

// Explicit experimental command: keep the bounded native backend separate from the
// default CLI emitter and its project/publish/runtime artifact policies.
internal static class NeoClrCommand
{
    internal static int Run(string[] args)
    {
        if (args.Length == 1 && args[0] is "--help" or "-h")
        {
            Console.WriteLine("rvnc neoclr [-o output.dll] [--library] [--core-reference NeoCLR.CoreProbe.dll] [--reference library.dll] source.rvn ...");
            Console.WriteLine("Optional --runtime-seed System.neox binds the explicitly selected CLI core bootstrap to retained runtime services; it imports no additional symbols.");
            Console.WriteLine("Optional --bootstrap-intrinsics authorizes checked storage from the explicitly selected --core-reference.");
            Console.WriteLine("Optional --bootstrap-ownership manifest.json selects source-library ownership and iteration contracts.");
            Console.WriteLine("Experimental PE/#Neo output. References are imported directly from supported native metadata.");
            Console.WriteLine("Legacy bridge: --system-symbols System.neox --system-method System.Math.Min/2 (repeat explicit selections).");
            Console.WriteLine("Static Int32 callable view only; not a complete core-library import. Run with the matching --system assembly.");
            Console.WriteLine("Native references require --core-reference; without references the legacy host primitive bootstrap remains available.");
            Console.WriteLine("Uses explicitly selected CLI primitive references for binding. No project, publish, PDB or managed execution support.");
            return 0;
        }
        string? projectionPath = null;
        try
        {
            var sources = new List<string>();
            var referencePaths = new List<string>();
            string? output = null;
            var library = false;
            var bootstrapIntrinsics = false;
            string? systemPath = null;
            string? runtimeSeedPath = null;
            string? corePath = null;
            BootstrapOwnershipManifest? ownership = null;
            var systemMethods = new List<string>();
            for (var i = 0; i < args.Length; i++)
            {
                switch (args[i])
                {
                    case "-o":
                        if (output is not null || ++i == args.Length) throw new ArgumentException("Specify -o once with an output path.");
                        output = Path.GetFullPath(args[i]);
                        break;
                    case "--bootstrap-ownership":
                        if (ownership is not null || ++i == args.Length) throw new ArgumentException("Specify --bootstrap-ownership once with a manifest path.");
                        ownership = BootstrapOwnershipManifest.Read(args[i]);
                        break;
                    case "--bootstrap-intrinsics":
                        if (bootstrapIntrinsics) throw new ArgumentException("Specify --bootstrap-intrinsics once.");
                        bootstrapIntrinsics = true;
                        break;
                    case "--runtime-seed":
                        if (runtimeSeedPath is not null || ++i == args.Length) throw new ArgumentException("Specify --runtime-seed once with an explicit native System path.");
                        runtimeSeedPath = Path.GetFullPath(args[i]);
                        break;
                    case "--core-reference":
                        if (corePath is not null || ++i == args.Length) throw new ArgumentException("Specify --core-reference once with a CLI primitive core path.");
                        corePath = Path.GetFullPath(args[i]);
                        break;
                    case "--reference":
                        if (++i == args.Length) throw new ArgumentException("--reference requires a native assembly path.");
                        referencePaths.Add(Path.GetFullPath(args[i]));
                        break;
                    case "--system-symbols":
                        if (systemPath is not null || ++i == args.Length) throw new ArgumentException("Specify --system-symbols once with a native System path.");
                        systemPath = Path.GetFullPath(args[i]);
                        break;
                    case "--system-method":
                        if (++i == args.Length) throw new ArgumentException("--system-method requires Qualified.Name/Int32-arity.");
                        systemMethods.Add(args[i]);
                        break;
                    case "--library":
                        if (library) throw new ArgumentException("Specify --library once.");
                        library = true;
                        break;
                    default:
                        if (args[i].StartsWith('-')) throw new ArgumentException("Unknown native compiler option: " + args[i]);
                        sources.Add(Path.GetFullPath(args[i]));
                        break;
                }
            }
            if (sources.Count == 0) throw new ArgumentException("Provide at least one Raven source file.");
            output ??= Path.ChangeExtension(sources[0], ".dll");
            if (File.Exists(output)) throw new IOException("Output already exists: " + output);
            if (sources.Concat(referencePaths).Contains(output, StringComparer.OrdinalIgnoreCase))
                throw new ArgumentException("Output must differ from every input path.");
            if (sources.Distinct(StringComparer.OrdinalIgnoreCase).Count() != sources.Count ||
                referencePaths.Distinct(StringComparer.OrdinalIgnoreCase).Count() != referencePaths.Count)
                throw new ArgumentException("Duplicate input path.");
            if (runtimeSeedPath is not null && (corePath is null || systemPath is not null || string.Equals(runtimeSeedPath, output, StringComparison.OrdinalIgnoreCase)))
                throw new ArgumentException("--runtime-seed requires --core-reference, a distinct output and no legacy --system-symbols selection.");
            if (bootstrapIntrinsics && corePath is null)
                throw new ArgumentException("--bootstrap-intrinsics requires an explicit --core-reference.");
            if (referencePaths.Count > 0 && corePath is null)
                throw new ArgumentException("Direct native references require --core-reference NeoCLR.CoreProbe.dll.");
            if (corePath is not null && string.Equals(corePath, output, StringComparison.OrdinalIgnoreCase))
                throw new ArgumentException("Output must differ from the primitive core input.");
            var name = Path.GetFileNameWithoutExtension(output);
            var host = corePath is null ? typeof(object).Assembly.GetName() : System.Reflection.AssemblyName.GetAssemblyName(corePath);
            var core = new AssemblyIdentity(host.Name!, host.Version!, host.CultureName ?? "", Convert.ToHexString(host.GetPublicKeyToken() ?? []));
            var console = MetadataReference.CreateFromFile(typeof(Console).Assembly.Location);
            var references = new List<MetadataReference> {
                MetadataReference.CreateFromFile(typeof(object).Assembly.Location), console,
                MetadataReference.CreateFromFile(System.Reflection.Assembly.Load("System.Runtime").Location)
            };
            MetadataReference? bootstrapReference = null;
            NeoClrPrimitiveBootstrap? primitiveBootstrap = null;
            if (corePath is not null)
            {
                if (new FileInfo(corePath).Length > 4 * 1024 * 1024) throw new InvalidDataException("Core snapshot exceeds 4 MiB.");
                primitiveBootstrap = NeoClrPrimitiveBootstrap.ReadAssembly(File.ReadAllBytes(corePath));
                var primitiveCore = primitiveBootstrap.Reference;
                references.Clear();
                references.Add(primitiveCore);
                console = primitiveCore;
                if (bootstrapIntrinsics) bootstrapReference = primitiveCore;
            }
            var dependencies = new List<NeoClrMetadataDependency>();
            if (runtimeSeedPath is not null)
            {
                if (new FileInfo(runtimeSeedPath).Length > 8 * 1024 * 1024 || new FileInfo(corePath!).Length > 4 * 1024 * 1024)
                    throw new InvalidDataException("Runtime seed or core snapshot exceeds its image limit.");
                var seed = NativeLibraryDefinition.ReadAssembly(File.ReadAllBytes(runtimeSeedPath));
                if (seed.ModuleName != "System") throw new InvalidDataException("Runtime seed must declare module System.");
                foreach (var type in ownership?.Libraries.SelectMany(library => library.Types) ?? [])
                {
                    var nativeName = System.Text.RegularExpressions.Regex.Replace(type.Replace('+', '.'), @"`\d+", "");
                    if (seed.TypeNames.Contains(nativeName))
                        throw new InvalidDataException("Runtime seed duplicates a source-owned declaration: " + type);
                }
                dependencies.Add(new(references[0], AssemblyDefinition.ReadAssembly(File.ReadAllBytes(corePath!), expectedExtended: false), core, seed));
            }
            foreach (var path in referencePaths)
            {
                if (new FileInfo(path).Length > 4 * 1024 * 1024) throw new InvalidDataException("Native PE reference exceeds 4 MiB: " + path);
                var bytes = File.ReadAllBytes(path);
                // Validate executable metadata, then import native declarations without a CLI projection.
                NativeAssemblyDefinition.ReadAssembly(RuntimeAssemblyContainer.Read(bytes));
                var reference = NeoClrMetadataReference.ReadAssembly(bytes, primitiveBootstrap!);
                references.Add(reference);
                dependencies.Add(new(reference, core));
            }
            NeoClrSystemSymbols? systemSymbols = null;
            if ((systemPath is null) != (systemMethods.Count == 0)) throw new ArgumentException("Use --system-symbols with explicit --system-method selections.");
            if (systemPath is not null)
            {
                // Host facades can forward System.Math back to host implementations.
                // This partial native mode retains only the primitive core bootstrap.
                if (corePath is null) references.RemoveRange(1, 2);
                if (new FileInfo(systemPath).Length > 8 * 1024 * 1024) throw new InvalidDataException("System image exceeds 8 MiB.");
                var system = NativeLibraryDefinition.ReadAssembly(File.ReadAllBytes(systemPath));
                if (system.ModuleName != "System") throw new InvalidDataException("System symbol input must declare module System.");
                var selected = new List<NativeFunctionDefinition>();
                foreach (var selection in systemMethods)
                {
                    var split = selection.LastIndexOf('/');
                    if (split <= 0 || !int.TryParse(selection[(split + 1)..], out var count) || count is < 0 or > 256)
                        throw new ArgumentException("Invalid System method selection: " + selection);
                    var matches = system.Functions.Where(f => f.Name == selection[..split] && f.TryGetStaticInt32Signature(out var arity) && arity == count).ToArray();
                    if (matches.Length != 1) throw new InvalidDataException("Unsupported, missing or ambiguous native System callable: " + selection);
                    selected.Add(matches[0]);
                }
                var identity = new AssemblyIdentity("NeoCLR.System.StaticView", new Version(1, 0, 0, 0));
                var projection = system.CreateStaticInt32ReferenceAssembly(identity, core, selected);
                projectionPath = Path.GetTempFileName();
                File.WriteAllBytes(projectionPath, projection);
                var reference = MetadataReference.CreateFromFile(projectionPath);
                references.Add(reference);
                systemSymbols = new(reference, identity.Name, system, selected);
            }
            var trees = sources.Select(path => SyntaxTree.ParseText(File.ReadAllText(path), path: path)).ToArray();
            var compilationOptions = (corePath is null ? new CompilationOptions() : CompilationOptions.NeoCLR)
                .WithOutputKind(library ? OutputKind.DynamicallyLinkedLibrary : OutputKind.ConsoleApplication);
            if (ownership is not null) compilationOptions = ownership.Apply(compilationOptions);
            var compilation = Compilation.Create(name, trees, references.ToArray(), compilationOptions);
            ownership?.Validate(compilation);
            using var image = new MemoryStream();
            var backend = new NeoClrEmissionBackend(
                new(new(name, new Version(1, 0, 0, 0)), core, dependencies, systemSymbols is null ? console : null, systemSymbols, bootstrapReference));
            var result = compilation.Emit(image, null, new EmitOptions().WithBackend(backend));
            foreach (var diagnostic in result.Diagnostics) Console.Error.WriteLine(diagnostic);
            if (!result.Success) return 1;
            // No destination is opened until binding and native encoding succeed.
            using var destination = new FileStream(output, FileMode.CreateNew, FileAccess.Write);
            image.Position = 0;
            image.CopyTo(destination);
            return 0;
        }
        catch (Exception error) when (error is ArgumentException or IOException or InvalidDataException or UnauthorizedAccessException or BadImageFormatException)
        {
            Console.Error.WriteLine("neoCLR: " + error.Message);
            return 1;
        }
        finally
        {
            if (projectionPath is not null) File.Delete(projectionPath);
        }
    }
}
#endif

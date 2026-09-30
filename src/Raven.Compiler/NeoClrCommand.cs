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
            Console.WriteLine("rvnc neoclr [-o output.dll] [--library] [--reference library.dll] source.rvn ...");
            Console.WriteLine("Experimental PE/#Neo output; Int32/Unit static subset. References must be native PE/#Neo assemblies.");
            Console.WriteLine("Uses host .NET primitive references for binding. No project, publish, PDB or managed execution support.");
            return 0;
        }
        try
        {
            var sources = new List<string>();
            var referencePaths = new List<string>();
            string? output = null;
            var library = false;
            for (var i = 0; i < args.Length; i++)
            {
                switch (args[i])
                {
                    case "-o":
                        if (output is not null || ++i == args.Length) throw new ArgumentException("Specify -o once with an output path.");
                        output = Path.GetFullPath(args[i]);
                        break;
                    case "--reference":
                        if (++i == args.Length) throw new ArgumentException("--reference requires a native assembly path.");
                        referencePaths.Add(Path.GetFullPath(args[i]));
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
            var name = Path.GetFileNameWithoutExtension(output);
            var host = typeof(object).Assembly.GetName();
            var core = new AssemblyIdentity(host.Name!, host.Version!, host.CultureName ?? "", Convert.ToHexString(host.GetPublicKeyToken() ?? []));
            var console = MetadataReference.CreateFromFile(typeof(Console).Assembly.Location);
            var references = new List<MetadataReference> {
                MetadataReference.CreateFromFile(typeof(object).Assembly.Location), console,
                MetadataReference.CreateFromFile(System.Reflection.Assembly.Load("System.Runtime").Location)
            };
            var dependencies = new List<NeoClrMetadataDependency>();
            foreach (var path in referencePaths)
            {
                if (new FileInfo(path).Length > 4 * 1024 * 1024) throw new InvalidDataException("Native PE reference exceeds 4 MiB: " + path);
                var bytes = File.ReadAllBytes(path);
                // Validate the native execution contract before exposing the reference-only CLI view.
                NativeAssemblyDefinition.ReadAssembly(RuntimeAssemblyContainer.Read(bytes));
                var definition = RuntimeAssemblyContainer.ReadCliProjection(bytes);
                var reference = MetadataReference.CreateFromFile(path);
                references.Add(reference);
                dependencies.Add(new(reference, definition, core));
            }
            var trees = sources.Select(path => SyntaxTree.ParseText(File.ReadAllText(path), path: path)).ToArray();
            var compilation = Compilation.Create(name, trees, references.ToArray(),
                new CompilationOptions(library ? OutputKind.DynamicallyLinkedLibrary : OutputKind.ConsoleApplication));
            using var image = new MemoryStream();
            var result = NeoClrCompilationEmitter.EmitMetadataAssembly(compilation, image,
                new(new(name, new Version(1, 0, 0, 0)), core, dependencies, console));
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
    }
}
#endif

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
    private static int RunProject(string[] args)
    {
        try
        {
            if (args.Length is not (2 or 4) || (args.Length == 4 && args[2] != "--run"))
                throw new ArgumentException("Usage: rvnc neoclr --project App.rvnproj [--run /path/to/neoclr]");
            var projectPath = Path.GetFullPath(args[1]);
            var provider = new NeoClrProjectMetadataProvider();
            var workspace = RavenWorkspace.Create(projectSystemService: new MsBuildProjectSystemService(
                RavenProjectConventions.Default, false, null, null, metadataProvider: provider));
            var id = workspace.OpenProject(projectPath);
            var project = workspace.CurrentSolution.GetProject(id)!;
            if (args.Length == 4 && project.CompilationOptions?.OutputKind == OutputKind.DynamicallyLinkedLibrary)
                throw new InvalidDataException("Cannot run a library project.");
            var config = provider.GetConfiguration(projectPath);
            var compilation = workspace.GetCompilation(id);
            config.Validate(compilation);
            using var image = new MemoryStream();
            var result = compilation.Emit(image, null, new EmitOptions().WithBackend(config.CreateEmissionBackend(project.AssemblyName!)));
            foreach (var diagnostic in result.Diagnostics) Console.Error.WriteLine(diagnostic);
            if (!result.Success) return 1;
            var output = Path.Combine(Path.GetDirectoryName(projectPath)!, "bin", "neoclr", project.AssemblyName + ".dll");
            if (project.Documents.Any(d => string.Equals(d.FilePath, output, StringComparison.OrdinalIgnoreCase)) ||
                workspace.Services.ProjectSystemService!.GetMetadataInputPaths(projectPath).Contains(output, StringComparer.OrdinalIgnoreCase))
                throw new InvalidDataException("Native output must differ from project inputs.");
            Directory.CreateDirectory(Path.GetDirectoryName(output)!);
            var temporary = output + "." + Guid.NewGuid().ToString("N") + ".tmp";
            var xmlOutput = project.DocumentationOptions?.GenerateXmlDocumentation == true
                ? Path.GetFullPath(project.DocumentationOptions.XmlDocumentationFile ?? Path.ChangeExtension(output, ".xml"), Path.GetDirectoryName(projectPath)!) : null;
            var temporaryXml = temporary + ".xml";
            var markdownOutput = project.DocumentationOptions?.GenerateMarkdownDocumentation == true
                ? Path.GetFullPath(project.DocumentationOptions.MarkdownDocumentationOutputPath ?? Path.ChangeExtension(output, ".docs"), Path.GetDirectoryName(projectPath)!) : null;
            var temporaryMarkdown = temporary + ".docs";
            var protectedPaths = workspace.Services.ProjectSystemService!.GetMetadataInputPaths(projectPath)
                .Concat(project.Documents.Select(d => d.FilePath).OfType<string>())
                .Append(projectPath).Append(output).ToArray();
            if (xmlOutput is not null && protectedPaths.Contains(xmlOutput, StringComparer.OrdinalIgnoreCase))
                throw new InvalidDataException("Documentation output must differ from native project inputs and assembly output.");
            try
            {
                if (xmlOutput is not null)
                    DocumentationEmitter.WriteDocumentation(compilation, DocumentationFormat.Xml,
                        temporaryXml, project.DocumentationOptions!.GenerateXmlDocumentationFromMarkdownComments);
                if (markdownOutput is not null)
                {
                    if (!markdownOutput.EndsWith(".docs", StringComparison.OrdinalIgnoreCase) ||
                        protectedPaths.Append(xmlOutput ?? output).Any(path =>
                            string.Equals(path, markdownOutput, StringComparison.OrdinalIgnoreCase) ||
                            path.StartsWith(markdownOutput + Path.DirectorySeparatorChar, StringComparison.OrdinalIgnoreCase)))
                        throw new InvalidDataException("Native Markdown output must be a dedicated .docs directory outside source inputs.");
                    DocumentationEmitter.WriteDocumentation(compilation, DocumentationFormat.Markdown, temporaryMarkdown);
                }
                File.WriteAllBytes(temporary, image.ToArray());
                if (xmlOutput is not null)
                {
                    Directory.CreateDirectory(Path.GetDirectoryName(xmlOutput)!);
                    File.Move(temporaryXml, xmlOutput, true);
                }
                if (markdownOutput is not null)
                {
                    Directory.CreateDirectory(Path.GetDirectoryName(markdownOutput)!);
                    if (Directory.Exists(markdownOutput)) Directory.Delete(markdownOutput, true);
                    Directory.Move(temporaryMarkdown, markdownOutput);
                }
                File.Move(temporary, output, true);
            }
            finally
            {
                if (File.Exists(temporary)) File.Delete(temporary);
                if (File.Exists(temporaryXml)) File.Delete(temporaryXml);
                if (Directory.Exists(temporaryMarkdown)) Directory.Delete(temporaryMarkdown, true);
            }
            Console.WriteLine("Native build output: " + output);
            if (args.Length == 2) return 0;
            var start = new System.Diagnostics.ProcessStartInfo(Path.GetFullPath(args[3])) { UseShellExecute = false };
            start.ArgumentList.Add("run"); start.ArgumentList.Add(output);
            if (config.RuntimeSeedPath is { } seed) { start.ArgumentList.Add("--system"); start.ArgumentList.Add(seed); }
            foreach (var path in config.ReferencePaths) { start.ArgumentList.Add("--module"); start.ArgumentList.Add(path); }
            if (config.ObjectRootPath is { } objectRoot) { start.ArgumentList.Add("--object-root"); start.ArgumentList.Add(objectRoot); }
            using var process = System.Diagnostics.Process.Start(start)!;
            process.WaitForExit();
            return process.ExitCode;
        }
        catch (Exception error) when (error is ArgumentException or IOException or InvalidDataException or
            UnauthorizedAccessException or BadImageFormatException or NotSupportedException or InvalidOperationException)
        {
            Console.Error.WriteLine("neoCLR project: " + error.Message);
            return 1;
        }
    }

    internal static int Run(string[] args)
    {
        if (args.Length > 0 && args[0] == "--project") return RunProject(args);
        if (args.Length == 1 && args[0] is "--help" or "-h")
        {
            Console.WriteLine("rvnc neoclr [-o output.dll] [--library] [--core-reference NeoCLR.CoreProbe.dll] [--reference library.dll] source.rvn ...");
            Console.WriteLine("Optional --runtime-seed System.neox binds the explicitly selected CLI core bootstrap to retained runtime services; it imports no additional symbols.");
            Console.WriteLine("Optional --async-library <assembly-name> selects native Task/builder symbols from an explicit --reference. Native async emission is experimental.");
            Console.WriteLine("Optional --source-object-root selects this library's System.Object; requires --library and --core-reference. Use --object-library for an explicitly referenced root; native emission remains capability-checked.");
            Console.WriteLine("Optional --bootstrap-intrinsics authorizes checked storage from the explicitly selected --core-reference.");
            Console.WriteLine("Optional --bootstrap-ownership manifest.json selects source-library ownership and iteration contracts.");
            Console.WriteLine("Experimental PE/#Neo output. References are imported directly from supported native metadata.");
            Console.WriteLine("Legacy bridge: --system-symbols System.neox --system-method System.Math.Min/2 (repeat explicit selections).");
            Console.WriteLine("Static Int32 callable view only; not a complete core-library import. Run with the matching --system assembly.");
            Console.WriteLine("Native references require --core-reference; without references the legacy host primitive bootstrap remains available.");
            Console.WriteLine("Native projects: rvnc neoclr --project App.rvnproj [--run /path/to/neoclr]. No publish, PDB or managed execution support.");
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
            var sourceObjectRoot = false;
            string? systemPath = null;
            string? runtimeSeedPath = null;
            string? corePath = null;
            string? asyncAssembly = null;
            string? objectAssembly = null;
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
                    case "--source-object-root":
                        if (sourceObjectRoot) throw new ArgumentException("Specify --source-object-root once.");
                        sourceObjectRoot = true;
                        break;
                    case "--bootstrap-intrinsics":
                        if (bootstrapIntrinsics) throw new ArgumentException("Specify --bootstrap-intrinsics once.");
                        bootstrapIntrinsics = true;
                        break;
                    case "--runtime-seed":
                        if (runtimeSeedPath is not null || ++i == args.Length) throw new ArgumentException("Specify --runtime-seed once with an explicit native System path.");
                        runtimeSeedPath = Path.GetFullPath(args[i]);
                        break;
                    case "--object-library":
                        if (objectAssembly is not null || ++i == args.Length || string.IsNullOrWhiteSpace(args[i]) || args[i].StartsWith('-'))
                            throw new ArgumentException("Specify --object-library once with a registered native assembly name.");
                        objectAssembly = args[i];
                        break;
                    case "--async-library":
                        if (asyncAssembly is not null || ++i == args.Length || string.IsNullOrWhiteSpace(args[i]) || args[i].StartsWith('-'))
                            throw new ArgumentException("Specify --async-library once with a registered native assembly name.");
                        asyncAssembly = args[i];
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
            if (asyncAssembly is not null && corePath is null)
                throw new ArgumentException("--async-library requires an explicit --core-reference.");
            if (objectAssembly is not null && (corePath is null || sourceObjectRoot || systemPath is not null))
                throw new ArgumentException("--object-library requires --core-reference and cannot be combined with source or legacy Object ownership.");
            if (sourceObjectRoot && (corePath is null || !library || systemPath is not null))
                throw new ArgumentException("--source-object-root requires --library, --core-reference and no legacy --system-symbols selection.");
            if (bootstrapIntrinsics && corePath is null)
                throw new ArgumentException("--bootstrap-intrinsics requires an explicit --core-reference.");
            if (referencePaths.Count > 0 && corePath is null)
                throw new ArgumentException("Direct native references require --core-reference NeoCLR.CoreProbe.dll.");
            if (corePath is not null && string.Equals(corePath, output, StringComparison.OrdinalIgnoreCase))
                throw new ArgumentException("Output must differ from the primitive core input.");
            var name = Path.GetFileNameWithoutExtension(output);
            var catalog = corePath is null ? null : NeoClrReferenceCatalog.Read(corePath, referencePaths, runtimeSeedPath);
            catalog?.ValidateSourceOwnership(ownership?.Libraries.SelectMany(library => library.Types) ?? []);
            var host = typeof(object).Assembly.GetName();
            var core = catalog?.CoreIdentity ?? new AssemblyIdentity(host.Name!, host.Version!, host.CultureName ?? "", Convert.ToHexString(host.GetPublicKeyToken() ?? []));
            MetadataReference console = catalog?.Bootstrap.Reference ?? MetadataReference.CreateFromFile(typeof(Console).Assembly.Location);
            var references = catalog?.References.ToList() ?? new List<MetadataReference> {
                MetadataReference.CreateFromFile(typeof(object).Assembly.Location), console,
                MetadataReference.CreateFromFile(System.Reflection.Assembly.Load("System.Runtime").Location)
            };
            MetadataReference? bootstrapReference = bootstrapIntrinsics ? catalog!.Bootstrap.Reference : null;
            var dependencies = catalog?.Dependencies.ToList() ?? [];
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
            if (ownership is not null) compilationOptions = ownership.Apply(compilationOptions, name, core.Name);
            if (sourceObjectRoot)
            {
                var imports = compilationOptions.MetadataImportOptions;
                // A root bootstrap has no implicit introspection services yet. An ownership
                // manifest may supply a real contract; never require the default seed facade.
                compilationOptions = compilationOptions.WithRuntimeTypeOfContract(ownership?.TypeOf)
                    .WithMetadataImportOptions(new MetadataImportOptions(
                        core.Name, imports?.PrimitiveAssemblies, imports?.SourcePrimitiveTypes, useSourceObjectRoot: true));
            }
            if (objectAssembly is not null)
            {
                if (!references.OfType<NeoClrMetadataReference>().Any(reference => reference.Definition.Name == objectAssembly))
                    throw new InvalidDataException("Object library is not a registered native reference: " + objectAssembly);
                compilationOptions = compilationOptions.WithMetadataImportOptions(
                    (compilationOptions.MetadataImportOptions ?? new MetadataImportOptions(core.Name)).WithObjectAssemblyName(objectAssembly));
            }
            if (asyncAssembly is not null)
                compilationOptions = compilationOptions.WithMetadataImportOptions(
                    (compilationOptions.MetadataImportOptions ?? new MetadataImportOptions(core.Name)).WithAsyncAssemblyName(asyncAssembly));
            var compilation = Compilation.Create(name, trees, references.ToArray(), compilationOptions);
            ownership?.Validate(compilation);
            using var image = new MemoryStream();
            var backend = new NeoClrEmissionBackend(
                new(new(name, new Version(1, 0, 0, 0)), core, dependencies, systemSymbols is null ? console : null, systemSymbols, bootstrapReference,
                    ownership?.NativePrimitives?.Where(p => p.Value == name && p.Key != "System.Char").Select(p => Enum.Parse<PrimitiveType>(p.Key[7..])),
                    implementsGrapheme: ownership?.NativePrimitives?.GetValueOrDefault("System.Char") == name));
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

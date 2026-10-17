using System.Xml.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;
using Raven.CodeAnalysis.Syntax;

namespace NeoClrMetadataProbe;

internal static class NativeCoreProjectChecks
{
    internal static void Run(string core, string library, string directory)
    {
        directory = Path.GetFullPath(directory);
        Directory.CreateDirectory(directory);
        core = Path.GetFullPath(core);
        library = Path.GetFullPath(library);
        var projectFile = Path.Combine(directory, "App.rvnproj");
        File.WriteAllText(Path.Combine(directory, "Main.rvn"),
            "module Example.App\nfunc Main() -> int => Example.Input.Value() + 2\n");
        var definition = NeoCLR.Metadata.Experimental.Model.AssemblyDefinition.ReadNativeAssembly(File.ReadAllBytes(core));
        new XDocument(new XElement("Project", new XAttribute("Sdk", "Microsoft.NET.Sdk"),
            new XElement("PropertyGroup",
                new XElement("TargetFramework", "net10.0"), new XElement("OutputType", "Exe"),
                new XElement("RavenTargetPlatform", "NeoCLR"), new XElement("RavenMetadataFormat", "NeoCLR"),
                new XElement("RavenNeoClrNativeCoreReference", core),
                new XElement("RavenMetadataCoreAssemblyName", definition.Name)),
            new XElement("ItemGroup", new XElement("Reference", new XAttribute("Include", "Input"),
                new XElement("HintPath", library))))).Save(projectFile);
        var provider = new NeoClrProjectMetadataProvider();
        RavenWorkspace Workspace() => RavenWorkspace.Create(targetFramework: "net10.0",
            projectSystemService: new MsBuildProjectSystemService(RavenProjectConventions.Default, false, null, "net10.0",
                metadataProvider: provider));
        var workspace = Workspace();
        var id = workspace.OpenProject(projectFile);
        var project = workspace.CurrentSolution.GetProject(id)!;
        if (project.MetadataReferences.Count() != 2 || project.MetadataReferences.Any(r => r is not NeoClrMetadataReference))
            throw new Exception("Native-only project acquired a CLI reference.");
        if (!workspace.Services.ProjectSystemService!.GetMetadataInputPaths(projectFile).Contains(core))
            throw new Exception("Native core is not a watched input.");
        var config = provider.GetConfiguration(projectFile);
        if (!config.Catalog.UsesNativeMetadata || config.ObjectRootPath != core || !config.ReferencePaths.Contains(core))
            throw new Exception("Native execution core selection was lost.");
        var compilation = workspace.GetCompilation(id);
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (errors.Length != 0) throw new Exception(string.Join("\n", errors.Select(d => d.ToString())));
        using var image = new MemoryStream();
        var result = compilation.Emit(image, null, new EmitOptions().WithBackend(config.CreateEmissionBackend(project.AssemblyName!)));
        if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
        File.WriteAllBytes(Path.Combine(directory, "Consumer.dll"), image.ToArray());

        var completionSource = "func Main() -> int { System. }";
        var completionTree = SyntaxTree.ParseText(completionSource);
        var completionCompilation = Compilation.Create("Completion", [completionTree],
            project.MetadataReferences.ToArray(), compilation.Options);
        var names = new CompletionService().GetCompletions(completionCompilation, completionTree,
            completionSource.IndexOf("System.", StringComparison.Ordinal) + "System.".Length)
            .Select(item => item.DisplayText).ToArray();
        if (!names.Contains("Int32") || names.Any(name => name.StartsWith("Action", StringComparison.Ordinal) || name.StartsWith("Func<", StringComparison.Ordinal)))
            throw new Exception("Native namespace completion omitted Int32 or leaked CLI delegates: " + string.Join(", ", names));

        var original = File.ReadAllText(projectFile);
        var inferred = XDocument.Parse(original);
        inferred.Root!.Element("PropertyGroup")!.Element("RavenMetadataCoreAssemblyName")!.Remove();
        inferred.Save(projectFile);
        var inferredWorkspace = Workspace();
        var inferredId = inferredWorkspace.OpenProject(projectFile);
        var inferredCompilation = inferredWorkspace.GetCompilation(inferredId);
        if (inferredCompilation.Options.TargetCoreAssemblyName != definition.Name ||
            inferredCompilation.Options.MetadataImportOptions?.CoreAssemblyName != definition.Name ||
            inferredCompilation.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error))
            throw new Exception("Native artifact identity did not replace the implicit CLI preset.");
        using var inferredImage = new MemoryStream();
        var inferredConfig = provider.GetConfiguration(projectFile);
        if (!inferredCompilation.Emit(inferredImage, null, new EmitOptions().WithBackend(
                inferredConfig.CreateEmissionBackend("App"))).Success)
            throw new Exception("Implicit native core selection did not emit.");
        File.WriteAllText(projectFile, original);

        // Explicit native contracts must survive the loader rather than silently
        // falling back to CLI reflection when their declarations are unavailable.
        var mapped = XDocument.Parse(original);
        var mappedProperties = mapped.Root!.Element("PropertyGroup")!;
        mappedProperties.SetElementValue("RavenTypeOfAssemblyName", definition.Name);
        mappedProperties.SetElementValue("RavenTypeOfInfoType", "Example.MissingTypeInfo");
        mappedProperties.SetElementValue("RavenTypeOfContextType", "Example.MissingContext");
        mapped.Save(projectFile);
        var mappedWorkspace = Workspace();
        var mappedId = mappedWorkspace.OpenProject(projectFile);
        var mappedCompilation = mappedWorkspace.GetCompilation(mappedId);
        if (mappedCompilation.Options.RuntimeTypeOfContract != new RuntimeTypeOfContract(
                definition.Name, "Example.MissingTypeInfo", "Example.MissingContext"))
            throw new Exception("Explicit native typeof contract was discarded.");
        if (!mappedCompilation.GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error))
            throw new Exception("Missing native typeof declarations silently fell back.");
        foreach (var name in new[] { "RavenTypeOfAssemblyName", "RavenTypeOfInfoType", "RavenTypeOfContextType" })
            mappedProperties.SetElementValue(name, "");
        mapped.Save(projectFile);
        var clearedWorkspace = Workspace();
        var clearedId = clearedWorkspace.OpenProject(projectFile);
        if (clearedWorkspace.GetCompilation(clearedId).Options.RuntimeTypeOfContract is not null)
            throw new Exception("Explicitly cleared native typeof contract was restored.");
        File.WriteAllText(projectFile, original);
        // Restore the provider's successful snapshot used by the rejection checks.
        workspace = Workspace();
        workspace.OpenProject(projectFile);
        config = provider.GetConfiguration(projectFile);

        foreach (var (property, value) in new[] {
            ("RavenNeoClrCoreReference", core),
            ("RavenNeoClrBootstrapOwnership", "missing.json"),
            ("RavenNeoClrBootstrapIntrinsics", "true"),
            ("RavenNeoClrSourceObjectRoot", "true"),
            ("RavenNeoClrObjectLibrary", definition.Name),
            ("RavenNeoClrAsyncLibrary", definition.Name),
            ("RavenMetadataCoreAssemblyName", "WrongCore"),
            ("RavenNeoClrRuntimeSeed", core),
            ("RavenNeoClrRuntimeSeed", "missing.neox") })
        {
            var invalid = XDocument.Parse(original);
            invalid.Root!.Element("PropertyGroup")!.SetElementValue(property, value);
            invalid.Save(projectFile);
            var failed = Workspace();
            try { failed.OpenProject(projectFile); throw new Exception("Accepted invalid " + property); }
            catch (InvalidDataException) { }
            if (failed.CurrentSolution.Projects.Any() || !ReferenceEquals(config, provider.GetConfiguration(projectFile)))
                throw new Exception("Failed reload replaced the successful snapshot.");
        }
        File.WriteAllText(projectFile, original);
        Console.WriteLine("PASS native-only references, compilation/emission, System completion, explicit typeof retention/clearing/rejection, execution paths and nine rejected configurations");
    }
}

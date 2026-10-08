using System.Xml.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;

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

        var original = File.ReadAllText(projectFile);
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
        Console.WriteLine("PASS native-only project references, module binding, watched core, emission, execution paths and nine rejected configurations");
    }
}

using System.Xml.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;

namespace NeoClrMetadataProbe;

internal static class NativeProjectChecks
{
    internal static void Run(string core, string seed, string directory)
    {
        directory = Path.GetFullPath(directory);
        var dependencies = Path.Combine(directory, "dependencies");
        ReferenceCatalogChecks.Run(core, seed, dependencies);
        var root = Path.Combine(directory, "project"); Directory.CreateDirectory(root);
        var source = Path.Combine(root, "Main.rvn"); File.WriteAllText(source, "func Main() -> int => Library.Api.Updated()\n");
        var projectFile = Path.Combine(root, "App.rvnproj");
        new XDocument(new XElement("Project", new XAttribute("Sdk", "Microsoft.NET.Sdk"),
            new XElement("PropertyGroup",
                new XElement("TargetFramework", "net10.0"), new XElement("OutputType", "Exe"),
                new XElement("RavenTargetPlatform", "NeoCLR"), new XElement("RavenMetadataFormat", "NeoCLR"),
                new XElement("RavenNeoClrCoreReference", Path.GetFullPath(core)),
                new XElement("RavenNeoClrRuntimeSeed", Path.GetFullPath(seed)),
                new XElement("RavenTypeOfAssemblyName", ""), new XElement("RavenTypeOfInfoType", ""), new XElement("RavenTypeOfContextType", "")),
            new XElement("ItemGroup", new XElement("Reference", new XAttribute("Include", "CatalogLibrary"),
                new XElement("HintPath", "../dependencies/Library.dll"))))).Save(projectFile);
        RavenWorkspace Workspace(bool native) => RavenWorkspace.Create(targetFramework: "net10.0",
            projectSystemService: new MsBuildProjectSystemService(RavenProjectConventions.Default, false, null, "net10.0",
                metadataProvider: native ? new NeoClrProjectMetadataProvider() : null));
        var workspace = Workspace(true);
        var id = workspace.OpenProject(projectFile);
        var project = workspace.CurrentSolution.GetProject(id)!;
        if (project.MetadataReferences.Count() != 2 || project.MetadataReferences.OfType<NeoClrMetadataReference>().Count() != 1)
            throw new Exception("native project silently acquired CLI references");
        var inputs = workspace.Services.ProjectSystemService!.GetMetadataInputPaths(projectFile);
        if (!inputs.Contains(Path.GetFullPath(core)) || !inputs.Contains(Path.GetFullPath(seed)) ||
            !inputs.Contains(Path.Combine(dependencies, "Library.dll"))) throw new Exception("missing watched artifact input");
        var compilation = workspace.GetCompilation(id);
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (errors.Length != 0) throw new Exception(string.Join("\n", errors.Select(d => d.ToString())));
        if (project.Documents.Count() != 1) throw new Exception("native project received host generated sources");
        Reject<NotSupportedException>(() => Workspace(false).OpenProject(projectFile));
        var document = XDocument.Load(projectFile);
        document.Descendants("HintPath").Single().Value = "../dependencies/Missing.dll";
        document.Save(projectFile);
        var failed = Workspace(true);
        Reject<IOException>(() => failed.OpenProject(projectFile));
        if (failed.CurrentSolution.Projects.Any()) throw new Exception("failed project was partially published");
        document.Descendants("HintPath").Single().Value = "../dependencies/Library.dll";
        document.Save(projectFile);
        var original = File.ReadAllText(projectFile);
        void RejectProperty(string name, string value)
        {
            var invalid = XDocument.Parse(original);
            invalid.Root!.Element("PropertyGroup")!.SetElementValue(name, value);
            invalid.Save(projectFile);
            Reject<InvalidDataException>(() => Workspace(true).OpenProject(projectFile));
            File.WriteAllText(projectFile, original);
        }
        RejectProperty("RavenNeoClrBootstrapIntrinsics", "invalid");
        RejectProperty("RavenNeoClrObjectLibrary", "Missing");
        RejectProperty("RavenTargetPlatform", "DotNet");
        RejectProperty("RavenTargetCoreAssemblyName", "WrongCore");
        RejectProperty("RavenMetadataCoreAssemblyName", "WrongCore");
        var duplicateReference = XDocument.Parse(original);
        duplicateReference.Root!.Element("ItemGroup")!.Add(new XElement(duplicateReference.Descendants("Reference").Single()));
        duplicateReference.Save(projectFile);
        Reject<InvalidDataException>(() => Workspace(true).OpenProject(projectFile));
        File.WriteAllText(projectFile, original);
        // The consumer imports a prebuilt artifact without loading dependency sources.
        var libraryProject = Path.Combine(dependencies, "Library.rvnproj");
        var libraryDocument = XDocument.Parse(original);
        libraryDocument.Root!.Element("ItemGroup")!.Remove();
        libraryDocument.Root.Element("PropertyGroup")!.SetElementValue("OutputType", "Library");
        libraryDocument.Root.Element("PropertyGroup")!.SetElementValue("AssemblyName", "CatalogLibrary");
        libraryDocument.Save(libraryProject);
        var artifact = new NeoClrProjectMetadataProvider().GetOutputPath(libraryProject, "CatalogLibrary");
        Directory.CreateDirectory(Path.GetDirectoryName(artifact)!);
        File.Copy(Path.Combine(dependencies, "Library.dll"), artifact, true);
        var graphProject = XDocument.Parse(original);
        graphProject.Descendants("Reference").Remove();
        graphProject.Root!.Element("ItemGroup")!.Add(new XElement("ProjectReference", new XAttribute("Include", libraryProject)));
        graphProject.Save(projectFile);
        var graphWorkspace = Workspace(true);
        var graphId = graphWorkspace.OpenProject(projectFile);
        if (graphWorkspace.CurrentSolution.Projects.Count() != 1 ||
            graphWorkspace.GetCompilation(graphId).GetDiagnostics().Any(d => d.Severity == DiagnosticSeverity.Error) ||
            !graphWorkspace.Services.ProjectSystemService!.GetMetadataInputPaths(projectFile).Contains(artifact))
            throw new Exception("native project reference did not import the prebuilt artifact");
        File.Move(artifact, artifact + ".saved");
        Reject<IOException>(() => Workspace(true).OpenProject(projectFile));
        File.Move(artifact + ".saved", artifact);
        File.Move(libraryProject, libraryProject + ".saved");
        Reject<IOException>(() => Workspace(true).OpenProject(projectFile));
        File.Move(libraryProject + ".saved", libraryProject);
        libraryDocument.Root.Add(new XElement("ItemGroup", new XElement("ProjectReference", new XAttribute("Include", projectFile))));
        libraryDocument.Save(libraryProject);
        // Make the root a library so cycle validation, rather than executable-reference validation, fires.
        graphProject.Root.Element("PropertyGroup")!.SetElementValue("OutputType", "Library");
        graphProject.Save(projectFile);
        Reject<InvalidOperationException>(() => Workspace(true).OpenProject(projectFile));
        libraryDocument.Root.Element("ItemGroup")!.Remove();
        libraryDocument.Root.Element("PropertyGroup")!.SetElementValue("RavenMetadataFormat", "CLI");
        libraryDocument.Save(libraryProject);
        Reject<NotSupportedException>(() => Workspace(true).OpenProject(projectFile));
        libraryDocument.Root.Element("PropertyGroup")!.SetElementValue("RavenMetadataFormat", "NeoCLR");
        libraryDocument.Root.Element("PropertyGroup")!.SetElementValue("AssemblyName", "WrongIdentity");
        libraryDocument.Save(libraryProject);
        var wrongArtifact = new NeoClrProjectMetadataProvider().GetOutputPath(libraryProject, "WrongIdentity");
        File.Copy(artifact, wrongArtifact, true);
        Reject<InvalidDataException>(() => Workspace(true).OpenProject(projectFile));
        File.WriteAllText(projectFile, original);
        var coreIdentity = NeoCLR.Metadata.Experimental.Model.AssemblyDefinition.ReadAssembly(File.ReadAllBytes(core), false).Identity;
        var graph = new NeoCLR.Metadata.Experimental.Model.AssemblyBuilder(new("ProjectRoot", new(1, 0, 0, 0)), coreIdentity);
        graph.AddNativeObjectRoot();
        var rootArtifact = Path.Combine(dependencies, "ProjectRoot.dll");
        File.WriteAllBytes(rootArtifact, NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer.WriteLibraryBinary(graph));
        var rootProject = XDocument.Parse(original);
        rootProject.Root!.Element("PropertyGroup")!.Add(new XElement("RavenNeoClrObjectLibrary", "ProjectRoot"));
        rootProject.Root.Element("ItemGroup")!.Add(new XElement("Reference", new XAttribute("Include", "ProjectRoot"),
            new XElement("HintPath", "../dependencies/ProjectRoot.dll")));
        rootProject.Save(projectFile);
        var provider = new NeoClrProjectMetadataProvider();
        var rootWorkspace = RavenWorkspace.Create(targetFramework: "net10.0",
            projectSystemService: new MsBuildProjectSystemService(RavenProjectConventions.Default, false, null, "net10.0", metadataProvider: provider));
        var rootId = rootWorkspace.OpenProject(projectFile);
        if (rootWorkspace.GetCompilation(rootId).GetSpecialType(SpecialType.System_Object).ContainingAssembly.Name != "ProjectRoot" ||
            provider.GetConfiguration(projectFile).ObjectRootPath != rootArtifact ||
            !rootWorkspace.Services.ProjectSystemService!.GetMetadataInputPaths(projectFile).Contains(rootArtifact))
            throw new Exception("project semantic/runtime Object owner or watched artifact differs");
        var conflictingGraph = new NeoCLR.Metadata.Experimental.Model.AssemblyBuilder(new("ProjectRoot", new(2, 0, 0, 0)), coreIdentity);
        conflictingGraph.AddNativeObjectRoot();
        File.WriteAllBytes(Path.Combine(dependencies, "ConflictingRoot.dll"),
            NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer.WriteLibraryBinary(conflictingGraph));
        rootProject.Root.Element("ItemGroup")!.Add(new XElement("Reference", new XAttribute("Include", "ConflictingRoot"),
            new XElement("HintPath", "../dependencies/ConflictingRoot.dll")));
        rootProject.Save(projectFile);
        Reject<InvalidDataException>(() => Workspace(true).OpenProject(projectFile));
        Console.WriteLine("PASS evaluated native project references, semantic binding, missing-adapter rejection and transactional failure");
        Console.WriteLine(projectFile);
    }
    private static void Reject<T>(Action action) where T : Exception
    {
        try { action(); } catch (T) { return; }
        throw new Exception("expected " + typeof(T).Name);
    }
}

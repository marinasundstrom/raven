using System.Xml.Linq;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;

namespace NeoClrMetadataProbe;

internal static class NativeBundleProjectChecks
{
    internal static void Run(string bundle, string directory, string source)
    {
        directory = Path.GetFullPath(directory);
        if (Directory.Exists(directory)) throw new IOException("Output directory must be fresh.");
        var relocated = Path.Combine(directory, "SDK with spaces");
        Directory.CreateDirectory(relocated);
        foreach (var file in Directory.EnumerateFiles(bundle, "*", SearchOption.AllDirectories))
        {
            var destination = Path.Combine(relocated, Path.GetRelativePath(bundle, file));
            Directory.CreateDirectory(Path.GetDirectoryName(destination)!);
            File.Copy(file, destination);
        }
        var configuration = Path.Combine(relocated, "NeoCLR.ClassLibrary.props");
        var projectFile = Path.Combine(directory, "Headers.rvnproj");
        File.Copy(source, Path.Combine(directory, "Main.rvn"));
        new XDocument(new XElement("Project", new XAttribute("Sdk", "Microsoft.NET.Sdk"),
            new XElement("Import", new XAttribute("Project", "SDK with spaces/NeoCLR.ClassLibrary.props")),
            new XElement("PropertyGroup", new XElement("TargetFramework", "net10.0"),
                new XElement("OutputType", "Exe"), new XElement("AssemblyName", "Headers")))).Save(projectFile);
        RavenWorkspace Workspace() => RavenWorkspace.Create(projectSystemService: new MsBuildProjectSystemService(
            RavenProjectConventions.Default, false, null, "net10.0", metadataProvider: new NeoClrProjectMetadataProvider()));
        var workspace = Workspace();
        var id = workspace.OpenProject(projectFile);
        var project = workspace.CurrentSolution.GetProject(id)!;
        if (project.MetadataReferences.Count() != 5 || project.MetadataReferences.OfType<NeoClrMetadataReference>().Count() != 4 || project.Documents.Count() != 1)
            throw new Exception("Bundle injected fallback references or dependency sources.");
        var compilation = workspace.GetCompilation(id);
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (errors.Length != 0) throw new Exception(string.Join("\n", errors.Select(d => d.ToString())));
        foreach (var (name, owner) in new[] { ("System.Object", "System.Runtime"), ("System.Data.Json.JsonValue", "System.Data"),
                     ("System.Networking.IPAddress", "System.Networking"), ("System.Web.Http.HttpClient", "System.Web") })
            if (compilation.GetTypeByMetadataName(name)?.ContainingAssembly.Name != owner)
                throw new Exception("Incorrect native symbol owner: " + name);
        if (File.Exists(Path.Combine(relocated, "System.Networking.xml")))
        {
            var address = compilation.GetTypeByMetadataName("System.Networking.IPAddress")!;
            if (address.GetDocumentationComment()?.Content.Contains("An immutable IP address") != true)
                throw new Exception("Relocated native documentation was not loaded.");
            Console.WriteLine("PASS relocated native documentation");
        }
        var inputs = workspace.Services.ProjectSystemService!.GetMetadataInputPaths(projectFile);
        foreach (var path in new[] { configuration, Path.Combine(relocated, "System.runtime.neox"), Path.Combine(relocated, "System.Web.dll") })
            if (!inputs.Contains(path)) throw new Exception("Unwatched bundle input: " + path);
        void Reject()
        {
            var failed = Workspace();
            try { failed.OpenProject(projectFile); }
            catch (Exception error) when (error is IOException or InvalidDataException)
            {
                if (failed.CurrentSolution.Projects.Any()) throw new Exception("Invalid bundle partially published a workspace.");
                return;
            }
            throw new Exception("Invalid bundle was accepted.");
        }
        var original = File.ReadAllText(configuration);
        var changed = XDocument.Parse(original);
        changed.Descendants("RavenNeoClrObjectLibrary").Single().Value = "Missing.Owner";
        changed.Save(configuration);
        Reject();
        File.WriteAllText(configuration, original);
        var web = Path.Combine(relocated, "System.Web.dll");
        File.Move(web, web + ".saved");
        Reject();
        File.Move(web + ".saved", web);
        Console.WriteLine("PASS relocated native bundle symbols, configuration watching and transactional rejection");
        Console.WriteLine(projectFile);
    }
}

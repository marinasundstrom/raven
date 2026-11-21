using System.Xml.Linq;

using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.NeoClr;

namespace NeoClrMetadataProbe;

internal static class ExtendedProjectChecks
{
    internal static void Run(string core, string directory)
    {
        directory = Path.GetFullPath(directory);
        Directory.CreateDirectory(directory);
        var projectPath = Path.Combine(directory, "App.rvnproj");
        var source = Path.Combine(directory, "Main.rvn");
        File.WriteAllText(source, "public func Answer() -> int => 42\n");
        var document = new XDocument(new XElement("Project", new XAttribute("Sdk", "Microsoft.NET.Sdk"),
            new XElement("PropertyGroup",
                new XElement("TargetFramework", "net10.0"), new XElement("OutputType", "Library"),
                new XElement("RavenTargetPlatform", "NeoCLR"), new XElement("RavenMetadataFormat", "NeoCLR"),
                new XElement("RavenNeoClrCoreReference", Path.GetFullPath(core)),
                new XElement("RavenTypeOfAssemblyName", ""), new XElement("RavenTypeOfInfoType", ""), new XElement("RavenTypeOfContextType", ""))));
        var properties = document.Root!.Element("PropertyGroup")!;
        foreach (var format in new string?[] { null, "Native", "ExtendedCli", "extendedcli" })
        {
            properties.SetElementValue("RavenNeoClrOutputFormat", format);
            document.Save(projectPath);
            var provider = new NeoClrProjectMetadataProvider();
            var workspace = RavenWorkspace.Create(projectSystemService: new MsBuildProjectSystemService(
                RavenProjectConventions.Default, false, null, "net10.0", metadataProvider: provider));
            var id = workspace.OpenProject(projectPath);
            var config = provider.GetConfiguration(projectPath);
            var extended = format?.Equals("ExtendedCli", StringComparison.OrdinalIgnoreCase) == true;
            if (config.OutputFormat != (extended ? NeoClrOutputFormat.ExtendedCli : NeoClrOutputFormat.Native))
                throw new Exception("project format selection lost");
            using var stream = new MemoryStream();
            var result = workspace.GetCompilation(id).Emit(stream, null, new EmitOptions().WithBackend(config.CreateEmissionBackend("App")));
            if (!result.Success) throw new Exception(string.Join("\n", result.Diagnostics));
            var image = stream.ToArray();
            var definition = extended ? AssemblyDefinition.ReadExtendedCliAssembly(image) : AssemblyDefinition.ReadNativeAssembly(image);
            if (definition.MainModule.Functions.Single().Name != "Answer") throw new Exception("project output lost function");
            var artifact = Path.Combine(directory, extended ? "Extended.dll" : "Native.dll");
            File.WriteAllBytes(artifact, image);
            provider.ValidateProjectArtifact(artifact, "App");
        }
        foreach (var invalid in new[] { "0", "1", "Other", "Native, ExtendedCli" })
        {
            properties.SetElementValue("RavenNeoClrOutputFormat", invalid);
            document.Save(projectPath);
            var workspace = RavenWorkspace.Create(projectSystemService: new MsBuildProjectSystemService(
                RavenProjectConventions.Default, false, null, "net10.0", metadataProvider: new NeoClrProjectMetadataProvider()));
            try { workspace.OpenProject(projectPath); throw new Exception("invalid format accepted"); }
            catch (InvalidDataException ex) when (ex.Message.Contains("RavenNeoClrOutputFormat")) { }
            if (workspace.CurrentSolution.Projects.Any()) throw new Exception("failed project published");
        }
        properties.SetElementValue("RavenNeoClrOutputFormat", "ExtendedCli");
        document.Save(projectPath);
        File.WriteAllText(source, "public func Wide(value: long) -> long => value\n");
        var rejectingProvider = new NeoClrProjectMetadataProvider();
        var rejectingWorkspace = RavenWorkspace.Create(projectSystemService: new MsBuildProjectSystemService(
            RavenProjectConventions.Default, false, null, "net10.0", metadataProvider: rejectingProvider));
        var rejectingId = rejectingWorkspace.OpenProject(projectPath);
        using var rejectedOutput = new MemoryStream();
        var rejected = rejectingWorkspace.GetCompilation(rejectingId).Emit(rejectedOutput, null,
            new EmitOptions().WithBackend(rejectingProvider.GetConfiguration(projectPath).CreateEmissionBackend("App")));
        if (rejected.Success || rejectedOutput.Length != 0 || !rejected.Diagnostics.Any(d => d.Id == "NEOMETA003"))
            throw new Exception("unsupported project persistence did not reject atomically");
        Console.WriteLine("PASS project persistence selection, native default, artifact validation, invalid settings and atomic rejection");
    }
}

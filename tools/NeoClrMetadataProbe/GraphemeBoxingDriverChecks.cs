using System.Diagnostics;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

namespace NeoClrMetadataProbe;

internal static class GraphemeBoxingDriverChecks
{
    internal static async Task Run(string driver, string root, string output)
    {
        driver = Path.GetFullPath(driver);
        root = Path.GetFullPath(root);
        output = Path.GetFullPath(output);
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        var corePath = Path.Combine(output, "NeoCLR.CoreProbe.dll");
        using (var bootstrap = Mono.Cecil.AssemblyDefinition.ReadAssembly(Path.Combine(root, "api-docs/reference/NeoCLR.CoreProbe.dll")))
        {
            bootstrap.MainModule.GetType("System.Char").Name = "Character";
            bootstrap.Write(corePath);
        }
        foreach (var selectRoot in new[] { false, true })
        {
            var path = Path.Combine(output, selectRoot ? "RavenImportedGraphemeBoxes.dll" : "MissingObject.dll");
            var start = new ProcessStartInfo("dotnet") { WorkingDirectory = root, RedirectStandardOutput = true, RedirectStandardError = true };
            foreach (var argument in new[] { driver, "neoclr", "--extended-cli", "--library", "--core-reference", corePath, "--bootstrap-intrinsics", "--bootstrap-ownership", "samples/metadata-forwarding/imported-values/boxing-ownership.json", "--reference", "tests/fixtures/cli-imported-values/BoxingCore.dll", "-o", path, "samples/metadata-forwarding/imported-values/CharacterBoxes.rvn" })
                start.ArgumentList.Add(argument);
            if (selectRoot)
            {
                start.ArgumentList.Add("--object-library");
                start.ArgumentList.Add("BoxingCore");
            }
            using var process = Process.Start(start)!;
            var stdout = process.StandardOutput.ReadToEndAsync();
            var stderr = process.StandardError.ReadToEndAsync();
            await process.WaitForExitAsync();
            var diagnostics = await stdout + await stderr;
            if (!selectRoot)
            {
                if (process.ExitCode == 0 || File.Exists(path) || !diagnostics.Contains("native type requires a supported symbol-only emission contract: System.Character"))
                    throw new Exception("missing selected Object owner must fail without publication: " + diagnostics);
                continue;
            }
            if (process.ExitCode != 0) throw new Exception("selected Object boxing failed: " + diagnostics);
            using var pe = new PEReader(File.OpenRead(path));
            var metadata = pe.GetMetadataReader();
            var roots = metadata.TypeReferences.Select(metadata.GetTypeReference).Where(t => metadata.GetString(t.Name) == "Object").ToArray();
            if (roots.Length != 1 || metadata.GetString(metadata.GetAssemblyReference((AssemblyReferenceHandle)roots[0].ResolutionScope).Name) != "BoxingCore")
                throw new Exception("Object storage must retain exactly the selected provider");
        }
        Console.WriteLine("PASS selected native Object/ValueType Character boxing and missing-owner rejection");
    }
}

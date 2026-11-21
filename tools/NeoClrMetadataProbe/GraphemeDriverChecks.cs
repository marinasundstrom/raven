using System.Diagnostics;

using NeoCLR.Metadata.Experimental.Model;

namespace NeoClrMetadataProbe;

internal static class GraphemeDriverChecks
{
    internal static async Task Run(string driver, string root, string output)
    {
        driver = Path.GetFullPath(driver);
        root = Path.GetFullPath(root);
        output = Path.GetFullPath(output);
        if (Directory.Exists(output)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(output);
        // Match the current Character identity in an isolated legacy CLI bootstrap.
        // This does not modify the retained documentation reference assembly.
        var corePath = Path.Combine(output, "NeoCLR.CoreProbe.dll");
        using (var bootstrap = Mono.Cecil.AssemblyDefinition.ReadAssembly(Path.Combine(root, "api-docs/reference/NeoCLR.CoreProbe.dll")))
        {
            bootstrap.MainModule.GetType("System.Char").Name = "Character";
            bootstrap.Write(corePath);
        }
        var path = Path.Combine(output, "RavenGraphemeStorage.dll");
        var start = new ProcessStartInfo("dotnet") { WorkingDirectory = root, RedirectStandardOutput = true, RedirectStandardError = true };
        foreach (var argument in new[] { driver, "neoclr", "--extended-cli", "--library", "--core-reference", corePath, "--bootstrap-intrinsics", "--bootstrap-ownership", "samples/metadata-forwarding/grapheme-storage/ownership.json", "-o", path, "samples/metadata-forwarding/grapheme-storage/Character.rvn" })
            start.ArgumentList.Add(argument);
        using var process = Process.Start(start)!;
        var stdout = process.StandardOutput.ReadToEndAsync();
        var stderr = process.StandardError.ReadToEndAsync();
        await process.WaitForExitAsync();
        if (process.ExitCode != 0) throw new Exception($"grapheme driver failed: {await stdout}{await stderr}");
        var assembly = AssemblyDefinition.ReadExtendedCliAssembly(File.ReadAllBytes(path));
        var character = assembly.MainModule.Types.Single(t => t.Name == "Character");
        if (!character.NativeGrapheme || character.NativePrimitive is not null || !character.IsValueType || character.Fields.Count != 0)
            throw new Exception("driver lost native grapheme storage");
        if (!character.Methods.Select(m => m.Name).Order().SequenceEqual(new[] { "Read" }))
            throw new Exception("driver lost grapheme methods");
        var consumerPath = Path.Combine(output, "RavenImportedGrapheme.dll");
        var consumerStart = new ProcessStartInfo("dotnet") { WorkingDirectory = root, RedirectStandardOutput = true, RedirectStandardError = true };
        foreach (var argument in new[] { driver, "neoclr", "--extended-cli", "--library", "--core-reference", corePath, "--bootstrap-intrinsics", "--bootstrap-ownership", "samples/metadata-forwarding/grapheme-storage/ownership.json", "--reference", path, "-o", consumerPath, "samples/metadata-forwarding/imported-values/Character.rvn" })
            consumerStart.ArgumentList.Add(argument);
        using var consumerProcess = Process.Start(consumerStart)!;
        var consumerStdout = consumerProcess.StandardOutput.ReadToEndAsync();
        var consumerStderr = consumerProcess.StandardError.ReadToEndAsync();
        await consumerProcess.WaitForExitAsync();
        if (consumerProcess.ExitCode != 0) throw new Exception($"imported grapheme driver failed: {await consumerStdout}{await consumerStderr}");
        var consumer = AssemblyDefinition.ReadExtendedCliAssembly(File.ReadAllBytes(consumerPath));
        var copy = consumer.MainModule.Methods.Single(m => m.Name == "ImportedCopy");
        if (!copy.TryGetSignature(out var signature) || signature!.ReturnType.ReferencedType?.Name != "Character" || consumer.MainModule.Types.Count != 0)
            throw new Exception("imported grapheme signature lost its provider reference");
        Console.WriteLine("PASS canonical System.Character standalone and imported ownership and extended metadata");
    }
}

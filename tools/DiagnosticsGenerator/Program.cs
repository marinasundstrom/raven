using System.Reflection;
using System.Security.Cryptography;
using System.Xml.Linq;
using Raven.Generators;

var diagnosticsPath = Path.GetFullPath("DiagnosticDescriptors.xml");
var outputPath = Path.GetFullPath("CompilerDiagnostics.g.cs");
var stampPath = Path.GetFullPath(".diagnostics.stamp");

var diagnostics = LoadDiagnosticDescriptorsFromXml(diagnosticsPath);
var hash = await GetHashAsync(diagnosticsPath);

var force = args.Contains("-f");

if (File.Exists(stampPath) && File.ReadAllText(stampPath).Trim() == hash && !force)
{
    Console.WriteLine("Diagnostics unchanged. Skipping generation.");
    return;
}

if (File.Exists(outputPath))
{
    File.Delete(outputPath);
}

var diagnosticsSource = DiagnosticDescriptorGenerator.GenerateCompilerDiagnostics(diagnostics);
await File.WriteAllTextAsync(outputPath, diagnosticsSource);

File.WriteAllText(stampPath, hash);

Console.WriteLine($"Generated {diagnostics.Count} diagnostics.");

static List<DiagnosticDescriptorModel> LoadDiagnosticDescriptorsFromXml(string path)
{
    var doc = XDocument.Load(path);
    var result = new List<DiagnosticDescriptorModel>();

    foreach (var descriptor in doc.Descendants("Descriptor"))
    {
        var model = new DiagnosticDescriptorModel(
            Id: descriptor.Attribute("Id")!.Value,
            Identifier: descriptor.Attribute("Identifier")!.Value,
            Title: descriptor.Attribute("Title")?.Value ?? string.Empty,
            Message: descriptor.Attribute("Message")?.Value ?? string.Empty,
            Category: descriptor.Attribute("Category")?.Value ?? string.Empty,
            Severity: descriptor.Attribute("Severity")?.Value ?? "Error",
            EnabledByDefault: bool.Parse(descriptor.Attribute("EnabledByDefault")?.Value ?? "true"),
            Description: descriptor.Attribute("Description")?.Value ?? string.Empty,
            HelpLinkUri: descriptor.Attribute("HelpLinkUri")?.Value ?? string.Empty);

        result.Add(model);
    }

    return result;
}

static async Task<string> GetHashAsync(string diagnosticsPath)
{
    var diagnosticsBytes = await File.ReadAllBytesAsync(diagnosticsPath);
    var assemblyBytes = File.ReadAllBytes(Assembly.GetExecutingAssembly().Location);

    var combined = diagnosticsBytes
        .Concat(assemblyBytes)
        .ToArray();

    return Convert.ToHexString(SHA256.HashData(combined));
}

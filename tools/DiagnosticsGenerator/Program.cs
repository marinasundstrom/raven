using System.Reflection;
using System.Security.Cryptography;
using System.Text.RegularExpressions;
using System.Xml.Linq;
using Raven.Generators;

var diagnosticsPath = Path.GetFullPath("DiagnosticDescriptors.xml");
var outputPath = Path.GetFullPath("CompilerDiagnostics.g.cs");
var extensionsOutputPath = Path.GetFullPath("DiagnosticBagExtensions.g.cs");
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
if (File.Exists(extensionsOutputPath))
{
    File.Delete(extensionsOutputPath);
}

var diagnosticsSource = DiagnosticDescriptorGenerator.GenerateCompilerDiagnostics(diagnostics);
var extensionsSource = DiagnosticDescriptorGenerator.GenerateDiagnosticBagExtensions(diagnostics);
await File.WriteAllTextAsync(outputPath, diagnosticsSource);
await File.WriteAllTextAsync(extensionsOutputPath, extensionsSource);

File.WriteAllText(stampPath, hash);

Console.WriteLine($"Generated {diagnostics.Count} diagnostics.");

static List<DiagnosticDescriptorModel> LoadDiagnosticDescriptorsFromXml(string path)
{
    var doc = XDocument.Load(path);
    var result = new List<DiagnosticDescriptorModel>();

    foreach (var descriptor in doc.Descendants("Descriptor"))
    {
        var title = descriptor.Attribute("Title")?.Value ?? string.Empty;
        var message = descriptor.Attribute("Message")?.Value ?? string.Empty;
        var args = new List<string>();

        string ConvertPlaceholders(string text)
        {
            return Regex.Replace(text, "\\{([^}]+)\\}", m =>
            {
                var name = m.Groups[1].Value;
                var index = args.IndexOf(name);
                if (index < 0)
                {
                    args.Add(name);
                    index = args.Count - 1;
                }
                return "{" + index + "}";
            });
        }

        title = ConvertPlaceholders(title);
        message = ConvertPlaceholders(message);

        var model = new DiagnosticDescriptorModel(
            Id: descriptor.Attribute("Id")!.Value,
            Identifier: descriptor.Attribute("Identifier")!.Value,
            Title: title,
            Message: message,
            Category: descriptor.Attribute("Category")?.Value ?? string.Empty,
            Severity: descriptor.Attribute("Severity")?.Value ?? "Error",
            EnabledByDefault: bool.Parse(descriptor.Attribute("EnabledByDefault")?.Value ?? "true"),
            Description: descriptor.Attribute("Description")?.Value ?? string.Empty,
            HelpLinkUri: descriptor.Attribute("HelpLinkUri")?.Value ?? string.Empty,
            Arguments: args);

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

using System.Diagnostics;
using System.Security.Cryptography;
using System.Text.RegularExpressions;
using System.Threading;
using System.Xml.Linq;

using Raven.Generators;

var diagnosticsPath = Path.GetFullPath("DiagnosticDescriptors.xml");
var outputPath = Path.GetFullPath("CompilerDiagnostics.g.cs");
var extensionsOutputPath = Path.GetFullPath("DiagnosticBagExtensions.g.cs");
var stampPath = Path.GetFullPath(".diagnostics.stamp");
var repoRoot = Path.GetFullPath(Path.Combine("..", ".."));
var lockPath = Path.GetFullPath(Path.Combine("obj", "DiagnosticsGenerator.lock"));

using var generationLock = AcquireGenerationLock(lockPath);

var diagnostics = LoadDiagnosticDescriptorsFromXml(diagnosticsPath);
var hash = await GetHashAsync(diagnosticsPath, EnumerateGeneratorInputs(repoRoot));

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

static FileStream AcquireGenerationLock(string lockPath)
{
    const int timeoutMilliseconds = 120_000;
    const int retryDelayMilliseconds = 50;

    Directory.CreateDirectory(Path.GetDirectoryName(lockPath)!);
    var stopwatch = Stopwatch.StartNew();

    while (true)
    {
        try
        {
            return new FileStream(
                lockPath,
                FileMode.OpenOrCreate,
                FileAccess.ReadWrite,
                FileShare.None);
        }
        catch (IOException exception)
        {
            if (stopwatch.ElapsedMilliseconds >= timeoutMilliseconds)
            {
                throw new TimeoutException(
                    $"Timed out waiting for the diagnostics generator lock '{lockPath}'.",
                    exception);
            }

            Thread.Sleep(retryDelayMilliseconds);
        }
    }
}

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

static async Task<string> GetHashAsync(
    string diagnosticsPath,
    IEnumerable<string> generatorInputs)
{
    using var hash = IncrementalHash.CreateHash(HashAlgorithmName.SHA256);
    hash.AppendData(await File.ReadAllBytesAsync(diagnosticsPath));

    foreach (var path in generatorInputs.OrderBy(static path => path, StringComparer.Ordinal))
        hash.AppendData(await File.ReadAllBytesAsync(path));

    return Convert.ToHexString(hash.GetHashAndReset());
}

static IEnumerable<string> EnumerateGeneratorInputs(string repoRoot)
{
    var directory = Path.Combine(repoRoot, "tools", "DiagnosticsGenerator");
    foreach (var file in Directory.EnumerateFiles(directory, "*.cs", SearchOption.AllDirectories))
    {
        if (!IsBuildOutput(file))
            yield return file;
    }

    yield return Path.Combine(directory, "DiagnosticsGenerator.csproj");
}

static bool IsBuildOutput(string path)
    => path.Contains(
        $"{Path.DirectorySeparatorChar}bin{Path.DirectorySeparatorChar}",
        StringComparison.OrdinalIgnoreCase)
    || path.Contains(
        $"{Path.DirectorySeparatorChar}obj{Path.DirectorySeparatorChar}",
        StringComparison.OrdinalIgnoreCase);

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Security.Cryptography;

using BoundNodeGenerator;

var repoRoot = FindRepositoryRoot();
var force = args.Any(static a => a.Equals("-f", StringComparison.OrdinalIgnoreCase) || a.Equals("--force", StringComparison.OrdinalIgnoreCase));

var boundTreeDir = Path.Combine(repoRoot, "src", "Raven.CodeAnalysis", "BoundTree");
var symbolsDir = Path.Combine(repoRoot, "src", "Raven.CodeAnalysis", "Symbols");
var boundOutputDir = Path.Combine(boundTreeDir, "generated");
var symbolOutputDir = Path.Combine(symbolsDir, "generated");
var stampPath = Path.Combine(boundOutputDir, ".stamp");

var inputs = EnumerateInputs(boundTreeDir, symbolsDir).ToList();
var hash = ComputeHash(inputs);

if (!force && File.Exists(stampPath) && File.ReadAllText(stampPath).Trim().Equals(hash, StringComparison.Ordinal))
{
    Console.WriteLine("Bound node specification unchanged. Skipping generation.");
    return;
}

Directory.CreateDirectory(boundOutputDir);
Directory.CreateDirectory(symbolOutputDir);

DeleteGeneratedFiles(boundOutputDir);
DeleteGeneratedFiles(symbolOutputDir);

var boundNodes = SpecificationLoader.LoadBoundNodes(boundTreeDir);
var symbolClasses = SpecificationLoader.LoadSymbolClasses(symbolsDir);
var symbolInterfaces = SpecificationLoader.LoadSymbolInterfaces(symbolsDir);

var generatedBoundFiles = 0;
var generatedSymbolFiles = 0;

foreach (var node in boundNodes.Where(static n => !n.IsAbstract))
{
    var text = BoundNodePartialGenerator.Generate(node);
    File.WriteAllText(Path.Combine(boundOutputDir, $"{node.Name}.g.cs"), text);
    generatedBoundFiles++;
}

File.WriteAllText(Path.Combine(boundOutputDir, "BoundNodeFactory.g.cs"), BoundNodeFactoryGenerator.Generate(boundNodes));
generatedBoundFiles++;

File.WriteAllText(Path.Combine(boundOutputDir, "BoundTreeVisitor.g.cs"), BoundTreeVisitorGenerator.GenerateVisitor(boundNodes));
generatedBoundFiles++;

File.WriteAllText(Path.Combine(boundOutputDir, "BoundTreeVisitor.Generic.g.cs"), BoundTreeVisitorGenerator.GenerateGenericVisitor(boundNodes));
generatedBoundFiles++;

File.WriteAllText(Path.Combine(boundOutputDir, "BoundTreeRewriter.g.cs"), BoundTreeRewriterGenerator.Generate(boundNodes));
generatedBoundFiles++;

foreach (var symbol in symbolClasses)
{
    var text = SymbolPartialGenerator.Generate(symbol);
    File.WriteAllText(Path.Combine(symbolOutputDir, $"{symbol.Name}.g.cs"), text);
    generatedSymbolFiles++;
}

File.WriteAllText(Path.Combine(symbolOutputDir, "SymbolVisitor.g.cs"), SymbolVisitorGenerator.GenerateVisitor(symbolInterfaces));
generatedSymbolFiles++;

File.WriteAllText(Path.Combine(symbolOutputDir, "SymbolVisitor.Generic.g.cs"), SymbolVisitorGenerator.GenerateGenericVisitor(symbolInterfaces));
generatedSymbolFiles++;

File.WriteAllText(stampPath, hash);

Console.WriteLine($"Generated {generatedBoundFiles} bound node files and {generatedSymbolFiles} symbol files.");

static string FindRepositoryRoot()
{
    var current = AppContext.BaseDirectory;

    while (!string.IsNullOrEmpty(current))
    {
        if (File.Exists(Path.Combine(current, "Raven.sln")))
            return current;

        current = Path.GetDirectoryName(current);
    }

    throw new InvalidOperationException("Unable to locate repository root.");
}

static IEnumerable<string> EnumerateInputs(string boundTreeDir, string symbolsDir)
{
    foreach (var file in Directory.EnumerateFiles(boundTreeDir, "*.cs", SearchOption.AllDirectories))
    {
        if (!file.Contains($"{Path.DirectorySeparatorChar}Generated{Path.DirectorySeparatorChar}", StringComparison.Ordinal))
            yield return file;
    }

    foreach (var file in Directory.EnumerateFiles(symbolsDir, "*.cs", SearchOption.AllDirectories))
    {
        if (!file.Contains($"{Path.DirectorySeparatorChar}Generated{Path.DirectorySeparatorChar}", StringComparison.Ordinal))
            yield return file;
    }

    yield return Assembly.GetExecutingAssembly().Location;
}

static string ComputeHash(IEnumerable<string> inputs)
{
    using var sha = SHA256.Create();

    foreach (var path in inputs.OrderBy(p => p, StringComparer.Ordinal))
    {
        var bytes = File.ReadAllBytes(path);
        sha.TransformBlock(bytes, 0, bytes.Length, null, 0);
    }

    sha.TransformFinalBlock(Array.Empty<byte>(), 0, 0);
    return Convert.ToHexString(sha.Hash!);
}

static void DeleteGeneratedFiles(string directory)
{
    if (!Directory.Exists(directory))
        return;

    foreach (var file in Directory.EnumerateFiles(directory, "*.g.cs", SearchOption.TopDirectoryOnly))
    {
        File.Delete(file);
    }
}

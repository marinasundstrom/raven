using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

internal static class ProjectSystemGeneratedDocumentHelper
{
    private static readonly string[] s_standardPreludeImports =
    [
        "System.*",
        "System.Collections.*",
        "System.Collections.Generic.*",
        "System.IO.*",
        "System.Linq.*",
        "System.Net.Http.*",
        "System.Threading.*",
        "System.Threading.Tasks.*",
        "System.Result.*",
        "System.Option.*"
    ];

    public static Solution AddGeneratedPreludeDocument(
        Solution solution,
        ProjectId projectId,
        string generatedDirectory,
        string generatedName,
        ProjectPreludeOptions options)
    {
        var project = solution.GetProject(projectId);
        if (project is null)
            return solution;

        var importLines = options.GenerateStandardImports
            ? s_standardPreludeImports.ToList()
            : [];
        var aliasLines = new List<string>();

        foreach (var import in options.Imports)
        {
            if (string.IsNullOrWhiteSpace(import.Include))
                continue;

            var include = import.Include.Trim();
            if (!string.IsNullOrWhiteSpace(import.Alias))
            {
                aliasLines.Add($"alias {import.Alias.Trim()} = {include}");
                continue;
            }

            importLines.Add(ToImportName(include, import.Static));
        }

        if (importLines.Count == 0 && aliasLines.Count == 0)
            return solution;

        var sourceLines = new List<string>();
        if (importLines.Count > 0)
        {
            sourceLines.Add("global {");
            sourceLines.AddRange(importLines.Distinct(StringComparer.Ordinal).Select(static importName => $"    import {importName}"));
            sourceLines.Add("}");
        }

        if (aliasLines.Count > 0)
        {
            if (sourceLines.Count > 0)
                sourceLines.Add(string.Empty);

            sourceLines.AddRange(aliasLines);
        }

        sourceLines.Add(string.Empty);
        var generatedSource = string.Join(Environment.NewLine, sourceLines);

        Directory.CreateDirectory(generatedDirectory);

        var generatedPath = Path.Combine(generatedDirectory, generatedName);
        File.WriteAllText(generatedPath, generatedSource);

        var generatedId = DocumentId.CreateNew(projectId);
        return solution.AddDocument(generatedId, generatedName, SourceText.From(generatedSource), generatedPath);
    }

    private static string ToImportName(string include, bool isStatic)
    {
        if (include.EndsWith(".*", StringComparison.Ordinal))
            return include;

        return isStatic ? include + ".*" : include + ".*";
    }

    public static Solution AddGeneratedTargetFrameworkAttributeDocumentIfNeeded(
        Solution solution,
        ProjectId projectId,
        string generatedDirectory,
        string generatedName,
        string? targetFramework)
    {
        if (string.IsNullOrWhiteSpace(targetFramework))
            return solution;

        var project = solution.GetProject(projectId);
        if (project is null || ContainsAssemblyTargetFrameworkAttribute(project))
            return solution;

        if (!TargetFrameworkMoniker.TryParse(targetFramework, out var tfm) || tfm is null)
            return solution;

        var frameworkString = tfm.ToFrameworkString();
        var generatedSource = string.Join(
            Environment.NewLine,
            "import System.Runtime.Versioning.*",
            string.Empty,
            $"[assembly: TargetFramework(\"{EscapeRavenString(frameworkString)}\")]",
            string.Empty);

        Directory.CreateDirectory(generatedDirectory);

        var generatedPath = Path.Combine(generatedDirectory, generatedName);
        File.WriteAllText(generatedPath, generatedSource);

        var generatedId = DocumentId.CreateNew(projectId);
        return solution.AddDocument(generatedId, generatedName, SourceText.From(generatedSource), generatedPath);
    }

    private static bool ContainsAssemblyTargetFrameworkAttribute(Project project)
    {
        foreach (var document in project.Documents)
        {
            if (!RavenFileExtensions.HasRavenExtension(document.Name) &&
                (document.FilePath is null || !RavenFileExtensions.HasRavenExtension(document.FilePath)))
            {
                continue;
            }

            var tree = SyntaxTree.ParseText(document.Text, path: document.FilePath ?? document.Name);
            if (tree.GetRoot() is not CompilationUnitSyntax compilationUnit)
                continue;

            foreach (var attributeList in compilationUnit.AttributeLists)
            {
                if (!string.Equals(attributeList.Target?.Identifier.ValueText, "assembly", StringComparison.OrdinalIgnoreCase))
                    continue;

                foreach (var attribute in attributeList.Attributes)
                {
                    var name = attribute.Name.ToString();
                    if (name.Equals("TargetFramework", StringComparison.OrdinalIgnoreCase) ||
                        name.Equals("TargetFrameworkAttribute", StringComparison.OrdinalIgnoreCase) ||
                        name.EndsWith(".TargetFramework", StringComparison.OrdinalIgnoreCase) ||
                        name.EndsWith(".TargetFrameworkAttribute", StringComparison.OrdinalIgnoreCase))
                    {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    private static string EscapeRavenString(string value)
        => value.Replace("\\", "\\\\", StringComparison.Ordinal).Replace("\"", "\\\"", StringComparison.Ordinal);
}

internal sealed record ProjectPreludeOptions(
    bool GenerateStandardImports,
    ImmutableArray<ProjectPreludeImportInfo> Imports)
{
    public static ProjectPreludeOptions Default { get; } = new(true, ImmutableArray<ProjectPreludeImportInfo>.Empty);
}

internal readonly record struct ProjectPreludeImportInfo(
    string Include,
    bool Static,
    string? Alias);

using System;
using System.IO;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

internal static class ProjectSystemGeneratedDocumentHelper
{
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

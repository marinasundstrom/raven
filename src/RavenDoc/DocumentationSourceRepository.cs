using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

/// <summary>GitHub repository and optional Raven declarations for metadata-only API inputs.</summary>
public sealed record DocumentationSourceRepository(
    string Url,
    string Revision = "main",
    string Root = ".",
    IReadOnlyList<string>? Paths = null);

internal sealed class DocumentationSourceLinks
{
    private readonly DocumentationSourceRepository? repository;
    private readonly Dictionary<string, List<string>> declarations = new(StringComparer.Ordinal);

    public DocumentationSourceLinks(DocumentationSourceRepository? repository)
    {
        this.repository = repository;
        if (repository is null) return;
        if (!Uri.TryCreate(repository.Url, UriKind.Absolute, out var uri) ||
            uri.Scheme != Uri.UriSchemeHttps || string.IsNullOrWhiteSpace(repository.Revision) ||
            !string.IsNullOrEmpty(uri.Query) || !string.IsNullOrEmpty(uri.Fragment))
            throw new InvalidOperationException("Source repository requires an HTTPS repository URL and a revision.");
        foreach (var input in repository.Paths ?? [])
        {
            var path = Path.GetFullPath(input, repository.Root);
            if (!File.Exists(path) && !Directory.Exists(path))
                throw new InvalidOperationException($"Source repository input does not exist: {path}");
            foreach (var file in (Directory.Exists(path)
                ? Directory.EnumerateFiles(path, "*.rvn", SearchOption.AllDirectories)
                : [path]).Order(StringComparer.Ordinal))
            {
                if (RelativePath(file) is null)
                    throw new InvalidOperationException($"Source input must be inside the repository root: {file}");
                var tree = SyntaxTree.ParseText(File.ReadAllText(file));
                foreach (var declaration in tree.GetRoot().DescendantNodes().OfType<BaseTypeDeclarationSyntax>())
                {
                    var parts = declaration.AncestorsAndSelf().Reverse().Select(node => node switch
                    {
                        BaseNamespaceDeclarationSyntax ns => ns.Name.ToString(),
                        BaseTypeDeclarationSyntax type => TypeName(type),
                        _ => null
                    }).Where(part => part is not null);
                    var name = string.Join(".", parts);
                    if (!declarations.TryGetValue(name, out var files))
                        declarations[name] = files = [];
                    if (!files.Contains(file, StringComparer.Ordinal)) files.Add(file);
                }
            }
        }
    }

    private static string TypeName(BaseTypeDeclarationSyntax type)
    {
        var arity = type.ChildNodes().OfType<TypeParameterListSyntax>().FirstOrDefault()?.Parameters.Count ?? 0;
        return type.Identifier.ValueText + (arity > 0 ? "`" + arity : "");
    }

    private static string TypeName(INamedTypeSymbol type)
    {
        var prefix = type.ContainingType is { } parent ? TypeName(parent)
            : type.ContainingNamespace is { IsGlobalNamespace: false } ns ? ns.ToDisplayString() : "";
        return (prefix.Length > 0 ? prefix + "." : "") + type.MetadataName;
    }

    private string? RelativePath(string path)
    {
        var relative = Path.GetRelativePath(Path.GetFullPath(repository!.Root), Path.GetFullPath(path));
        return relative == ".." || relative.StartsWith(".." + Path.DirectorySeparatorChar) || Path.IsPathRooted(relative)
            ? null : relative;
    }

    private string? Url(string path, int? line = null)
    {
        if (repository is null || RelativePath(path) is not { } relative) return null;
        var encoded = string.Join("/", relative.Replace('\\', '/').Split('/').Select(Uri.EscapeDataString));
        return repository.Url.TrimEnd('/') + "/blob/" + Uri.EscapeDataString(repository.Revision) + "/" + encoded +
            (line is { } number ? "#L" + number : "");
    }

    public IReadOnlyList<(string File, string? Url)> GetLinks(ISymbol symbol)
    {
        var source = symbol.Locations.Where(location => location.IsInSource &&
            !string.IsNullOrWhiteSpace(location.SourceTree?.FilePath)).ToArray();
        if (source.Length > 0)
            return source.Select(location => (Path.GetFileName(location.SourceTree!.FilePath),
                Url(location.SourceTree.FilePath, location.GetLineSpan().StartLinePosition.Line + 1))).Distinct().ToArray();
        // Metadata members point to their declaring type's files, without claiming an exact member line.
        var type = symbol as INamedTypeSymbol ?? symbol.ContainingType;
        if (type is null || !declarations.TryGetValue(TypeName(type), out var files)) return [];
        return files.Select(file => (Path.GetFileName(file), Url(file))).ToArray();
    }
}

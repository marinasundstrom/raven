using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using CSharpSyntax = Microsoft.CodeAnalysis.CSharp.Syntax;

/// <summary>GitHub repository and optional Raven or C# declarations for metadata-only API inputs.</summary>
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
                ? Directory.EnumerateFiles(path, "*", SearchOption.AllDirectories)
                    .Where(file => Path.GetExtension(file) is ".rvn" or ".cs")
                    .Where(file => !Path.GetRelativePath(path, file).Split(Path.DirectorySeparatorChar)
                        .Any(part => part is "bin" or "obj" or ".git"))
                : [path]).Order(StringComparer.Ordinal))
            {
                if (RelativePath(file) is null)
                    throw new InvalidOperationException($"Source input must be inside the repository root: {file}");
                if (Path.GetExtension(file).Equals(".cs", StringComparison.OrdinalIgnoreCase))
                {
                    IndexCSharp(file);
                    continue;
                }
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
                    AddDeclaration(name, file);
                }
            }
        }
    }

    private void AddDeclaration(string name, string file)
    {
        if (!declarations.TryGetValue(name, out var files))
            declarations[name] = files = [];
        if (!files.Contains(file, StringComparer.Ordinal)) files.Add(file);
    }

    private void IndexCSharp(string file)
    {
        var tree = Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText(File.ReadAllText(file));
        foreach (var declaration in tree.GetRoot().DescendantNodes().Where(node =>
                     node is CSharpSyntax.BaseTypeDeclarationSyntax or CSharpSyntax.DelegateDeclarationSyntax))
        {
            var parts = declaration.AncestorsAndSelf().Reverse().Select(node => node switch
            {
                CSharpSyntax.BaseNamespaceDeclarationSyntax ns => string.Join(".",
                    ns.Name.DescendantTokens().Where(token => token.RawKind ==
                        (int)Microsoft.CodeAnalysis.CSharp.SyntaxKind.IdentifierToken).Select(token => token.ValueText)),
                CSharpSyntax.TypeDeclarationSyntax type => MetadataName(type.Identifier.ValueText, type.TypeParameterList?.Parameters.Count ?? 0),
                CSharpSyntax.BaseTypeDeclarationSyntax type => type.Identifier.ValueText,
                CSharpSyntax.DelegateDeclarationSyntax type => MetadataName(type.Identifier.ValueText, type.TypeParameterList?.Parameters.Count ?? 0),
                _ => null
            }).Where(part => part is not null);
            AddDeclaration(string.Join(".", parts), file);
        }
    }

    private static string MetadataName(string name, int arity) => name + (arity > 0 ? "`" + arity : "");

    private static string TypeName(BaseTypeDeclarationSyntax type)
    {
        var arity = type.ChildNodes().OfType<TypeParameterListSyntax>().FirstOrDefault()?.Parameters.Count ?? 0;
        return type.Identifier.ValueText + (arity > 0 ? "`" + arity : "");
    }

    private static string TypeName(INamedTypeSymbol type)
    {
        var prefix = type.ContainingType is { } parent ? TypeName(parent)
            : type.ContainingNamespace is { IsGlobalNamespace: false } ns ? NamespaceName(ns) : "";
        return (prefix.Length > 0 ? prefix + "." : "") + type.MetadataName;
    }

    private static string NamespaceName(INamespaceSymbol ns)
    {
        var parts = new Stack<string>();
        for (var current = ns; current is { IsGlobalNamespace: false }; current = current.ContainingNamespace)
            parts.Push(current.Name);
        return string.Join(".", parts);
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

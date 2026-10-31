using Raven.CodeAnalysis;
#if NEOCLR_METADATA
using Raven.CodeAnalysis.NeoClr;
#endif

/// <summary>Explicit native semantic input; never loads framework or adjacent assembly references.</summary>
internal static class NativeDocumentationInput
{
    internal static void Generate(IReadOnlyList<string> inputs, string output, string corePath,
        IReadOnlyList<string> dependencies, DocumentationSiteOptions options)
    {
#if NEOCLR_METADATA
        corePath = Path.GetFullPath(corePath);
        var targets = inputs.Select(Path.GetFullPath).ToArray();
        if (targets.Length == 0 || targets.Distinct(StringComparer.OrdinalIgnoreCase).Count() != targets.Length)
            throw new ArgumentException("Specify distinct native documentation inputs.");
        var paths = targets.Concat(dependencies.Select(Path.GetFullPath))
            .Where(path => !string.Equals(path, corePath, StringComparison.OrdinalIgnoreCase))
            .Distinct(StringComparer.OrdinalIgnoreCase).ToArray();
        var outputRoot = Path.GetFullPath(output).TrimEnd(Path.DirectorySeparatorChar) + Path.DirectorySeparatorChar;
        if (paths.Append(corePath).Any(path => path.StartsWith(outputRoot, StringComparison.OrdinalIgnoreCase) ||
            string.Equals(path, outputRoot.TrimEnd(Path.DirectorySeparatorChar), StringComparison.OrdinalIgnoreCase)))
            throw new ArgumentException("Native documentation output must not contain metadata inputs.");
        var catalog = NeoClrReferenceCatalog.ReadNative(corePath, paths);
        var compilation = Compilation.Create("RavenDoc.NativeMetadataHost", options:
            CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
                .WithTargetCoreAssemblyName(catalog.CoreIdentity.Name)
                .WithRuntimeTypeOfContract(null)
                .WithRuntimeUnitContract(new(catalog.CoreIdentity.Name, "System.Void"))
                .WithMetadataImportOptions(new MetadataImportOptions(catalog.CoreIdentity.Name)
                    .WithObjectAssemblyName(catalog.CoreIdentity.Name).WithNativeMetadata()))
            .AddReferences(catalog.References.ToArray());
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (errors.Length != 0)
            throw new InvalidDataException(string.Join(Environment.NewLine, errors.Select(d => d.ToString())));
        var byPath = new Dictionary<string, MetadataReference>(StringComparer.OrdinalIgnoreCase)
        {
            [corePath] = catalog.CoreReference
        };
        for (var i = 0; i < paths.Length; i++) byPath.Add(paths[i], catalog.References[i + 1]);
        var assemblies = targets.Select(path => compilation.GetAssemblyOrModuleSymbol(byPath[path]) as IAssemblySymbol
            ?? throw new InvalidDataException("Could not load native assembly symbols: " + path)).ToArray();
        // Existing page URLs are namespace/type based, so ambiguous targets cannot share a site.
        var names = new HashSet<string>(StringComparer.Ordinal);
        void CheckNames(INamespaceOrTypeSymbol owner, string prefix)
        {
            foreach (var member in owner.GetMembers())
            {
                var name = prefix + member.MetadataName;
                if (member is INamespaceSymbol ns) CheckNames(ns, name + ".");
                else if (member is INamedTypeSymbol type && type.DeclaredAccessibility == Accessibility.Public)
                {
                    if (!names.Add(name)) throw new InvalidDataException("Conflicting native documentation type: " + name);
                    CheckNames(type, name + "+");
                }
            }
        }
        foreach (var assembly in assemblies) CheckNames(assembly.GlobalNamespace, "");
        if (assemblies.Length == 1)
            DocumentationGenerator.ProcessAssembly(compilation, assemblies[0], output, options);
        else
            DocumentationGenerator.ProcessAssemblies(compilation, assemblies, output, options);
#else
        throw new NotSupportedException("Native documentation requires a RavenDoc build with NeoClrMetadataProject configured.");
#endif
    }
}

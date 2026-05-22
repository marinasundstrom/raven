using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class UnusedImportDirectiveAnalyzer : DiagnosticAnalyzer
{
    public const string DiagnosticId = "RAV9031";

    private static readonly DiagnosticDescriptor Descriptor = DiagnosticDescriptor.Create(
        id: DiagnosticId,
        title: "Import directive is unused",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "Import directive '{0}' is unused within this scope.",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Hidden);

    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => [Descriptor];

    public override void Initialize(AnalysisContext context)
        => context.RegisterSyntaxTreeAction(AnalyzeSyntaxTree);

    private static void AnalyzeSyntaxTree(SyntaxTreeAnalysisContext context)
    {
        var root = context.SyntaxTree.GetRoot(context.CancellationToken);
        if (root is not CompilationUnitSyntax compilationUnit)
            return;

        var semanticModel = context.Compilation.GetSemanticModel(context.SyntaxTree);
        if (HasBlockingDiagnostics(context.SyntaxTree, semanticModel, context.CancellationToken))
            return;

        AnalyzeImportScope(context, semanticModel, compilationUnit, compilationUnit.Imports);

        foreach (var namespaceDeclaration in compilationUnit.DescendantNodes().OfType<BaseNamespaceDeclarationSyntax>())
            AnalyzeImportScope(context, semanticModel, namespaceDeclaration, namespaceDeclaration.Imports);
    }

    private static void AnalyzeImportScope(
        SyntaxTreeAnalysisContext context,
        SemanticModel semanticModel,
        SyntaxNode scope,
        IEnumerable<ImportDirectiveSyntax> imports)
    {
        foreach (var import in imports)
        {
            context.CancellationToken.ThrowIfCancellationRequested();

            if (!TryGetWildcardNamespaceImport(context.Compilation, import, out var importedNamespace, out var importName))
                continue;

            if (IsNamespaceUsedInScope(context.Compilation, semanticModel, scope, import, importedNamespace, importName, context.CancellationToken))
                continue;

            context.ReportDiagnostic(Diagnostic.Create(Descriptor, import.Name.GetLocation(), importName));
        }
    }

    private static bool HasBlockingDiagnostics(
        SyntaxTree syntaxTree,
        SemanticModel semanticModel,
        CancellationToken cancellationToken)
    {
        if (syntaxTree.GetDiagnostics(cancellationToken).Any(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error))
            return true;

        return semanticModel.GetDocumentDiagnostics(cancellationToken)
            .Any(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    private static bool TryGetWildcardNamespaceImport(
        Compilation compilation,
        ImportDirectiveSyntax import,
        out INamespaceSymbol importedNamespace,
        out string importName)
    {
        importedNamespace = null!;
        importName = string.Empty;

        if (import.Name is not QualifiedNameSyntax { Left: var namespaceName, Right: WildcardNameSyntax })
            return false;

        importName = namespaceName.ToString();
        var resolved = ResolveNamespace(compilation.GlobalNamespace, importName);
        if (resolved is null)
            return false;

        importedNamespace = resolved;
        return true;
    }

    private static bool IsNamespaceUsedInScope(
        Compilation compilation,
        SemanticModel semanticModel,
        SyntaxNode scope,
        ImportDirectiveSyntax import,
        INamespaceSymbol importedNamespace,
        string importName,
        CancellationToken cancellationToken)
    {
        foreach (var qualifiedName in scope.DescendantNodes().OfType<QualifiedNameSyntax>())
        {
            cancellationToken.ThrowIfCancellationRequested();

            if (qualifiedName.GetAncestor<ImportDirectiveSyntax>() is not null ||
                qualifiedName.GetAncestor<AliasDirectiveSyntax>() is not null ||
                qualifiedName.GetAncestor<BaseNamespaceDeclarationSyntax>() is { } namespaceDeclaration &&
                namespaceDeclaration.Name.DescendantNodesAndSelf().Contains(qualifiedName, ReferenceEqualityComparer.Instance))
                continue;

            if (GetLeftmostSimpleName(qualifiedName) is { } leftmost &&
                ReferencesImportedNamespaceBySyntax(compilation, leftmost, importName))
                return true;
        }

        foreach (var name in scope.DescendantNodes().OfType<SimpleNameSyntax>())
        {
            cancellationToken.ThrowIfCancellationRequested();

            if (!IsReferenceCandidate(name, import))
                continue;

            ISymbol? symbol;
            try
            {
                symbol = semanticModel.GetSymbolInfo(name, cancellationToken).Symbol;
            }
            catch (NotSupportedException)
            {
                continue;
            }

            if (symbol is null)
            {
                if (ReferencesImportedNamespaceBySyntax(compilation, name, importName))
                    return true;

                continue;
            }

            if (ReferencesImportedNamespace(symbol, importName))
                return true;

            if (IsQualifiedNameLeftmost(name) && ReferencesImportedNamespaceBySyntax(compilation, name, importName))
                return true;
        }

        return false;
    }

    private static bool IsReferenceCandidate(SimpleNameSyntax name, ImportDirectiveSyntax import)
    {
        if (ReferenceEquals(name, import.Name) || name.AncestorsAndSelf().Contains(import, ReferenceEqualityComparer.Instance))
            return false;

        if (name.GetAncestor<ImportDirectiveSyntax>() is not null ||
            name.GetAncestor<AliasDirectiveSyntax>() is not null ||
            name.GetAncestor<BaseNamespaceDeclarationSyntax>() is { } namespaceDeclaration &&
            namespaceDeclaration.Name.DescendantNodesAndSelf().Contains(name, ReferenceEqualityComparer.Instance))
            return false;

        if (name.Parent is QualifiedNameSyntax qualifiedName)
            return ReferenceEquals(qualifiedName.Left, name);

        if (name.Parent is MemberAccessExpressionSyntax memberAccess)
            return ReferenceEquals(memberAccess.Expression, name);

        return true;
    }

    private static SimpleNameSyntax? GetLeftmostSimpleName(NameSyntax name)
        => name switch
        {
            SimpleNameSyntax simpleName => simpleName,
            QualifiedNameSyntax qualifiedName => GetLeftmostSimpleName(qualifiedName.Left),
            _ => null
        };

    private static bool IsQualifiedNameLeftmost(SimpleNameSyntax name)
        => name.Parent is QualifiedNameSyntax qualifiedName &&
           ReferenceEquals(qualifiedName.Left, name);

    private static bool ReferencesImportedNamespaceBySyntax(Compilation compilation, SimpleNameSyntax name, string importedNamespaceName)
        => ResolveNamespace(compilation.GlobalNamespace, importedNamespaceName + "." + name.Identifier.ValueText) is not null;

    private static bool ReferencesImportedNamespace(ISymbol symbol, string importedNamespaceName)
    {
        symbol = symbol.UnderlyingSymbol;

        if (symbol is INamespaceSymbol namespaceSymbol)
        {
            var namespaceName = namespaceSymbol.ToMetadataName();
            if (string.IsNullOrEmpty(namespaceName))
                return false;

            return IsNestedNamespaceName(namespaceName, importedNamespaceName);
        }

        var containingNamespaceName = symbol.ContainingNamespace?.ToMetadataName();
        return !string.IsNullOrEmpty(containingNamespaceName) &&
               (string.Equals(containingNamespaceName, importedNamespaceName, StringComparison.Ordinal) ||
                IsNestedNamespaceName(containingNamespaceName, importedNamespaceName));
    }

    private static bool IsNestedNamespaceName(string namespaceName, string ancestorName)
        => namespaceName.Length > ancestorName.Length &&
           namespaceName.StartsWith(ancestorName, StringComparison.Ordinal) &&
           namespaceName[ancestorName.Length] == '.';

    private static INamespaceSymbol? ResolveNamespace(INamespaceSymbol root, string metadataName)
    {
        INamespaceSymbol? current = root;
        foreach (var part in metadataName.Split('.', StringSplitOptions.RemoveEmptyEntries))
        {
            current = ResolveNamespaceMember(current, part);
            if (current is null)
                return null;
        }

        return current;
    }

    private static INamespaceSymbol? ResolveNamespaceMember(INamespaceSymbol namespaceSymbol, string name)
        => namespaceSymbol.LookupNamespace(name)
           ?? namespaceSymbol.GetMembers(name).OfType<INamespaceSymbol>().FirstOrDefault();
}

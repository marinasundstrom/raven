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
    {
        context.EnableConcurrentExecution();

        context.RegisterSyntaxTreeAction(AnalyzeSyntaxTree);
    }

    private static void AnalyzeSyntaxTree(SyntaxTreeAnalysisContext context)
    {
        var root = context.SyntaxTree.GetRoot(context.CancellationToken);
        if (root is not CompilationUnitSyntax compilationUnit)
            return;

        var semanticModel = context.Compilation.GetSemanticModel(context.SyntaxTree);
        if (HasBlockingSyntaxDiagnostics(context.SyntaxTree, context.CancellationToken))
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
        var wildcardImports = imports
            .Select(import => new
            {
                Import = import,
                HasNamespace = TryGetWildcardNamespaceImport(context.Compilation, import, out _, out var importName),
                ImportName = importName
            })
            .Where(static item => item.HasNamespace)
            .ToArray();

        if (wildcardImports.Length == 0)
            return;

        var usageFacts = CollectScopeUsageFacts(context.Compilation, semanticModel, scope, context.CancellationToken);

        foreach (var import in wildcardImports)
        {
            context.CancellationToken.ThrowIfCancellationRequested();

            if (IsNamespaceUsedInScope(context.Compilation, usageFacts, import.ImportName))
                continue;

            context.ReportDiagnostic(Diagnostic.Create(Descriptor, import.Import.Name.GetLocation(), import.ImportName));
        }
    }

    private static bool HasBlockingSyntaxDiagnostics(
        SyntaxTree syntaxTree,
        CancellationToken cancellationToken)
        => syntaxTree.GetDiagnostics(cancellationToken)
            .Any(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

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
        var resolved = compilation.SymbolLookup.GetNamespace(importName);
        if (resolved is null)
            return false;

        importedNamespace = resolved;
        return true;
    }

    private static ScopeUsageFacts CollectScopeUsageFacts(
        Compilation compilation,
        SemanticModel semanticModel,
        SyntaxNode scope,
        CancellationToken cancellationToken)
    {
        var syntaxReferenceNames = new HashSet<string>(StringComparer.Ordinal);
        var referencedNamespaceNames = new HashSet<string>(StringComparer.Ordinal);

        foreach (var qualifiedName in scope.DescendantNodes().OfType<QualifiedNameSyntax>())
        {
            cancellationToken.ThrowIfCancellationRequested();

            if (qualifiedName.GetAncestor<ImportDirectiveSyntax>() is not null ||
                qualifiedName.GetAncestor<AliasDirectiveSyntax>() is not null ||
                qualifiedName.GetAncestor<BaseNamespaceDeclarationSyntax>() is { } namespaceDeclaration &&
                namespaceDeclaration.Name.DescendantNodesAndSelf().Contains(qualifiedName, ReferenceEqualityComparer.Instance))
                continue;

            if (GetLeftmostSimpleName(qualifiedName) is { } leftmost)
                syntaxReferenceNames.Add(leftmost.Identifier.ValueText);
        }

        foreach (var name in scope.DescendantNodes().OfType<SimpleNameSyntax>())
        {
            cancellationToken.ThrowIfCancellationRequested();

            if (!IsReferenceCandidate(name))
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
                syntaxReferenceNames.Add(name.Identifier.ValueText);

                continue;
            }

            AddReferencedNamespaceNames(symbol, referencedNamespaceNames);

            if (IsQualifiedNameLeftmost(name))
                syntaxReferenceNames.Add(name.Identifier.ValueText);
        }

        return new ScopeUsageFacts(syntaxReferenceNames, referencedNamespaceNames);
    }

    private static bool IsNamespaceUsedInScope(
        Compilation compilation,
        ScopeUsageFacts usageFacts,
        string importName)
    {
        foreach (var name in usageFacts.SyntaxReferenceNames)
        {
            if (ReferencesImportedNamespaceBySyntax(compilation, name, importName))
                return true;
        }

        foreach (var namespaceName in usageFacts.ReferencedNamespaceNames)
        {
            if (ReferencesImportedNamespace(namespaceName, importName))
                return true;
        }

        return false;
    }

    private static bool IsReferenceCandidate(SimpleNameSyntax name)
    {
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

    private static bool ReferencesImportedNamespaceBySyntax(Compilation compilation, string name, string importedNamespaceName)
        => compilation.SymbolLookup.GetNamespace(importedNamespaceName + "." + name) is not null;

    private static void AddReferencedNamespaceNames(ISymbol symbol, HashSet<string> referencedNamespaceNames)
    {
        symbol = symbol.UnderlyingSymbol;

        if (symbol is INamespaceSymbol namespaceSymbol)
        {
            var namespaceName = namespaceSymbol.ToMetadataName();
            if (!string.IsNullOrEmpty(namespaceName))
                referencedNamespaceNames.Add(namespaceName);
            return;
        }

        var containingNamespaceName = symbol.ContainingNamespace?.ToMetadataName();
        if (!string.IsNullOrEmpty(containingNamespaceName))
            referencedNamespaceNames.Add(containingNamespaceName);
    }

    private static bool ReferencesImportedNamespace(string namespaceName, string importedNamespaceName)
    {
        if (string.IsNullOrEmpty(namespaceName))
            return false;

        return string.Equals(namespaceName, importedNamespaceName, StringComparison.Ordinal) ||
               IsNestedNamespaceName(namespaceName, importedNamespaceName);
    }

    private static bool IsNestedNamespaceName(string namespaceName, string ancestorName)
        => namespaceName.Length > ancestorName.Length &&
           namespaceName.StartsWith(ancestorName, StringComparison.Ordinal) &&
           namespaceName[ancestorName.Length] == '.';

    private sealed record ScopeUsageFacts(
        HashSet<string> SyntaxReferenceNames,
        HashSet<string> ReferencedNamespaceNames);

}

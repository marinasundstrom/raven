using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class SourceDeclarationIndex
{
    private readonly Compilation _compilation;
    private readonly Dictionary<NamespaceMemberFunctionLookupKey, ImmutableArray<FunctionStatementSyntax>> _namespaceFunctions;
    private readonly Dictionary<NamespaceTypeLookupKey, ImmutableArray<SyntaxNode>> _namespaceTypes;

    public SourceDeclarationIndex(Compilation compilation, IEnumerable<SyntaxTree> syntaxTrees)
    {
        _compilation = compilation;
        var namespaceFunctions = new Dictionary<NamespaceMemberFunctionLookupKey, ImmutableArray<FunctionStatementSyntax>.Builder>();
        var namespaceTypes = new Dictionary<NamespaceTypeLookupKey, ImmutableArray<SyntaxNode>.Builder>();

        foreach (var syntaxTree in syntaxTrees)
        {
            if (syntaxTree.GetRoot() is CompilationUnitSyntax compilationUnit)
                IndexContainer(compilationUnit, GetCompilationUnitNamespace(compilationUnit), namespaceFunctions, namespaceTypes);
        }

        _namespaceFunctions = namespaceFunctions.ToDictionary(
            static pair => pair.Key,
            static pair => pair.Value.ToImmutable());
        _namespaceTypes = namespaceTypes.ToDictionary(
            static pair => pair.Key,
            static pair => pair.Value.ToImmutable());
    }

    public ImmutableArray<FunctionStatementSyntax> GetNamespaceFunctions(string namespaceMetadataName, string name)
    {
        if (string.IsNullOrWhiteSpace(name))
            return ImmutableArray<FunctionStatementSyntax>.Empty;

        var key = new NamespaceMemberFunctionLookupKey(namespaceMetadataName ?? string.Empty, name);
        return _namespaceFunctions.TryGetValue(key, out var functions)
            ? functions
            : ImmutableArray<FunctionStatementSyntax>.Empty;
    }

    public ImmutableArray<SyntaxNode> GetNamespaceTypes(string namespaceMetadataName, string name)
    {
        if (string.IsNullOrWhiteSpace(name))
            return ImmutableArray<SyntaxNode>.Empty;

        var key = new NamespaceTypeLookupKey(namespaceMetadataName ?? string.Empty, name);
        return _namespaceTypes.TryGetValue(key, out var types)
            ? types
            : ImmutableArray<SyntaxNode>.Empty;
    }

    private void IndexContainer(
        SyntaxNode containerNode,
        string namespaceMetadataName,
        Dictionary<NamespaceMemberFunctionLookupKey, ImmutableArray<FunctionStatementSyntax>.Builder> namespaceFunctions,
        Dictionary<NamespaceTypeLookupKey, ImmutableArray<SyntaxNode>.Builder> namespaceTypes)
    {
        foreach (var member in containerNode.ChildNodes())
        {
            switch (member)
            {
                case BaseNamespaceDeclarationSyntax namespaceDeclaration:
                    IndexContainer(
                        namespaceDeclaration,
                        GetNamespaceDeclarationMetadataName(namespaceMetadataName, namespaceDeclaration),
                        namespaceFunctions,
                        namespaceTypes);
                    break;

                case BaseTypeDeclarationSyntax typeDeclaration:
                    AddType(namespaceTypes, namespaceMetadataName, typeDeclaration.Identifier.ValueText, typeDeclaration);
                    break;

                case DelegateDeclarationSyntax delegateDeclaration:
                    AddType(namespaceTypes, namespaceMetadataName, delegateDeclaration.Identifier.ValueText, delegateDeclaration);
                    break;

                case ExtensionDeclarationSyntax extensionDeclaration:
                    AddType(namespaceTypes, namespaceMetadataName, extensionDeclaration.Identifier.ValueText, extensionDeclaration);
                    break;

                case GlobalStatementSyntax globalStatement
                    when Compilation.IsTopLevelFunctionMember(globalStatement) &&
                         !IsFileScopeLocalFunction(globalStatement) &&
                         globalStatement.Statement is FunctionStatementSyntax functionStatement:
                    AddNamespaceFunction(namespaceFunctions, namespaceMetadataName, functionStatement);
                    break;
            }
        }
    }

    private static void AddType(
        Dictionary<NamespaceTypeLookupKey, ImmutableArray<SyntaxNode>.Builder> namespaceTypes,
        string namespaceMetadataName,
        string name,
        SyntaxNode declaration)
    {
        if (string.IsNullOrWhiteSpace(name))
            return;

        var key = new NamespaceTypeLookupKey(namespaceMetadataName, name);
        if (!namespaceTypes.TryGetValue(key, out var types))
        {
            types = ImmutableArray.CreateBuilder<SyntaxNode>();
            namespaceTypes.Add(key, types);
        }

        types.Add(declaration);
    }

    private static void AddNamespaceFunction(
        Dictionary<NamespaceMemberFunctionLookupKey, ImmutableArray<FunctionStatementSyntax>.Builder> namespaceFunctions,
        string namespaceMetadataName,
        FunctionStatementSyntax functionStatement)
    {
        var name = functionStatement.Identifier.ValueText;
        if (string.IsNullOrWhiteSpace(name))
            return;

        var key = new NamespaceMemberFunctionLookupKey(namespaceMetadataName, name);
        if (!namespaceFunctions.TryGetValue(key, out var functions))
        {
            functions = ImmutableArray.CreateBuilder<FunctionStatementSyntax>();
            namespaceFunctions.Add(key, functions);
        }

        functions.Add(functionStatement);
    }

    private static string GetCompilationUnitNamespace(CompilationUnitSyntax compilationUnit)
        => compilationUnit.Members.OfType<FileScopedNamespaceDeclarationSyntax>().FirstOrDefault()?.Name.ToString()
           ?? string.Empty;

    private static string GetNamespaceDeclarationMetadataName(
        string parentNamespaceMetadataName,
        BaseNamespaceDeclarationSyntax namespaceDeclaration)
    {
        var declaredName = namespaceDeclaration.Name.ToString();
        if (namespaceDeclaration is FileScopedNamespaceDeclarationSyntax ||
            string.IsNullOrWhiteSpace(parentNamespaceMetadataName))
        {
            return declaredName;
        }

        return parentNamespaceMetadataName + "." + declaredName;
    }

    private bool IsFileScopeLocalFunction(GlobalStatementSyntax globalStatement)
        => globalStatement.Ancestors().OfType<CompilationUnitSyntax>().FirstOrDefault() is { } compilationUnit &&
           _compilation.HasRunnableFileScopeCode(compilationUnit);

    private readonly record struct NamespaceMemberFunctionLookupKey(string NamespaceMetadataName, string Name);

    private readonly record struct NamespaceTypeLookupKey(string NamespaceMetadataName, string Name);
}

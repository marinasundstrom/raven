using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class SourceDeclarationIndex
{
    private readonly Compilation _compilation;
    private readonly Dictionary<NamespaceMemberFunctionLookupKey, ImmutableArray<FunctionStatementSyntax>> _namespaceFunctions;

    public SourceDeclarationIndex(Compilation compilation, IEnumerable<SyntaxTree> syntaxTrees)
    {
        _compilation = compilation;
        var namespaceFunctions = new Dictionary<NamespaceMemberFunctionLookupKey, ImmutableArray<FunctionStatementSyntax>.Builder>();

        foreach (var syntaxTree in syntaxTrees)
        {
            if (syntaxTree.GetRoot() is CompilationUnitSyntax compilationUnit)
                IndexContainer(compilationUnit, GetCompilationUnitNamespace(compilationUnit), namespaceFunctions);
        }

        _namespaceFunctions = namespaceFunctions.ToDictionary(
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

    private void IndexContainer(
        SyntaxNode containerNode,
        string namespaceMetadataName,
        Dictionary<NamespaceMemberFunctionLookupKey, ImmutableArray<FunctionStatementSyntax>.Builder> namespaceFunctions)
    {
        foreach (var member in containerNode.ChildNodes())
        {
            switch (member)
            {
                case BaseNamespaceDeclarationSyntax namespaceDeclaration:
                    IndexContainer(
                        namespaceDeclaration,
                        GetNamespaceDeclarationMetadataName(namespaceMetadataName, namespaceDeclaration),
                        namespaceFunctions);
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
}

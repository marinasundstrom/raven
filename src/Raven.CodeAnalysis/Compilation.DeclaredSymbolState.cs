using System.Collections.Concurrent;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private readonly ConcurrentDictionary<DeclaredSyntaxKey, SourceNamedTypeSymbol> _declaredTypeSymbols = new();
    private readonly ConcurrentDictionary<DeclaredTypeLookupKey, ConcurrentDictionary<DeclaredSyntaxKey, SourceNamedTypeSymbol>> _declaredTypeSymbolsByName = new();
    private readonly ConcurrentDictionary<DeclaredTypeLookupKey, ImmutableArray<SyntaxNode>> _declaredTypeDeclarationsByName = new();
    private readonly ConcurrentDictionary<DeclaredSyntaxKey, SourceNamedTypeSymbol> _classSymbols = new();
    private readonly ConcurrentDictionary<DeclaredSyntaxKey, SourceUnionSymbol> _unionSymbols = new();
    private readonly ConcurrentDictionary<DeclaredSyntaxKey, SourceUnionCaseTypeSymbol> _unionCaseSymbols = new();
    private readonly ConcurrentDictionary<DeclaredSyntaxKey, IMethodSymbol> _methodSymbols = new();
    private readonly ConcurrentDictionary<DeclaredSyntaxKey, IPropertySymbol> _propertySymbols = new();
    private readonly ConcurrentDictionary<DeclaredSyntaxKey, IEventSymbol> _eventSymbols = new();

    internal void RegisterDeclaredTypeSymbol(SyntaxNode node, SourceNamedTypeSymbol symbol)
    {
        var syntaxKey = CreateDeclaredSyntaxKey(node);
        _declaredTypeSymbols[syntaxKey] = symbol;

        var lookupKey = CreateDeclaredTypeLookupKey(node, symbol);
        _declaredTypeSymbolsByName
            .GetOrAdd(lookupKey, static _ => new ConcurrentDictionary<DeclaredSyntaxKey, SourceNamedTypeSymbol>())
            [syntaxKey] = symbol;
    }

    internal bool TryGetDeclaredTypeSymbol(SyntaxNode node, out SourceNamedTypeSymbol symbol)
        => _declaredTypeSymbols.TryGetValue(CreateDeclaredSyntaxKey(node), out symbol!);

    internal bool TryGetDeclaredTypeDeclaration(string name, int arity, out SyntaxNode declaration)
    {
        declaration = null!;

        if (string.IsNullOrWhiteSpace(name))
            return false;

        var candidates = _declaredTypeDeclarationsByName.GetOrAdd(
            new DeclaredTypeLookupKey(name, arity),
            static (key, compilation) => compilation.FindDeclaredTypeDeclarations(key),
            this);
        if (candidates.IsDefaultOrEmpty)
            return false;

        declaration = candidates[0];
        return true;
    }

    internal bool TryGetDeclaredTypeSymbol(string name, int arity, out SourceNamedTypeSymbol symbol)
    {
        symbol = null!;

        if (string.IsNullOrWhiteSpace(name) ||
            !_declaredTypeSymbolsByName.TryGetValue(new DeclaredTypeLookupKey(name, arity), out var candidates))
        {
            return false;
        }

        SourceNamedTypeSymbol? match = null;
        foreach (var candidate in candidates.Values)
        {
            if (!MatchesArity(candidate, arity))
                continue;

            if (match is null)
            {
                match = candidate;
                continue;
            }

            if (!SymbolEqualityComparer.Default.Equals(match, candidate))
                return false;
        }

        if (match is null)
            return false;

        symbol = match;
        return true;
    }

    internal void RegisterClassSymbol(TypeDeclarationSyntax node, SourceNamedTypeSymbol symbol)
        => _classSymbols[CreateDeclaredSyntaxKey(node)] = symbol;

    internal SourceNamedTypeSymbol GetClassSymbol(TypeDeclarationSyntax node)
        => _classSymbols[CreateDeclaredSyntaxKey(node)];

    internal bool TryGetClassSymbol(TypeDeclarationSyntax node, out SourceNamedTypeSymbol symbol)
        => _classSymbols.TryGetValue(CreateDeclaredSyntaxKey(node), out symbol!);

    internal void RegisterUnionSymbol(UnionDeclarationSyntax node, SourceUnionSymbol symbol)
        => _unionSymbols[CreateDeclaredSyntaxKey(node)] = symbol;

    internal SourceUnionSymbol GetUnionSymbol(UnionDeclarationSyntax node)
        => _unionSymbols[CreateDeclaredSyntaxKey(node)];

    internal bool TryGetUnionSymbol(UnionDeclarationSyntax node, out SourceUnionSymbol symbol)
        => _unionSymbols.TryGetValue(CreateDeclaredSyntaxKey(node), out symbol!);

    internal void RegisterUnionCaseSymbol(CaseDeclarationSyntax node, SourceUnionCaseTypeSymbol symbol)
        => _unionCaseSymbols[CreateDeclaredSyntaxKey(node)] = symbol;

    internal SourceUnionCaseTypeSymbol GetUnionCaseSymbol(CaseDeclarationSyntax node)
        => _unionCaseSymbols[CreateDeclaredSyntaxKey(node)];

    internal bool TryGetUnionCaseSymbol(CaseDeclarationSyntax node, out SourceUnionCaseTypeSymbol symbol)
        => _unionCaseSymbols.TryGetValue(CreateDeclaredSyntaxKey(node), out symbol!);

    internal void RegisterMethodSymbol(MethodDeclarationSyntax node, IMethodSymbol symbol)
        => _methodSymbols[CreateDeclaredSyntaxKey(node)] = symbol;

    internal bool TryGetMethodSymbol(MethodDeclarationSyntax node, out IMethodSymbol symbol)
        => _methodSymbols.TryGetValue(CreateDeclaredSyntaxKey(node), out symbol!);

    internal void RegisterMethodSymbol(ConstructorDeclarationSyntax node, IMethodSymbol symbol)
        => _methodSymbols[CreateDeclaredSyntaxKey(node)] = symbol;

    internal bool TryGetMethodSymbol(ConstructorDeclarationSyntax node, out IMethodSymbol symbol)
        => _methodSymbols.TryGetValue(CreateDeclaredSyntaxKey(node), out symbol!);

    internal void RegisterMethodSymbol(ParameterlessConstructorDeclarationSyntax node, IMethodSymbol symbol)
        => _methodSymbols[CreateDeclaredSyntaxKey(node)] = symbol;

    internal bool TryGetMethodSymbol(ParameterlessConstructorDeclarationSyntax node, out IMethodSymbol symbol)
        => _methodSymbols.TryGetValue(CreateDeclaredSyntaxKey(node), out symbol!);

    internal void RegisterMethodSymbol(FunctionStatementSyntax node, IMethodSymbol symbol)
        => _methodSymbols[CreateDeclaredSyntaxKey(node)] = symbol;

    internal bool TryGetMethodSymbol(FunctionStatementSyntax node, out IMethodSymbol symbol)
        => _methodSymbols.TryGetValue(CreateDeclaredSyntaxKey(node), out symbol!);

    internal void RegisterPropertySymbol(PropertyDeclarationSyntax node, IPropertySymbol symbol)
        => _propertySymbols[CreateDeclaredSyntaxKey(node)] = symbol;

    internal bool TryGetPropertySymbol(PropertyDeclarationSyntax node, out IPropertySymbol symbol)
        => _propertySymbols.TryGetValue(CreateDeclaredSyntaxKey(node), out symbol!);

    internal void RegisterEventSymbol(EventDeclarationSyntax node, IEventSymbol symbol)
        => _eventSymbols[CreateDeclaredSyntaxKey(node)] = symbol;

    internal bool TryGetEventSymbol(EventDeclarationSyntax node, out IEventSymbol symbol)
        => _eventSymbols.TryGetValue(CreateDeclaredSyntaxKey(node), out symbol!);

    private static DeclaredSyntaxKey CreateDeclaredSyntaxKey(SyntaxNode node)
        => new(node.SyntaxTree, node.Span, node.Kind);

    private ImmutableArray<SyntaxNode> FindDeclaredTypeDeclarations(DeclaredTypeLookupKey lookupKey)
    {
        var builder = ImmutableArray.CreateBuilder<SyntaxNode>();

        foreach (var syntaxTree in _syntaxTrees)
        {
            if (syntaxTree.GetRoot() is not CompilationUnitSyntax root)
                continue;

            foreach (var declaration in root.DescendantNodes().OfType<SyntaxNode>())
            {
                if (!TryGetDeclaredTypeNameAndArity(declaration, out var name, out var arity) ||
                    arity != lookupKey.Arity ||
                    !string.Equals(name, lookupKey.Name, StringComparison.Ordinal))
                {
                    continue;
                }

                builder.Add(declaration);
            }
        }

        return builder.ToImmutable();
    }

    private static DeclaredTypeLookupKey CreateDeclaredTypeLookupKey(SyntaxNode node, SourceNamedTypeSymbol symbol)
        => new(symbol.Name, GetDeclaredTypeArity(node, symbol));

    private static int GetDeclaredTypeArity(SyntaxNode node, SourceNamedTypeSymbol symbol)
        => TryGetDeclaredTypeArity(node, out var arity)
            ? arity
            : symbol.TypeParameters.IsDefault ? symbol.Arity : symbol.TypeParameters.Length;

    private static bool TryGetDeclaredTypeArity(SyntaxNode node, out int arity)
    {
        arity = node switch
        {
            ClassDeclarationSyntax declaration => declaration.TypeParameterList?.Parameters.Count ?? 0,
            RecordDeclarationSyntax declaration => declaration.TypeParameterList?.Parameters.Count ?? 0,
            StructDeclarationSyntax declaration => declaration.TypeParameterList?.Parameters.Count ?? 0,
            InterfaceDeclarationSyntax declaration => declaration.TypeParameterList?.Parameters.Count ?? 0,
            ExtensionDeclarationSyntax declaration => declaration.TypeParameterList?.Parameters.Count ?? 0,
            UnionDeclarationSyntax declaration => declaration.TypeParameterList?.Parameters.Count ?? 0,
            DelegateDeclarationSyntax declaration => declaration.TypeParameterList?.Parameters.Count ?? 0,
            EnumDeclarationSyntax => 0,
            _ => -1
        };

        return arity >= 0;
    }

    private static int GetDeclaredTypeArity(SyntaxNode node)
        => node switch
        {
            ClassDeclarationSyntax declaration => declaration.TypeParameterList?.Parameters.Count ?? 0,
            RecordDeclarationSyntax declaration => declaration.TypeParameterList?.Parameters.Count ?? 0,
            StructDeclarationSyntax declaration => declaration.TypeParameterList?.Parameters.Count ?? 0,
            InterfaceDeclarationSyntax declaration => declaration.TypeParameterList?.Parameters.Count ?? 0,
            ExtensionDeclarationSyntax declaration => declaration.TypeParameterList?.Parameters.Count ?? 0,
            UnionDeclarationSyntax declaration => declaration.TypeParameterList?.Parameters.Count ?? 0,
            DelegateDeclarationSyntax declaration => declaration.TypeParameterList?.Parameters.Count ?? 0,
            EnumDeclarationSyntax => 0,
            _ => 0
        };

    private static bool TryGetDeclaredTypeNameAndArity(SyntaxNode node, out string name, out int arity)
    {
        switch (node)
        {
            case BaseTypeDeclarationSyntax declaration:
                name = declaration.Identifier.ValueText;
                arity = GetDeclaredTypeArity(declaration);
                return !string.IsNullOrWhiteSpace(name);
            case DelegateDeclarationSyntax declaration:
                name = declaration.Identifier.ValueText;
                arity = declaration.TypeParameterList?.Parameters.Count ?? 0;
                return !string.IsNullOrWhiteSpace(name);
            case ExtensionDeclarationSyntax declaration:
                name = declaration.Identifier.ValueText;
                arity = declaration.TypeParameterList?.Parameters.Count ?? 0;
                return !string.IsNullOrWhiteSpace(name);
            default:
                name = string.Empty;
                arity = 0;
                return false;
        }
    }

    private static bool MatchesArity(SourceNamedTypeSymbol symbol, int arity)
        => symbol.Arity == arity ||
           (!symbol.TypeParameters.IsDefault && symbol.TypeParameters.Length == arity);

    private readonly record struct DeclaredSyntaxKey(SyntaxTree SyntaxTree, Text.TextSpan Span, SyntaxKind Kind);

    private readonly record struct DeclaredTypeLookupKey(string Name, int Arity);
}

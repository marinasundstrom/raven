using System.Collections.Immutable;
using System.Reflection;
using System.Reflection.Metadata;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    private readonly DiagnosticBag _diagnostics;
    private readonly List<ISymbol> _symbols = new List<ISymbol>();
    private readonly List<ISymbol> _localSymbols = new List<ISymbol>();
    private readonly Dictionary<SyntaxNode, SymbolInfo> _bindings = new();
    private readonly Dictionary<TypeSyntax, INamespaceSymbol> _imports = new();
    private readonly Dictionary<string, ITypeSymbol> _keywordTypeSymbols = new();

    public SemanticModel(Compilation compilation, List<ISymbol> symbols, SyntaxTree syntaxTree)
    {
        _symbols = symbols;
        _diagnostics = new DiagnosticBag();
        Compilation = compilation;
        SyntaxTree = syntaxTree;

        _keywordTypeSymbols = new Dictionary<string, ITypeSymbol>
        {
            { "int", Compilation.GetSpecialType(SpecialType.System_Int32) },
            { "bool", Compilation.GetSpecialType(SpecialType.System_Boolean) },
            { "char", Compilation.GetSpecialType(SpecialType.System_Char) },
            { "string", Compilation.GetSpecialType(SpecialType.System_String) }
        };

        CreateModel();
    }

    private void CreateModel()
    {
        foreach (var symbol in _symbols.OfType<SourceMethodSymbol>())
        {
            var syntaxRef = symbol.DeclaringSyntaxReferences.First();
            var syntax = syntaxRef.GetSyntax();

            if (syntax is MethodDeclarationSyntax methodDeclaration)
            {

            }
            else if (syntax is CompilationUnitSyntax compilationUnit)
            {
                foreach (var import in compilationUnit.Imports)
                {
                    var namespaceSymbol = Compilation.GetNamespaceSymbol(import.NamespaceOrType.ToString());
                    _imports.Add(import.NamespaceOrType, namespaceSymbol);
                }

                foreach (var member in compilationUnit.Members)
                {
                    if (member is GlobalStatementSyntax globalStatement)
                    {
                        AnalyzeStatement(symbol, globalStatement.Statement);
                    }
                }
            }
        }
    }

    public Compilation Compilation { get; }

    public SyntaxTree SyntaxTree { get; }

    private DiagnosticBag Diagnostics => _diagnostics;

    public IImmutableList<Diagnostic> GetDiagnostics(CancellationToken cancellationToken = default) => _diagnostics.ToImmutableArray();

    public SymbolInfo GetSymbolInfo(SyntaxNode node, CancellationToken cancellationToken = default)
    {
        if (_bindings.TryGetValue(node, out var symbolInfo))
        {
            return symbolInfo;
        }

        return new SymbolInfo(CandidateReason.None, ImmutableArray<ISymbol>.Empty);
    }

    public ISymbol? GetDeclaredSymbol(SyntaxNode node)
    {
        return _symbols.Concat(_localSymbols).FirstOrDefault(x => x.DeclaringSyntaxReferences.Any(x2 => x2.GetSyntax() == node));
    }

    public ImmutableArray<ISymbol> LookupSymbols(int position,
        INamespaceOrTypeSymbol container, string name, bool includeReducedExtensionMethods)
    {
        throw new NotImplementedException();
    }

    private void Bind(SyntaxNode node, ISymbol symbol)
    {
        _bindings[node] = new SymbolInfo(symbol);
    }

    private void Bind(SyntaxNode node, CandidateReason reason, params IEnumerable<ISymbol> symbols)
    {
        _bindings[node] = new SymbolInfo(reason, symbols.ToImmutableArray());
    }

    public TypeInfo GetTypeInfo(SyntaxNode node, CancellationToken cancellationToken = default)
    {
        // Get the symbol associated with the syntax node
        var symbolInfo = GetSymbolInfo(node).Symbol;

        // If a symbol is found, attempt to determine its type
        if (symbolInfo is not null)
        {
            // Handle specific types of symbols
            if (symbolInfo is ITypeSymbol typeSymbol)
            {
                // Directly return the TypeInfo for type symbols
                return new TypeInfo(typeSymbol, null);
            }
            else if (symbolInfo is IMethodSymbol methodSymbol)
            {
                // Return the return type of the method
                return new TypeInfo(methodSymbol.ReturnType, null);
            }
            else if (symbolInfo is IPropertySymbol propertySymbol)
            {
                // Return the type of the property
                return new TypeInfo(propertySymbol.Type, null);
            }
            else if (symbolInfo is IFieldSymbol fieldSymbol)
            {
                // Return the type of the field
                return new TypeInfo(fieldSymbol.Type, null);
            }
        }

        // Handle cases where no symbol is found
        if (node is LiteralExpressionSyntax literalExpression)
        {
            // Infer the type of the literal
            var literalType = InferLiteralType(literalExpression);
            return new TypeInfo(literalType, null);
        }

        // If no type could be determined, return null TypeInfo
        return new TypeInfo(null, null);
    }

    private ITypeSymbol? InferLiteralType(LiteralExpressionSyntax literal)
    {
        // Example inference logic for literals
        return literal.Token.Value switch
        {
            int => GetTypeSymbol("System.Int32"),
            string => GetTypeSymbol("System.String"),
            double => GetTypeSymbol("System.Double"),
            bool => GetTypeSymbol("System.Boolean"),
            _ => null
        };
    }

    private ITypeSymbol? GetTypeSymbol(string typeName)
    {
        return _symbols
            .OfType<ITypeSymbol>()
            .FirstOrDefault(x => x.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat) == typeName);
    }
}

internal class ErrorTypeSymbol : Symbol, IErrorTypeSymbol
{
    public ErrorTypeSymbol(string name, ISymbol containingSymbol, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.ErrorType, name, containingSymbol, null, null, locations, declaringSyntaxReferences)
    {
    }

    public ImmutableArray<IMethodSymbol> Constructors => [];

    public IMethodSymbol? StaticConstructor => null;

    public ImmutableArray<ITypeSymbol> TypeArguments => [];

    public ImmutableArray<ITypeParameterSymbol> TypeParameters => [];

    public SpecialType SpecialType => SpecialType.None;

    public bool IsValueType => false;

    public bool IsNamespace => false;

    public bool IsType => false;

    public INamedTypeSymbol? BaseType => throw new NotImplementedException();

    public ImmutableArray<ISymbol> GetMembers() => [];

    public ImmutableArray<ISymbol> GetMembers(string name) => [];
}

internal interface IErrorTypeSymbol : INamedTypeSymbol
{
}

public class TypeInfo
{
    internal TypeInfo(ITypeSymbol type, ITypeSymbol? convertedType)
    {
        Type = type;
        ConvertedType = convertedType;
    }

    public NullabilityInfo ConvertedNullability { get; }

    public ITypeSymbol? ConvertedType { get; }

    public NullabilityInfo Nullability { get; }

    public ITypeSymbol? Type { get; }
}

[System.Diagnostics.DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public readonly struct NullabilityInfo : IEquatable<NullabilityInfo>
{
    public NullableAnnotation Annotation { get; }

    public NullableFlowState FlowState { get; }

    public bool Equals(NullabilityInfo other)
    {
        throw new NotImplementedException();
    }
}

public enum NullableAnnotation
{
    None,
    NotAnnotated,
    Annotated
}

public enum NullableFlowState
{
    None,
    NotNull,
    MaybeNull
}

public static class SymbolExtensions
{
    /// <summary>
    /// Unwraps the actual type of a property symbol or local symbol. 
    /// If the symbol is a type symbol, then return it, otherwise null.
    /// </summary>
    /// <param name="symbol"></param>
    /// <returns></returns>
    public static ITypeSymbol? UnwrapType(this ISymbol symbol)
    {
        if (symbol is IPropertySymbol propertySymbol) return propertySymbol.Type;

        if (symbol is IFieldSymbol fieldSymbol) return fieldSymbol.Type;

        if (symbol is ILocalSymbol localSymbol) return localSymbol.Type;

        return symbol as ITypeSymbol;
    }
}
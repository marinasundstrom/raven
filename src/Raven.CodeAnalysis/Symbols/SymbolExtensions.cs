using System.Linq;
using System.Text;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public static partial class SymbolExtensions
{
    private static readonly Dictionary<string, string> s_specialTypeNames = new()
    {
        ["System.Object"] = "object",
        ["System.String"] = "string",
        ["System.Boolean"] = "bool",
        ["System.Byte"] = "byte",
        ["System.SByte"] = "sbyte",
        ["System.Int16"] = "short",
        ["System.UInt16"] = "ushort",
        ["System.Int32"] = "int",
        ["System.UInt32"] = "uint",
        ["System.Int64"] = "long",
        ["System.UInt64"] = "ulong",
        ["System.Single"] = "float",
        ["System.Double"] = "double",
        ["System.Decimal"] = "decimal",
        ["System.Char"] = "char",
        ["System.Void"] = "void",
        ["System.Unit"] = "unit"
    };

    public static string ToDisplayStringKeywordAware(this ITypeSymbol typeSymbol, SymbolDisplayFormat format)
    {
        if (typeSymbol is IArrayTypeSymbol arrayType)
        {
            var elementType = arrayType.ElementType;

            if (arrayType.Rank > 1)
            {
                return elementType + "[" + new string(',', arrayType.Rank - 1) + "]";
            }

            return elementType.ToDisplayStringKeywordAware(format) + "[]";
        }

        /*
        if (typeSymbol is IPointerTypeSymbol pointerType)
        {
            return pointerType.PointedAtType.ToDisplayStringKeywordAware(format) + "*";
        } */

        if (format.MiscellaneousOptions.HasFlag(SymbolDisplayMiscellaneousOptions.UseSpecialTypes))
        {
            if (typeSymbol.SpecialType == SpecialType.System_Unit)
                return "unit";

            var fullName = typeSymbol.ToFullyQualifiedMetadataName(); // e.g. "System.Int32"
            if (fullName is not null && s_specialTypeNames.TryGetValue(fullName, out var keyword))
                return keyword;
        }

        return typeSymbol.ToDisplayString(format);
    }

    public static string ToDisplayStringForDiagnostics(this ITypeSymbol typeSymbol, SymbolDisplayFormat format)
    {
        if (typeSymbol is LiteralTypeSymbol literal)
            typeSymbol = literal.UnderlyingType;

        return typeSymbol.ToDisplayStringKeywordAware(format);
    }

    public static string ToDisplayString(this ISymbol symbol, SymbolDisplayFormat? format = default!)
    {
        format ??= SymbolDisplayFormat.CSharpErrorMessageFormat;

        var result = new StringBuilder();

        if (symbol is ILocalSymbol localSymbol)
        {
            if (format.LocalOptions.HasFlag(SymbolDisplayLocalOptions.IncludeType))
            {
                var typeFormat = WithoutTypeAccessibility(format);
                var localType = localSymbol.Type.ToDisplayStringKeywordAware(typeFormat);
                result.Append($"{localType} ");
            }

            result.Append(localSymbol.Name);
            return result.ToString();
        }

        if (symbol is IParameterSymbol parameterSymbol)
        {
            if (format.ParameterOptions.HasFlag(SymbolDisplayParameterOptions.IncludeType))
            {
                var typeFormat = WithoutTypeAccessibility(format);
                var localType = parameterSymbol.Type.ToDisplayStringKeywordAware(typeFormat);
                result.Append($"{localType} ");
            }

            result.Append(parameterSymbol.Name);
            return result.ToString();
        }

        // Example: Include namespace and type qualification
        if (format.TypeQualificationStyle == SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces)
        {
            // Assume `Namespace` and `ContainingType` are properties of the symbol
            if (symbol.ContainingNamespace?.ContainingNamespace is not null)
            {
                var ns = GetFullNamespace(symbol);
                result.Append(ns).Append(".");
            }

            if (symbol.ContainingType is not null)
            {
                var type = GetFullType(symbol);
                result.Append(type).Append(".");
            }
        }

        // Append the symbol's name
        result.Append(symbol.Name); // Assume `Name` is a property of the symbol   

        if (symbol is INamedTypeSymbol typeSymbol)
        {
            // Handle generics
            if (format.GenericsOptions.HasFlag(SymbolDisplayGenericsOptions.IncludeTypeParameters) &&
                typeSymbol.TypeParameters != null && !typeSymbol.TypeParameters.IsEmpty)
            {
                result.Append("<");
                result.Append(string.Join(", ",
                    typeSymbol.TypeParameters)); // Assume `TypeParameters` is a list of generic type names
                result.Append(">");
            }
        }

        if (symbol is IMethodSymbol methodSymbol)
        {
            // Return type (if requested)
            if (format.MemberOptions.HasFlag(SymbolDisplayMemberOptions.IncludeType))
            {
                var returnType = methodSymbol.ReturnType?.ToDisplayStringKeywordAware(format);
                if (!string.IsNullOrEmpty(returnType))
                {
                    result.Insert(0, returnType + " ");
                }
            }

            // Handle method parameters (if the symbol is a method)
            if (format.DelegateStyle == SymbolDisplayDelegateStyle.NameAndSignature)
            {
                result.Append("(");
                result.Append(string.Join(", ", methodSymbol.Parameters.Select(p => FormatParameter(p, format))));
                result.Append(")");
            }
        }

        // Example for accessibility modifiers
        if (format.MemberOptions.HasFlag(SymbolDisplayMemberOptions.IncludeAccessibility))
        {
            if (symbol.DeclaredAccessibility is not Accessibility.NotApplicable)
            {
                result.Insert(0,
                    symbol.DeclaredAccessibility.ToString().ToLower() + " "); // Assume `Accessibility` is a property
            }
        }

        // Handle miscellaneous options
        if (format.MiscellaneousOptions.HasFlag(SymbolDisplayMiscellaneousOptions.EscapeIdentifiers))
        {
            return EscapeIdentifier(result.ToString());
        }

        return result.ToString();
    }

    private static SymbolDisplayFormat WithoutTypeAccessibility(SymbolDisplayFormat format)
    {
        return format.WithMemberOptions(format.MemberOptions & ~SymbolDisplayMemberOptions.IncludeAccessibility);
    }

    // Helper method to format a parameter
    private static string FormatParameter(IParameterSymbol parameter, SymbolDisplayFormat format)
    {
        var sb = new StringBuilder();

        if (format.ParameterOptions.HasFlag(SymbolDisplayParameterOptions.IncludeModifiers))
        {
            //if (parameter.IsRef) sb.Append("ref ");
            //if (parameter.IsOut) sb.Append("out ");
        }

        if (format.ParameterOptions.HasFlag(SymbolDisplayParameterOptions.IncludeType))
        {
            sb.Append(parameter.Type.ToDisplayStringKeywordAware(format));
            sb.Append(" ");
        }

        if (format.ParameterOptions.HasFlag(SymbolDisplayParameterOptions.IncludeName))
        {
            sb.Append(parameter.Name); // Assume `Name` is a property of the parameter
        }

        return sb.ToString();
    }

    // Helper method to escape identifiers
    private static string EscapeIdentifier(string identifier)
    {
        // Replace any special characters or keywords with escaped versions
        return identifier; //identifier.Replace("<", "&lt;").Replace(">", "&gt;");
    }

    private static string GetFullNamespace(ISymbol symbol)
    {
        var namespaces = new List<string>();
        var currentNamespace = symbol.ContainingNamespace;

        // Traverse all containing namespaces
        while (currentNamespace is not null && !currentNamespace.IsGlobalNamespace)
        {
            namespaces.Insert(0, currentNamespace.Name);
            currentNamespace = currentNamespace.ContainingNamespace;
        }

        // Join namespaces with '.'
        return string.Join(".", namespaces);
    }

    private static string GetFullType(ISymbol symbol)
    {
        var types = new List<string>();
        var currentType = symbol.ContainingType;

        // Traverse all containing types
        while (currentType is not null)
        {
            types.Insert(0, currentType.Name);
            currentType = currentType.ContainingType;
        }

        // Join types with '.'
        return string.Join(".", types);
    }

    public static IMethodSymbol? GetDelegateInvokeMethod(this INamedTypeSymbol typeSymbol)
    {
        if (typeSymbol.TypeKind != TypeKind.Delegate)
            return null;

        return typeSymbol
            .GetMembers("Invoke")
            .OfType<IMethodSymbol>()
            .FirstOrDefault();
    }
}

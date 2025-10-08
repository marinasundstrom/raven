using System.Collections.Immutable;
using System.Linq;
using System.Text;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

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
        if (TryFormatFunctionType(typeSymbol, format, out var functionDisplay))
            return functionDisplay;

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

    public static string ToDisplayStringForTypeMismatchDiagnostic(this ITypeSymbol typeSymbol, SymbolDisplayFormat format)
    {
        var display = typeSymbol.ToDisplayStringKeywordAware(format);

        if (display.IndexOf('\'') >= 0 || display.IndexOf('"') >= 0)
            return display;

        return $"'{display}'";
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

            result.Append(EscapeIdentifierIfNeeded(localSymbol.Name, format));
            return result.ToString();
        }

        if (symbol is ILabelSymbol labelSymbol)
        {
            result.Append(EscapeIdentifierIfNeeded(labelSymbol.Name, format));
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

            result.Append(EscapeIdentifierIfNeeded(parameterSymbol.Name, format));
            return result.ToString();
        }

        // Example: Include namespace and type qualification
        if (format.TypeQualificationStyle == SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces)
        {
            // Append containing namespaces when present (excluding the global namespace)
            if (symbol.ContainingNamespace is { } containingNamespace && !containingNamespace.IsGlobalNamespace)
            {
                var ns = GetFullNamespace(symbol, format);
                if (!string.IsNullOrEmpty(ns))
                {
                    result.Append(ns).Append(".");
                }
            }

            if (symbol.ContainingType is not null)
            {
                var type = GetFullType(symbol, format);
                if (!string.IsNullOrEmpty(type))
                {
                    result.Append(type).Append(".");
                }
            }
        }

        // Append the symbol's name
        result.Append(EscapeIdentifierIfNeeded(symbol.Name, format));

        if (symbol is INamedTypeSymbol typeSymbol)
        {
            // Handle generics
            if (format.GenericsOptions.HasFlag(SymbolDisplayGenericsOptions.IncludeTypeParameters) &&
                typeSymbol.TypeParameters != null && !typeSymbol.TypeParameters.IsEmpty)
            {
                result.Append("<");
                result.Append(string.Join(", ",
                    typeSymbol.TypeParameters.Select(p => EscapeIdentifierIfNeeded(p.Name, format))));
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

    private static bool TryFormatFunctionType(ITypeSymbol typeSymbol, SymbolDisplayFormat format, out string display)
    {
        if (typeSymbol is INamedTypeSymbol named && named.TypeKind == TypeKind.Delegate)
        {
            if (named is SynthesizedDelegateTypeSymbol synthesized)
            {
                display = FormatFunctionSignature(
                    synthesized.ParameterTypes,
                    synthesized.ParameterRefKinds,
                    synthesized.ReturnType,
                    format);
                return true;
            }

            if (IsSystemFuncOrAction(named))
            {
                var invoke = named.GetDelegateInvokeMethod();
                if (invoke is null)
                {
                    display = null!;
                    return false;
                }

                var parameterTypes = invoke.Parameters.Select(p => p.Type).ToImmutableArray();
                var refKinds = invoke.Parameters.Select(p => p.RefKind).ToImmutableArray();

                display = FormatFunctionSignature(parameterTypes, refKinds, invoke.ReturnType, format);
                return true;
            }
        }

        display = null!;
        return false;
    }

    private static string FormatFunctionSignature(
        ImmutableArray<ITypeSymbol> parameterTypes,
        ImmutableArray<RefKind> refKinds,
        ITypeSymbol returnType,
        SymbolDisplayFormat format)
    {
        var parameterDisplays = ImmutableArray.CreateBuilder<string>(parameterTypes.IsDefault ? 0 : parameterTypes.Length);

        if (!parameterTypes.IsDefaultOrEmpty)
        {
            for (var i = 0; i < parameterTypes.Length; i++)
            {
                var parameterType = parameterTypes[i];
                var refKind = !refKinds.IsDefaultOrEmpty && i < refKinds.Length
                    ? refKinds[i]
                    : RefKind.None;

                parameterDisplays.Add(FormatFunctionParameter(parameterType, refKind, format));
            }
        }

        string parameterText = parameterDisplays.Count switch
        {
            0 => "()",
            1 => parameterDisplays[0],
            _ => $"({string.Join(", ", parameterDisplays)})"
        };

        var returnDisplay = returnType.ToDisplayStringKeywordAware(format);
        return $"{parameterText} -> {returnDisplay}";
    }

    private static string FormatFunctionParameter(ITypeSymbol type, RefKind refKind, SymbolDisplayFormat format)
    {
        var typeDisplay = type.ToDisplayStringKeywordAware(format);

        return refKind switch
        {
            RefKind.In => $"in {typeDisplay}",
            RefKind.Ref => $"ref {typeDisplay}",
            RefKind.Out => $"out {typeDisplay}",
            RefKind.RefReadOnly => $"ref readonly {typeDisplay}",
            _ => typeDisplay
        };
    }

    private static bool IsSystemFuncOrAction(INamedTypeSymbol named)
    {
        if (named.Name is not ("Func" or "Action"))
            return false;

        var ns = named.ContainingNamespace;
        if (ns is null || ns.IsGlobalNamespace)
            return false;

        return ns.Name == "System" && (ns.ContainingNamespace is null || ns.ContainingNamespace.IsGlobalNamespace);
    }

    private static SymbolDisplayFormat WithoutTypeAccessibility(SymbolDisplayFormat format)
    {
        return format.WithMemberOptions(format.MemberOptions & ~SymbolDisplayMemberOptions.IncludeAccessibility);
    }

    private static string EscapeIdentifierIfNeeded(string identifier, SymbolDisplayFormat format)
    {
        if (string.IsNullOrEmpty(identifier))
        {
            return identifier;
        }

        var result = identifier;

        if (format.MiscellaneousOptions.HasFlag(SymbolDisplayMiscellaneousOptions.EscapeKeywordIdentifiers) &&
            SyntaxFacts.TryParseKeyword(identifier, out _))
        {
            result = "@" + result;
        }

        if (format.MiscellaneousOptions.HasFlag(SymbolDisplayMiscellaneousOptions.EscapeIdentifiers))
        {
            result = EscapeIdentifier(result);
        }

        return result;
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
            sb.Append(EscapeIdentifierIfNeeded(parameter.Name, format));
        }

        return sb.ToString();
    }

    // Helper method to escape identifiers
    private static string EscapeIdentifier(string identifier)
    {
        // Replace any special characters or keywords with escaped versions
        return identifier; //identifier.Replace("<", "&lt;").Replace(">", "&gt;");
    }

    private static string GetFullNamespace(ISymbol symbol, SymbolDisplayFormat format)
    {
        var namespaces = new List<string>();
        var currentNamespace = symbol.ContainingNamespace;

        // Traverse all containing namespaces
        while (currentNamespace is not null && !currentNamespace.IsGlobalNamespace)
        {
            namespaces.Insert(0, EscapeIdentifierIfNeeded(currentNamespace.Name, format));
            currentNamespace = currentNamespace.ContainingNamespace;
        }

        // Join namespaces with '.'
        return string.Join(".", namespaces);
    }

    private static string GetFullType(ISymbol symbol, SymbolDisplayFormat format)
    {
        var types = new List<string>();
        var currentType = symbol.ContainingType;

        // Traverse all containing types
        while (currentType is not null)
        {
            types.Insert(0, EscapeIdentifierIfNeeded(currentType.Name, format));
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

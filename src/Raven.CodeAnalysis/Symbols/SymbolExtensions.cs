using System;
using System.Collections.Immutable;
using System.Globalization;
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

    public static bool MetadataIdentityEquals(this ITypeSymbol? left, ITypeSymbol? right)
    {
        if (SymbolEqualityComparer.Default.Equals(left, right))
            return true;

        if (left is null || right is null)
            return false;

        var leftIdentity = left.ToDisplayString(SymbolDisplayFormat.CSharpSymbolKeyFormat);
        var rightIdentity = right.ToDisplayString(SymbolDisplayFormat.CSharpSymbolKeyFormat);
        return string.Equals(leftIdentity, rightIdentity, StringComparison.Ordinal);
    }

    /// <summary>
    /// Legacy name: now just delegates to the central type formatter.
    /// </summary>
    public static string ToDisplayStringKeywordAware(this ITypeSymbol typeSymbol, SymbolDisplayFormat format)
    {
        return FormatType(typeSymbol, format);
    }

    public static string ToDisplayStringForDiagnostics(this ITypeSymbol typeSymbol, SymbolDisplayFormat format)
    {
        if (typeSymbol is LiteralTypeSymbol literal)
            typeSymbol = literal.UnderlyingType;

        return FormatType(typeSymbol, format);
    }

    public static bool ContainsErrorType(this ITypeSymbol? typeSymbol)
    {
        if (typeSymbol is null)
            return false;

        if (typeSymbol is IErrorTypeSymbol)
            return true;

        if (typeSymbol is LiteralTypeSymbol literal)
            return literal.UnderlyingType.ContainsErrorType();

        if (typeSymbol is NullableTypeSymbol nullable)
            return nullable.UnderlyingType.ContainsErrorType();

        switch (typeSymbol)
        {
            case IArrayTypeSymbol array:
                return array.ElementType.ContainsErrorType();

            case ByRefTypeSymbol byRef:
                return byRef.ElementType.ContainsErrorType();

            case IPointerTypeSymbol pointer:
                return pointer.PointedAtType.ContainsErrorType();

            case IAddressTypeSymbol address:
                return address.ReferencedType.ContainsErrorType();

            case IUnionTypeSymbol union:
                foreach (var member in union.Types)
                {
                    if (member.ContainsErrorType())
                        return true;
                }

                return false;

            case INamedTypeSymbol named:
                if (!named.TypeArguments.IsDefaultOrEmpty)
                {
                    foreach (var argument in named.TypeArguments)
                    {
                        if (argument.ContainsErrorType())
                            return true;
                    }
                }

                if (named.TypeKind == TypeKind.Delegate &&
                    named.GetDelegateInvokeMethod() is { } invoke)
                {
                    if (invoke.ReturnType.ContainsErrorType())
                        return true;

                    foreach (var parameter in invoke.Parameters)
                    {
                        if (parameter.Type.ContainsErrorType())
                            return true;
                    }
                }

                return false;

            case ITypeParameterSymbol typeParameter:
                if (!typeParameter.ConstraintTypes.IsDefaultOrEmpty)
                {
                    foreach (var constraint in typeParameter.ConstraintTypes)
                    {
                        if (constraint.ContainsErrorType())
                            return true;
                    }
                }

                return false;
        }

        return false;
    }

    public static string ToDisplayStringForTypeMismatchDiagnostic(this ITypeSymbol typeSymbol, SymbolDisplayFormat format)
    {
        var display = FormatType(typeSymbol, format);

        if (display.IndexOf('\'') >= 0 || display.IndexOf('"') >= 0)
            return display;

        return $"'{display}'";
    }

    public static string ToDisplayString(this ISymbol symbol, SymbolDisplayFormat? format = default!)
    {
        format ??= SymbolDisplayFormat.CSharpErrorMessageFormat;

        // Single entry point for types
        if (symbol is ITypeSymbol typeSymbol)
            return FormatType(typeSymbol, format);

        var result = new StringBuilder();

        if (symbol is ILocalSymbol localSymbol)
        {
            var display = FormatNamedSymbol(
                localSymbol.Name,
                localSymbol.Type,
                format.LocalOptions.HasFlag(SymbolDisplayLocalOptions.IncludeType),
                format,
                useNameOption: true);
            result.Append(display);
            return result.ToString();
        }

        if (symbol is ILabelSymbol labelSymbol)
        {
            result.Append(EscapeIdentifierIfNeeded(labelSymbol.Name, format));
            return result.ToString();
        }

        if (symbol is IParameterSymbol parameterSymbol)
        {
            var display = FormatParameter(parameterSymbol, format);
            result.Append(display);
            return result.ToString();
        }

        if (symbol is IMethodSymbol methodSymbol2
            && !methodSymbol2.ExplicitInterfaceImplementations.IsEmpty)
        {
            if (format.MemberOptions.HasFlag(SymbolDisplayMemberOptions.IncludeExplicitInterface))
            {
                var t = methodSymbol2.ExplicitInterfaceImplementations[0];
                var type = GetFullType(t, format);
                if (!string.IsNullOrEmpty(type))
                {
                    result.Append(type).Append('.');
                }
            }
        }
        else if (symbol is IPropertySymbol propertySymbol2
            && !propertySymbol2.ExplicitInterfaceImplementations.IsEmpty)
        {
            if (format.MemberOptions.HasFlag(SymbolDisplayMemberOptions.IncludeExplicitInterface))
            {
                var t = propertySymbol2.ExplicitInterfaceImplementations[0];
                var type = GetFullType(t, format);
                if (!string.IsNullOrEmpty(type))
                {
                    result.Append(type).Append('.');
                }
            }
        }
        else
        {
            // Type qualification for non-type symbols (namespaces/types used as containers)
            if (format.TypeQualificationStyle == SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces)
            {
                if (symbol.ContainingNamespace is { } containingNamespace && !containingNamespace.IsGlobalNamespace)
                {
                    var ns = GetFullNamespace(symbol, format);
                    if (!string.IsNullOrEmpty(ns))
                    {
                        result.Append(ns).Append('.');
                    }
                }

                if (symbol.ContainingType is not null)
                {
                    var type = GetFullType(symbol, format);
                    if (!string.IsNullOrEmpty(type))
                    {
                        result.Append(type).Append('.');
                    }
                }
            }
            else if (format.TypeQualificationStyle == SymbolDisplayTypeQualificationStyle.NameAndContainingTypes)
            {
                if (symbol.ContainingType is not null)
                {
                    var type = GetFullType(symbol, format);
                    if (!string.IsNullOrEmpty(type))
                    {
                        result.Append(type).Append('.');
                    }
                }
            }
        }

        // Symbol name
        result.Append(EscapeIdentifierIfNeeded(symbol.Name, format));

        if (symbol is INamedTypeSymbol typeSymbol2)
        {
            if (format.GenericsOptions.HasFlag(SymbolDisplayGenericsOptions.IncludeTypeParameters) &&
                typeSymbol2.TypeParameters is { IsDefaultOrEmpty: false })
            {
                IEnumerable<string> arguments;

                if (!typeSymbol2.TypeArguments.IsDefaultOrEmpty &&
                    typeSymbol2.TypeArguments.Length == typeSymbol2.TypeParameters.Length)
                {
                    arguments = typeSymbol2.TypeArguments
                        .Select(arg => FormatType(arg, format));
                }
                else
                {
                    arguments = typeSymbol2.TypeParameters
                        .Select(p => EscapeIdentifierIfNeeded(p.Name, format));
                }

                result.Append('<');
                result.Append(string.Join(", ", arguments));
                result.Append('>');
            }
        }

        if (symbol is IMethodSymbol methodSymbol)
        {
            if (format.GenericsOptions.HasFlag(SymbolDisplayGenericsOptions.IncludeTypeParameters)
                && (!methodSymbol.TypeParameters.IsDefaultOrEmpty || !methodSymbol.TypeArguments.IsDefaultOrEmpty))
            {

                IEnumerable<string> arguments;

                if (!methodSymbol.TypeArguments.IsDefaultOrEmpty &&
                    methodSymbol.TypeArguments.Length == methodSymbol.TypeParameters.Length)
                {
                    arguments = methodSymbol.TypeArguments.Select(a => FormatType(a, format));
                }
                else
                {
                    arguments = methodSymbol.TypeParameters
                        .Select(p => EscapeIdentifierIfNeeded(p.Name, format));
                }

                result.Append('<');
                result.Append(string.Join(", ", arguments));
                result.Append('>');
            }

            if (format.DelegateStyle == SymbolDisplayDelegateStyle.NameAndSignature ||
                format.MemberOptions.HasFlag(SymbolDisplayMemberOptions.IncludeParameters))
            {
                result.Append('(');
                result.Append(string.Join(", ", methodSymbol.Parameters.Select(p => FormatParameter(p, format))));
                result.Append(')');
            }

            if (format.MemberOptions.HasFlag(SymbolDisplayMemberOptions.IncludeType))
            {
                var returnType = methodSymbol.ReturnType;
                if (returnType is not null)
                {
                    var returnDisplay = FormatType(returnType, format);
                    if (!string.IsNullOrEmpty(returnDisplay))
                    {
                        result.Append(" -> ");
                        result.Append(returnDisplay);
                    }
                }
            }
        }
        else if (symbol is IPropertySymbol propertySymbol)
        {
            if (format.MemberOptions.HasFlag(SymbolDisplayMemberOptions.IncludeType))
            {
                var typeDisplay = FormatType(propertySymbol.Type, format);
                result.Append(": ");
                result.Append(typeDisplay);
            }
        }
        else if (symbol is IFieldSymbol fieldSymbol)
        {
            if (format.MemberOptions.HasFlag(SymbolDisplayMemberOptions.IncludeType))
            {
                var typeDisplay = FormatType(fieldSymbol.Type, format);
                result.Append(": ");
                result.Append(typeDisplay);
            }
        }

        // Prefix: accessibility + modifiers
        string? accessibilityPrefix = null;
        if (format.MemberOptions.HasFlag(SymbolDisplayMemberOptions.IncludeAccessibility) &&
            symbol.DeclaredAccessibility is not Accessibility.NotApplicable)
        {
            accessibilityPrefix = symbol.DeclaredAccessibility.ToString().ToLower();
        }

        string? modifiersPrefix = null;
        if (format.MemberOptions.HasFlag(SymbolDisplayMemberOptions.IncludeModifiers))
        {
            modifiersPrefix = GetMemberModifiers(symbol);
        }

        if (!string.IsNullOrEmpty(accessibilityPrefix) || !string.IsNullOrEmpty(modifiersPrefix))
        {
            var prefixBuilder = new StringBuilder();

            if (!string.IsNullOrEmpty(accessibilityPrefix))
            {
                prefixBuilder.Append(accessibilityPrefix);
            }

            if (!string.IsNullOrEmpty(modifiersPrefix))
            {
                if (prefixBuilder.Length > 0)
                    prefixBuilder.Append(' ');

                prefixBuilder.Append(modifiersPrefix);
            }

            if (prefixBuilder.Length > 0)
            {
                prefixBuilder.Append(' ');
                result.Insert(0, prefixBuilder.ToString());
            }
        }

        if (format.MiscellaneousOptions.HasFlag(SymbolDisplayMiscellaneousOptions.EscapeIdentifiers))
        {
            return EscapeIdentifier(result.ToString());
        }

        return result.ToString();
    }

    // =========================
    //  Type formatting helpers
    // =========================

    private static string FormatType(ITypeSymbol typeSymbol, SymbolDisplayFormat format)
    {
        // Unwrap literal pseudo-types
        if (typeSymbol is LiteralTypeSymbol literal)
            return FormatType(literal.UnderlyingType, format);

        // Nullable<T> => T?
        if (typeSymbol is NullableTypeSymbol nullable)
        {
            var underlying = nullable.UnderlyingType;

            // Nullable of function type => (A -> B)?
            if (underlying is INamedTypeSymbol { TypeKind: TypeKind.Delegate } &&
                TryFormatFunctionType(underlying, format, out var funcDisplay))
            {
                return $"({funcDisplay})?";
            }

            var underlyingDisplay = FormatType(underlying, format);
            return underlyingDisplay + "?";
        }

        // Delegate function sugar (synthesized + System.Func/Action)
        if (TryFormatFunctionType(typeSymbol, format, out var functionDisplay))
            return functionDisplay;

        // Type parameter: just the (escaped) name
        if (typeSymbol is ITypeParameterSymbol typeParameter)
            return EscapeIdentifierIfNeeded(typeParameter.Name, format);

        // Arrays
        if (typeSymbol is IArrayTypeSymbol arrayType)
        {
            var elementDisplay = FormatType(arrayType.ElementType, format);

            if (arrayType.Rank == 1)
                return elementDisplay + "[]";

            return elementDisplay + "[" + new string(',', arrayType.Rank - 1) + "]";
        }

        // Pointers
        if (typeSymbol is IPointerTypeSymbol pointerType)
        {
            var pointedAt = FormatType(pointerType.PointedAtType, format);
            return "*" + pointedAt;
        }

        // ByRef
        if (typeSymbol is ByRefTypeSymbol byRefType)
        {
            var addressTo = FormatType(byRefType.ElementType, format);
            return "&" + addressTo;
        }

        // Tuples
        if (typeSymbol is ITupleTypeSymbol tupleType)
        {
            var elementTypes = tupleType.TupleElements
                .Select(e =>
                {
                    var t = FormatType(e.Type, format);
                    // Later: include element names if desired
                    return t;
                });

            return "(" + string.Join(", ", elementTypes) + ")";
        }

        // Unions (if you want pipe-style display)
        if (typeSymbol is IUnionTypeSymbol unionType)
        {
            var members = unionType.Types.Select(t => FormatType(t, format));
            return string.Join(" | ", members);
        }

        // Special types => keywords
        if (format.MiscellaneousOptions.HasFlag(SymbolDisplayMiscellaneousOptions.UseSpecialTypes))
        {
            if (typeSymbol.SpecialType == SpecialType.System_Unit)
                return "()";

            var fullName = typeSymbol.ToFullyQualifiedMetadataName();
            if (fullName is not null && s_specialTypeNames.TryGetValue(fullName, out var keyword))
                return keyword;
        }

        // Named types and everything else
        if (typeSymbol is INamedTypeSymbol namedType)
            return FormatNamedType(namedType, format);

        // Fallback: just the name
        return EscapeIdentifierIfNeeded(typeSymbol.Name, format);
    }

    private static string FormatSimpleNamedType(INamedTypeSymbol typeSymbol, SymbolDisplayFormat format)
    {
        var sb = new StringBuilder();

        // Simple name
        sb.Append(EscapeIdentifierIfNeeded(typeSymbol.Name, format));

        // Generic arguments / parameters
        if (format.GenericsOptions.HasFlag(SymbolDisplayGenericsOptions.IncludeTypeParameters) &&
            typeSymbol.Arity > 0)
        {
            IEnumerable<string> arguments;

            if (!typeSymbol.TypeArguments.IsDefaultOrEmpty &&
                typeSymbol.TypeArguments.Length == typeSymbol.TypeParameters.Length)
            {
                // Constructed type: use actual type arguments
                arguments = typeSymbol.TypeArguments.Select(a => FormatType(a, format));
            }
            else
            {
                // Unconstructed type: fall back to parameter names
                arguments = typeSymbol.TypeParameters
                    .Select(p => EscapeIdentifierIfNeeded(p.Name, format));
            }

            sb.Append('<');
            sb.Append(string.Join(", ", arguments));
            sb.Append('>');
        }

        return sb.ToString();
    }

    private static string FormatNamedType(INamedTypeSymbol typeSymbol, SymbolDisplayFormat format)
    {
        var sb = new StringBuilder();

        // Qualification
        if (format.TypeQualificationStyle == SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces)
        {
            if (typeSymbol.ContainingNamespace is { IsGlobalNamespace: false })
            {
                var ns = GetFullNamespace(typeSymbol, format);
                if (!string.IsNullOrEmpty(ns))
                {
                    sb.Append(ns).Append('.');
                }
            }

            if (typeSymbol.ContainingType is not null)
            {
                var containingTypes = GetFullType(typeSymbol, format);
                if (!string.IsNullOrEmpty(containingTypes))
                {
                    sb.Append(containingTypes).Append('.');
                }
            }
        }
        else if (format.TypeQualificationStyle == SymbolDisplayTypeQualificationStyle.NameAndContainingTypes)
        {
            if (typeSymbol.ContainingType is not null)
            {
                var containingTypes = GetFullType(typeSymbol, format);
                if (!string.IsNullOrEmpty(containingTypes))
                {
                    sb.Append(containingTypes).Append('.');
                }
            }
        }

        // Simple name + generic params/args
        sb.Append(FormatSimpleNamedType(typeSymbol, format));

        return sb.ToString();
    }

    // =========================
    //  Function type helpers
    // =========================

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
            1 when parameterTypes[0] is not ITupleTypeSymbol and not UnitTypeSymbol => parameterDisplays[0],
            _ => $"({string.Join(", ", parameterDisplays)})"
        };

        var returnDisplay = FormatType(returnType, format);

        if (returnType.TypeKind == TypeKind.Delegate)
        {
            returnDisplay = $"({returnDisplay})";
        }

        return $"{parameterText} -> {returnDisplay}";
    }

    private static string FormatFunctionParameter(ITypeSymbol type, RefKind refKind, SymbolDisplayFormat format)
    {
        var typeDisplay = FormatType(type, format);

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

    // =========================
    //  Misc helpers
    // =========================

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

    private static string FormatParameter(IParameterSymbol parameter, SymbolDisplayFormat format)
    {
        var includeType = format.ParameterOptions.HasFlag(SymbolDisplayParameterOptions.IncludeType);
        var includeName = format.ParameterOptions.HasFlag(SymbolDisplayParameterOptions.IncludeName);

        // Core "type name" (or just type / just name depending on options)
        var core = FormatNamedSymbol(parameter.Name, parameter.Type, includeType, format, includeName);

        if (parameter.IsParams)
        {
            core = ".." + core;
        }

        // Optionally prepend modifiers (var / .. etc.) when requested
        if (format.MemberOptions.HasFlag(SymbolDisplayMemberOptions.IncludeModifiers))
        {
            var modifiers = GetMemberModifiers(parameter);
            if (!string.IsNullOrEmpty(modifiers))
                return modifiers + " " + core;
        }

        return core;
    }

    private static string EscapeIdentifier(string identifier)
    {
        // Could HTML-escape here if needed
        return identifier;
    }

    private static string FormatNamedSymbol(
        string name,
        ITypeSymbol type,
        bool includeType,
        SymbolDisplayFormat format,
        bool useNameOption)
    {
        var sb = new StringBuilder();

        if (useNameOption)
        {
            sb.Append(EscapeIdentifierIfNeeded(name, format));

            if (includeType)
            {
                sb.Append(": ");
                var typeFormat = WithoutTypeAccessibility(format);
                var typeDisplay = FormatType(type, typeFormat);
                sb.Append(typeDisplay);
            }

            return sb.ToString();
        }

        if (includeType)
        {
            var typeFormat = WithoutTypeAccessibility(format);
            var typeDisplay = FormatType(type, typeFormat);
            sb.Append(typeDisplay);
        }

        return sb.ToString();
    }

    private static string GetFullNamespace(ISymbol symbol, SymbolDisplayFormat format)
    {
        var namespaces = new List<string>();
        var currentNamespace = symbol.ContainingNamespace;

        while (currentNamespace is not null && !currentNamespace.IsGlobalNamespace)
        {
            namespaces.Insert(0, EscapeIdentifierIfNeeded(currentNamespace.Name, format));
            currentNamespace = currentNamespace.ContainingNamespace;
        }

        return string.Join(".", namespaces);
    }

    private static string GetFullType(ISymbol symbol, SymbolDisplayFormat format)
    {
        var types = new List<string>();
        var currentType = symbol.ContainingType;

        while (currentType is not null)
        {
            if (currentType is INamedTypeSymbol named)
            {
                // Include generic type parameters/arguments for each containing type
                types.Insert(0, FormatSimpleNamedType(named, format));
            }
            else
            {
                // Fallback: just the name
                types.Insert(0, EscapeIdentifierIfNeeded(currentType.Name, format));
            }

            currentType = currentType.ContainingType;
        }

        return string.Join(".", types);
    }

    private static string GetMemberModifiers(ISymbol symbol)
    {
        var parts = new List<string>();

        switch (symbol)
        {
            case IParameterSymbol @param:
                if (param.RefKind == RefKind.Out)
                {
                    parts.Add("out");
                }
                if (param.IsMutable)
                {
                    parts.Add("var");
                }
                break;

            case IFieldSymbol field:
                /* if (field.IsConst)
                {
                    parts.Add("const");
                }
                else
                { */
                if (field.IsStatic)
                    parts.Add("static");

                /*
                if (field.IsReadOnly)
                    parts.Add("readonly");

                if (field.IsVolatile)
                    parts.Add("volatile");
                */
                //}

                break;

            case IMethodSymbol method:
                // Local functions will also show these correctly.
                if (method.IsStatic)
                    parts.Add("static");

                if (method.IsAbstract)
                    parts.Add("abstract");

                // C#-style: sealed override
                if (method.IsSealed && method.IsOverride)
                    parts.Add("sealed");

                if (method.IsVirtual)
                    parts.Add("virtual");

                if (method.IsOverride)
                    parts.Add("override");

                if (method.IsAsync)
                    parts.Add("async");

                //if (method.IsPartialDefinition || method.IsPartialImplementation)
                //    parts.Add("partial");

                break;

            case IPropertySymbol property:
                if (property.IsStatic)
                    parts.Add("static");

                /*
                if (property.IsAbstract)
                    parts.Add("abstract");

                if (property.IsSealed && property.IsOverride)
                    parts.Add("sealed");

                if (property.IsVirtual)
                    parts.Add("virtual");

                if (property.IsOverride)
                    parts.Add("override");

                // If Raven has required / readonly properties:
                if (property.IsRequired)
                    parts.Add("required");

                if (property.IsReadOnly)
                    parts.Add("readonly");
                */

                break;

            /*
            case IEventSymbol @event:
                if (@event.IsStatic)
                    parts.Add("static");

                if (@event.IsAbstract)
                    parts.Add("abstract");

                if (@event.IsSealed && @event.IsOverride)
                    parts.Add("sealed");

                if (@event.IsVirtual)
                    parts.Add("virtual");

                if (@event.IsOverride)
                    parts.Add("override");

                break;
            */

            case INamedTypeSymbol type:
                // Class / struct / interface / delegate modifiers
                switch (type.TypeKind)
                {
                    case TypeKind.Class:
                    case TypeKind.Struct:
                    case TypeKind.Interface:
                    case TypeKind.Delegate:
                        if (type.IsStatic)
                        {
                            parts.Add("static");
                        }
                        else
                        {
                            if (type.IsAbstract)
                                parts.Add("abstract");

                            if (!type.IsSealed)
                                parts.Add("open");
                        }

                        break;
                }

                break;
        }

        return string.Join(" ", parts);
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

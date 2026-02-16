using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;

using Raven.CodeAnalysis;

public static class SymbolExtensions
{
    static readonly SymbolDisplayFormat format = SymbolDisplayFormat.FullyQualifiedFormat
        .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly);
    // .WithKindOptions(SymbolDisplayKindOptions.None);

    public static string ToSymbolHierarchyString(this ISymbol symbol, int maxDepth = int.MaxValue)
    {
        return symbol.ToSymbolHierarchyString(static _ => true, maxDepth);
    }

    public static string ToSymbolHierarchyString(this ISymbol symbol, Func<ISymbol, bool> filter, int maxDepth = int.MaxValue)
    {
        var builder = new StringBuilder();
        AppendSymbol(symbol, builder, indent: 0, maxDepth, filter);
        return builder.ToString();
    }

    private static void AppendSymbol(ISymbol symbol, StringBuilder builder, int indent, int maxDepth, Func<ISymbol, bool> filter)
    {
        if (indent > maxDepth)
            return;

        if (!filter(symbol))
            return;

        try
        {
            builder.Append(new string(' ', indent * 2));
            builder.AppendLine($"{symbol.Kind}: {symbol.ToDisplayString(format)}");

            switch (symbol)
            {
                case IAssemblySymbol asm:
                    AppendSymbol(asm.GlobalNamespace, builder, indent + 1, maxDepth, filter);
                    break;

                case IModuleSymbol mod:
                    AppendSymbol(mod.GlobalNamespace, builder, indent + 1, maxDepth, filter);
                    break;

                case INamespaceSymbol ns:
                    foreach (var member in ns.GetMembers().OrderBy(m => m.Name, StringComparer.Ordinal))
                        AppendSymbol(member, builder, indent + 1, maxDepth, filter);
                    break;

                case INamedTypeSymbol type:
                    foreach (var member in type.GetMembers().OrderBy(m => m.Name, StringComparer.Ordinal))
                        AppendSymbol(member, builder, indent + 1, maxDepth, filter);
                    break;
            }
        }
        catch (InvalidOperationException)
        {
            builder.Append(new string(' ', indent * 2));
            builder.AppendLine($"[Error: Failed to load symbol '{symbol.Name}']");
        }
    }

    private static readonly SymbolDisplayFormat s_defaultSymbolListFormat =
        SymbolDisplayFormat.MinimallyQualifiedFormat
            .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces)
            .WithMemberOptions(
                SymbolDisplayMemberOptions.IncludeContainingType |
                SymbolDisplayMemberOptions.IncludeParameters |
                SymbolDisplayMemberOptions.IncludeType |
                SymbolDisplayMemberOptions.IncludeModifiers)
            .WithParameterOptions(
                SymbolDisplayParameterOptions.IncludeType |
                SymbolDisplayParameterOptions.IncludeName |
                SymbolDisplayParameterOptions.IncludeParamsRefOut |
                SymbolDisplayParameterOptions.IncludeOptionalBrackets |
                SymbolDisplayParameterOptions.IncludeDefaultValue)
            .WithMiscellaneousOptions(
                SymbolDisplayMiscellaneousOptions.EscapeKeywordIdentifiers |
                //SymbolDisplayMiscellaneousOptions.IncludeNullableReferenceTypeModifier |
                SymbolDisplayMiscellaneousOptions.UseSpecialTypes);

    public static string ToSymbolListString(this ISymbol symbol, Func<ISymbol, bool> filter, SymbolDisplayFormat? displayFormat = null)
    {
        displayFormat ??= s_defaultSymbolListFormat;

        var builder = new StringBuilder();
        AppendSymbolWithProperties(symbol, builder, filter, displayFormat);
        return builder.ToString();
    }

    private static void AppendSymbolWithProperties(ISymbol symbol, StringBuilder builder, Func<ISymbol, bool> filter, SymbolDisplayFormat displayFormat)
    {
        if (!filter(symbol))
            return;

        var hasSourceDeclaration = !symbol.DeclaringSyntaxReferences.IsDefaultOrEmpty;

        if (hasSourceDeclaration)
        {
            if (builder.Length > 0)
                builder.AppendLine();

            var displayName = symbol.ToDisplayString(displayFormat);
            if (string.IsNullOrWhiteSpace(displayName))
                displayName = GetFallbackDisplay(symbol);

            builder.AppendLine($"Symbol: {displayName}");

            AppendProperty(builder, "Kind", symbol.Kind.ToString());
            AppendProperty(builder, "Name", FormatString(symbol.Name));
            if (symbol.MetadataName is not null)
                AppendProperty(builder, "MetadataName", FormatString(symbol.MetadataName));
            AppendProperty(builder, "DeclaredAccessibility", symbol.DeclaredAccessibility.ToString());
            AppendBooleanProperty(builder, "IsStatic", symbol.IsStatic);
            AppendBooleanProperty(builder, "IsImplicitlyDeclared", symbol.IsImplicitlyDeclared);
            AppendBooleanProperty(builder, "IsAlias", symbol.IsAlias);
            AppendBooleanProperty(builder, "CanBeReferencedByName", symbol.CanBeReferencedByName);
            AppendProperty(builder, "ContainingSymbol", FormatDisplay(symbol.ContainingSymbol, displayFormat));
            AppendProperty(builder, "ContainingNamespace", FormatDisplay(symbol.ContainingNamespace, displayFormat));
            AppendProperty(builder, "ContainingType", FormatDisplay(symbol.ContainingType, displayFormat));

            AppendSequence(builder, "Locations", FormatLocations(symbol.Locations));
            AppendSequence(builder, "DeclaringSyntaxReferences", FormatSyntaxReferences(symbol.DeclaringSyntaxReferences));
            AppendSequence(builder, "Attributes", FormatAttributes(symbol.GetAttributes(), displayFormat));

            AppendKindSpecificProperties(builder, symbol, displayFormat);
        }

        foreach (var child in EnumerateChildren(symbol))
            AppendSymbolWithProperties(child, builder, filter, displayFormat);
    }

    private static IEnumerable<ISymbol> EnumerateChildren(ISymbol symbol)
    {
        switch (symbol)
        {
            case IAssemblySymbol assembly:
                yield return assembly.GlobalNamespace;
                yield break;
            case IModuleSymbol module:
                yield return module.GlobalNamespace;
                yield break;
        }

        if (symbol is INamespaceSymbol namespaceSymbol)
        {
            foreach (var member in namespaceSymbol.GetMembers().OrderBy(m => m.Name, StringComparer.Ordinal))
                yield return member;
            yield break;
        }

        if (symbol is INamedTypeSymbol typeSymbol)
        {
            foreach (var member in typeSymbol.GetMembers().OrderBy(m => m.Name, StringComparer.Ordinal))
                yield return member;

            foreach (var typeParameter in typeSymbol.TypeParameters)
                yield return typeParameter;

            yield break;
        }

        if (symbol is IMethodSymbol methodSymbol)
        {
            foreach (var typeParameter in methodSymbol.TypeParameters)
                yield return typeParameter;

            foreach (var parameter in methodSymbol.Parameters)
                yield return parameter;

            yield break;
        }

        if (symbol is IPropertySymbol propertySymbol)
        {
            if (propertySymbol.GetMethod is not null)
                yield return propertySymbol.GetMethod;
            if (propertySymbol.SetMethod is not null)
                yield return propertySymbol.SetMethod;
        }
    }

    private static void AppendKindSpecificProperties(StringBuilder builder, ISymbol symbol, SymbolDisplayFormat displayFormat)
    {
        switch (symbol)
        {
            case INamespaceSymbol namespaceSymbol:
                AppendBooleanProperty(builder, "IsGlobalNamespace", namespaceSymbol.IsGlobalNamespace);
                break;

            case INamedTypeSymbol typeSymbol:
                AppendProperty(builder, "TypeKind", typeSymbol.TypeKind.ToString());
                AppendProperty(builder, "BaseType", FormatDisplay(typeSymbol.BaseType, displayFormat));
                AppendSequence(builder, "Interfaces", typeSymbol.Interfaces.Select(t => t.ToDisplayString(displayFormat)));
                AppendBooleanProperty(builder, "IsAbstract", typeSymbol.IsAbstract);
                AppendBooleanProperty(builder, "IsSealed", typeSymbol.IsClosed);
                AppendBooleanProperty(builder, "IsGenericType", typeSymbol.IsGenericType);
                AppendBooleanProperty(builder, "IsUnboundGenericType", typeSymbol.IsUnboundGenericType);
                if (typeSymbol.TypeParameters.Length > 0)
                    AppendSequence(builder, "TypeParameters", typeSymbol.TypeParameters.Select(tp => tp.ToDisplayString(displayFormat)));
                if (typeSymbol.TypeArguments.Length > 0)
                    AppendSequence(builder, "TypeArguments", typeSymbol.TypeArguments.Select(tp => tp.ToDisplayString(displayFormat)));
                break;

            case IMethodSymbol methodSymbol:
                AppendProperty(builder, "MethodKind", methodSymbol.MethodKind.ToString());
                AppendProperty(builder, "ReturnType", methodSymbol.ReturnType.ToDisplayString(displayFormat));
                AppendSequence(builder, "Parameters", methodSymbol.Parameters.Select(FormatParameter));
                if (methodSymbol.TypeParameters.Length > 0)
                    AppendSequence(builder, "TypeParameters", methodSymbol.TypeParameters.Select(tp => tp.ToDisplayString(displayFormat)));
                AppendBooleanProperty(builder, "IsConstructor", methodSymbol.IsConstructor);
                AppendBooleanProperty(builder, "IsAbstract", methodSymbol.IsAbstract);
                AppendBooleanProperty(builder, "IsAsync", methodSymbol.IsAsync);
                AppendBooleanProperty(builder, "IsOverride", methodSymbol.IsOverride);
                AppendBooleanProperty(builder, "IsVirtual", methodSymbol.IsVirtual);
                AppendBooleanProperty(builder, "IsSealed", methodSymbol.IsFinal);
                AppendBooleanProperty(builder, "IsExtensionMethod", methodSymbol.IsExtensionMethod);
                if (methodSymbol.ExplicitInterfaceImplementations.Length > 0)
                    AppendSequence(builder, "ExplicitInterfaceImplementations", methodSymbol.ExplicitInterfaceImplementations.Select(tp => tp.ToDisplayString(displayFormat)));
                if (methodSymbol.AssociatedSymbol is not null)
                    AppendProperty(builder, "AssociatedSymbol", methodSymbol.AssociatedSymbol?.ToDisplayString(displayFormat) ?? "<null>");
                break;

            case IPropertySymbol propertySymbol:
                AppendProperty(builder, "Type", propertySymbol.Type.ToDisplayString(displayFormat));
                AppendBooleanProperty(builder, "IsIndexer", propertySymbol.IsIndexer);
                AppendBooleanProperty(builder, "IsRequired", propertySymbol.IsRequired);
                AppendProperty(builder, "Getter", FormatDisplay(propertySymbol.GetMethod, displayFormat));
                AppendProperty(builder, "Setter", FormatDisplay(propertySymbol.SetMethod, displayFormat));
                if (propertySymbol.ExplicitInterfaceImplementations.Length > 0)
                    AppendSequence(builder, "ExplicitInterfaceImplementations", propertySymbol.ExplicitInterfaceImplementations.Select(tp => tp.ToDisplayString(displayFormat)));
                break;

            case IFieldSymbol fieldSymbol:
                AppendProperty(builder, "Type", fieldSymbol.Type.ToDisplayString(displayFormat));
                AppendBooleanProperty(builder, "IsMutable", fieldSymbol.IsMutable);
                AppendBooleanProperty(builder, "IsConst", fieldSymbol.IsConst);
                AppendBooleanProperty(builder, "IsRequired", fieldSymbol.IsRequired);
                if (fieldSymbol.IsConst)
                {
                    var constantValue = fieldSymbol.GetConstantValue();
                    AppendProperty(builder, "ConstantValue", constantValue?.ToString() ?? "<null>");
                }
                break;

            case IParameterSymbol parameterSymbol:
                AppendProperty(builder, "Type", parameterSymbol.Type.ToDisplayString(displayFormat));
                AppendBooleanProperty(builder, "IsMutable", parameterSymbol.IsMutable);
                AppendProperty(builder, "RefKind", parameterSymbol.RefKind.ToString());
                AppendBooleanProperty(builder, "IsParams", parameterSymbol.IsParams);
                AppendBooleanProperty(builder, "IsMutable", parameterSymbol.IsMutable);
                AppendBooleanProperty(builder, "HasExplicitDefaultValue", parameterSymbol.HasExplicitDefaultValue);
                if (parameterSymbol.HasExplicitDefaultValue)
                {
                    var defaultValue = parameterSymbol.ExplicitDefaultValue;
                    var formattedDefault = defaultValue?.ToString() ?? "null";
                    AppendProperty(builder, "ExplicitDefaultValue", formattedDefault);
                }
                break;

            case ILocalSymbol localSymbol:
                AppendProperty(builder, "Type", localSymbol.Type.ToDisplayString(displayFormat));
                AppendBooleanProperty(builder, "IsMutable", localSymbol.IsMutable);
                break;

            case ITypeParameterSymbol typeParameterSymbol:
                AppendProperty(builder, "Variance", typeParameterSymbol.Variance.ToString());
                AppendProperty(builder, "ConstraintKind", typeParameterSymbol.ConstraintKind.ToString());
                if (!typeParameterSymbol.ConstraintTypes.IsDefaultOrEmpty)
                    AppendSequence(builder, "ConstraintTypes", typeParameterSymbol.ConstraintTypes.Select(t => t.ToDisplayString(displayFormat)));
                break;
        }
    }

    private static string FormatParameter(IParameterSymbol parameter)
    {
        var parts = new List<string>();

        if (parameter.IsParams)
            parts.Add("params");

        parts.Add(parameter.RefKind switch
        {
            RefKind.Ref => "ref",
            RefKind.Out => "out",
            RefKind.In => "in",
            RefKind.RefReadOnly => "ref readonly",
            RefKind.RefReadOnlyParameter => "ref readonly",
            _ => string.Empty
        });

        if (parameter.IsMutable)
            parts.Add("var");

        var typeDisplay = parameter.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
        parts.Add(typeDisplay);
        parts.Add(parameter.Name);

        var signature = string.Join(' ', parts.Where(p => !string.IsNullOrEmpty(p)));

        if (parameter.HasExplicitDefaultValue)
        {
            var defaultValue = parameter.ExplicitDefaultValue ?? "null";
            signature += $" = {defaultValue}";
        }

        return signature;
    }

    private static string FormatDisplay(ISymbol? symbol, SymbolDisplayFormat format)
    {
        if (symbol is null)
            return "<null>";

        var display = symbol.ToDisplayString(format);
        if (!string.IsNullOrWhiteSpace(display))
            return display;

        return GetFallbackDisplay(symbol);
    }

    private static string GetFallbackDisplay(ISymbol symbol)
    {
        return symbol switch
        {
            INamespaceSymbol { IsGlobalNamespace: true } => "<global namespace>",
            IAssemblySymbol assembly => assembly.Name,
            IModuleSymbol module when !string.IsNullOrWhiteSpace(module.Name) => module.Name,
            { Name.Length: > 0 } => symbol.Name,
            _ => "<anonymous>"
        };
    }

    private static IEnumerable<string> FormatLocations(ImmutableArray<Location> locations)
    {
        if (locations.IsDefaultOrEmpty)
            yield break;

        foreach (var location in locations.OrderBy(l => l))
        {
            if (location == Location.None)
                continue;

            var span = location.GetLineSpan();
            var displayPath = FormatPath(span.Path);
            var start = span.StartLinePosition;
            var end = span.EndLinePosition;

            yield return $"{displayPath}({start.Line + 1},{start.Character + 1})-({end.Line + 1},{end.Character + 1})";
        }
    }

    private static IEnumerable<string> FormatSyntaxReferences(ImmutableArray<SyntaxReference> references)
    {
        if (references.IsDefaultOrEmpty)
            yield break;

        foreach (var reference in references)
        {
            var tree = reference.SyntaxTree;
            var location = tree.GetLocation(reference.Span);
            var span = location.GetLineSpan();
            var displayPath = FormatPath(span.Path);
            var start = span.StartLinePosition;
            var end = span.EndLinePosition;

            yield return $"{displayPath}({start.Line + 1},{start.Character + 1})-({end.Line + 1},{end.Character + 1})";
        }
    }

    private static IEnumerable<string> FormatAttributes(ImmutableArray<AttributeData> attributes, SymbolDisplayFormat format)
    {
        if (attributes.IsDefaultOrEmpty)
            yield break;

        foreach (var attribute in attributes)
        {
            var name = attribute.AttributeClass?.ToDisplayString(format) ?? attribute.ToString();
            if (!string.IsNullOrEmpty(name))
                yield return name!;
        }
    }

    private static void AppendProperty(StringBuilder builder, string name, string value)
    {
        builder.Append("  ").Append(name).Append(": ").AppendLine(value);
    }

    private static void AppendBooleanProperty(StringBuilder builder, string name, bool value)
        => AppendProperty(builder, name, value ? "true" : "false");

    private static void AppendSequence(StringBuilder builder, string name, IEnumerable<string> values)
    {
        var items = values.Where(v => !string.IsNullOrWhiteSpace(v)).ToArray();

        if (items.Length == 0)
        {
            AppendProperty(builder, name, "<none>");
            return;
        }

        builder.Append("  ").Append(name).AppendLine(":");
        foreach (var item in items)
            builder.Append("    - ").AppendLine(item);
    }

    private static string FormatPath(string? path)
    {
        if (string.IsNullOrEmpty(path))
            return "<unknown>";

        try
        {
            var relative = Path.GetRelativePath(Environment.CurrentDirectory, path);
            if (!relative.StartsWith("..", StringComparison.Ordinal))
                return NormalizePath(relative);
        }
        catch
        {
        }

        return NormalizePath(path);
    }

    private static string NormalizePath(string path)
        => path.Replace('\\', '/');

    private static string FormatString(string? value)
        => value switch
        {
            null => "<null>",
            { Length: 0 } => "<empty>",
            _ => value
        };
}

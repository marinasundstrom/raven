using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Globalization;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class AttributeUsageHelper
{
    private readonly struct AttributeUsageInfo(AttributeTargets validTargets, bool allowMultiple)
    {
        public AttributeTargets ValidTargets { get; } = validTargets;

        public bool AllowMultiple { get; } = allowMultiple;
    }

    public static bool TryValidateAttribute(
        Compilation compilation,
        SemanticModel semanticModel,
        ISymbol owner,
        AttributeSyntax attributeSyntax,
        AttributeData data,
        AttributeTargets defaultTarget,
        Dictionary<AttributeTargets, HashSet<INamedTypeSymbol>> seenAttributes)
    {
        if (compilation is null)
            throw new ArgumentNullException(nameof(compilation));

        if (semanticModel is null)
            throw new ArgumentNullException(nameof(semanticModel));

        if (owner is null)
            throw new ArgumentNullException(nameof(owner));

        if (attributeSyntax is null)
            throw new ArgumentNullException(nameof(attributeSyntax));

        if (data is null)
            throw new ArgumentNullException(nameof(data));

        if (seenAttributes is null)
            throw new ArgumentNullException(nameof(seenAttributes));

        var attributeClass = data.AttributeClass;
        if (attributeClass is null)
            return false;

        var target = ResolveAttributeTarget(attributeSyntax, owner, defaultTarget);
        var usage = GetAttributeUsageInfo(compilation, attributeClass);

        if (!IsTargetAllowed(target, usage.ValidTargets))
        {
            ReportAttributeNotValidForTarget(semanticModel, attributeSyntax, attributeClass.Name, target, usage.ValidTargets);
            return false;
        }

        if (!usage.AllowMultiple && !RegisterAttributeApplication(seenAttributes, target, attributeClass))
        {
            ReportAttributeDoesNotAllowMultiple(semanticModel, attributeSyntax, attributeClass.Name, target);
            return false;
        }

        return true;
    }

    private static bool IsTargetAllowed(AttributeTargets appliedTarget, AttributeTargets validTargets)
    {
        if (validTargets == AttributeTargets.All)
            return true;

        return (validTargets & appliedTarget) != 0;
    }

    private static bool RegisterAttributeApplication(
        Dictionary<AttributeTargets, HashSet<INamedTypeSymbol>> seenAttributes,
        AttributeTargets target,
        INamedTypeSymbol attributeClass)
    {
        if (!seenAttributes.TryGetValue(target, out var set))
        {
            set = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);
            seenAttributes[target] = set;
        }

        return set.Add(attributeClass);
    }

    private static void ReportAttributeNotValidForTarget(
        SemanticModel semanticModel,
        AttributeSyntax attributeSyntax,
        string attributeName,
        AttributeTargets attemptedTarget,
        AttributeTargets validTargets)
    {
        if (semanticModel.GetBinder(attributeSyntax) is AttributeBinder binder)
        {
            binder.Diagnostics.ReportAttributeNotValidForTarget(
                attributeName,
                GetTargetDisplay(attemptedTarget),
                GetTargetsDisplay(validTargets),
                attributeSyntax.GetLocation());
        }
    }

    private static void ReportAttributeDoesNotAllowMultiple(
        SemanticModel semanticModel,
        AttributeSyntax attributeSyntax,
        string attributeName,
        AttributeTargets target)
    {
        if (semanticModel.GetBinder(attributeSyntax) is AttributeBinder binder)
        {
            binder.Diagnostics.ReportAttributeDoesNotAllowMultiple(
                attributeName,
                GetTargetDisplay(target),
                attributeSyntax.GetLocation());
        }
    }

    private static AttributeTargets ResolveAttributeTarget(AttributeSyntax attributeSyntax, ISymbol owner, AttributeTargets defaultTarget)
    {
        if (attributeSyntax.Parent is AttributeListSyntax list && list.Target is { } targetSpecifier)
        {
            var identifier = targetSpecifier.Identifier.ValueText;
            if (!string.IsNullOrWhiteSpace(identifier))
            {
                if (TryMapExplicitTarget(identifier, owner, out var explicitTarget))
                    return explicitTarget;
            }
        }

        return defaultTarget;
    }

    private static bool TryMapExplicitTarget(string identifier, ISymbol owner, out AttributeTargets target)
    {
        identifier = identifier.Trim();

        if (identifier.Length == 0)
        {
            target = default;
            return false;
        }

        switch (identifier.ToLowerInvariant())
        {
            case "assembly":
                target = AttributeTargets.Assembly;
                return true;
            case "module":
                target = AttributeTargets.Module;
                return true;
            case "field":
                target = AttributeTargets.Field;
                return true;
            case "event":
                target = AttributeTargets.Event;
                return true;
            case "method":
                target = AttributeTargets.Method;
                return true;
            case "param":
            case "parameter":
                target = AttributeTargets.Parameter;
                return true;
            case "property":
                target = AttributeTargets.Property;
                return true;
            case "return":
                target = AttributeTargets.ReturnValue;
                return true;
            case "type":
                target = GetDefaultTargetForOwner(owner);
                return true;
        }

        target = default;
        return false;
    }

    public static AttributeTargets GetDefaultTargetForOwner(ISymbol owner)
    {
        return owner.Kind switch
        {
            SymbolKind.Assembly => AttributeTargets.Assembly,
            SymbolKind.Module => AttributeTargets.Module,
            SymbolKind.Method => GetMethodTarget((IMethodSymbol)owner),
            SymbolKind.Parameter => AttributeTargets.Parameter,
            SymbolKind.Property => AttributeTargets.Property,
            SymbolKind.Field => AttributeTargets.Field,
            SymbolKind.Type => GetTypeTarget((INamedTypeSymbol)owner),
            _ => AttributeTargets.All,
        };
    }

    private static AttributeTargets GetMethodTarget(IMethodSymbol method)
    {
        if (method.MethodKind is MethodKind.Constructor or MethodKind.NamedConstructor or MethodKind.StaticConstructor)
            return AttributeTargets.Constructor;

        return AttributeTargets.Method;
    }

    private static AttributeTargets GetTypeTarget(INamedTypeSymbol type)
    {
        return type.TypeKind switch
        {
            TypeKind.Class => AttributeTargets.Class,
            TypeKind.Struct => AttributeTargets.Struct,
            TypeKind.Interface => AttributeTargets.Interface,
            TypeKind.Enum => AttributeTargets.Enum,
            TypeKind.Delegate => AttributeTargets.Delegate,
            _ => AttributeTargets.All,
        };
    }

    private static AttributeUsageInfo GetAttributeUsageInfo(Compilation compilation, INamedTypeSymbol attributeType)
    {
        var attributeUsageType = compilation.GetTypeByMetadataName("System.AttributeUsageAttribute");
        if (attributeUsageType is not null)
        {
            foreach (var attribute in attributeType.GetAttributes())
            {
                if (attribute.AttributeClass is not null &&
                    SymbolEqualityComparer.Default.Equals(attribute.AttributeClass, attributeUsageType))
                {
                    var validTargets = TryReadAttributeTargets(attribute.ConstructorArguments)
                        ?? AttributeTargets.All;
                    var allowMultiple = TryReadAllowMultiple(attribute.NamedArguments)
                        ?? TryReadAllowMultipleFromClrType(compilation, attributeType)
                        ?? false;

                    return new AttributeUsageInfo(validTargets, allowMultiple);
                }
            }
        }

        if (TryGetClrAttributeUsage(compilation, attributeType) is { } clrUsage)
            return clrUsage;

        return new AttributeUsageInfo(AttributeTargets.All, false);
    }

    private static AttributeUsageInfo? TryGetClrAttributeUsage(Compilation compilation, INamedTypeSymbol attributeType)
    {
        try
        {
            var clrType = attributeType.GetClrType(compilation);
            if (clrType is null)
                return null;

            var usage = (AttributeUsageAttribute?)Attribute.GetCustomAttribute(clrType, typeof(AttributeUsageAttribute));
            if (usage is null)
                return null;

            return new AttributeUsageInfo(usage.ValidOn, usage.AllowMultiple);
        }
        catch
        {
            return null;
        }
    }

    private static AttributeTargets? TryReadAttributeTargets(ImmutableArray<TypedConstant> arguments)
    {
        if (arguments.IsDefaultOrEmpty)
            return null;

        var first = arguments[0];

        if (first.Value is AttributeTargets direct)
            return direct;

        if (first.Value is int intValue)
            return (AttributeTargets)intValue;

        if (first.Value is long longValue)
            return (AttributeTargets)longValue;

        if (first.Type is INamedTypeSymbol { TypeKind: TypeKind.Enum } && first.Value is not null)
        {
            try
            {
                return (AttributeTargets)Convert.ToInt64(first.Value, CultureInfo.InvariantCulture);
            }
            catch
            {
            }
        }

        return null;
    }

    private static bool? TryReadAllowMultiple(ImmutableArray<KeyValuePair<string, TypedConstant>> namedArguments)
    {
        foreach (var (name, value) in namedArguments)
        {
            if (string.Equals(name, nameof(AttributeUsageAttribute.AllowMultiple), StringComparison.Ordinal) &&
                value.Value is bool boolValue)
            {
                return boolValue;
            }
        }

        return null;
    }

    private static bool? TryReadAllowMultipleFromClrType(Compilation compilation, INamedTypeSymbol attributeType)
    {
        var usage = TryGetClrAttributeUsage(compilation, attributeType);
        return usage?.AllowMultiple;
    }

    private static string GetTargetDisplay(AttributeTargets target)
        => GetTargetsDisplay(target);

    private static string GetTargetsDisplay(AttributeTargets targets)
    {
        if (targets == AttributeTargets.All)
            return "all targets";

        var parts = targets
            .ToString()
            .Split(new[] { ',', '|' }, StringSplitOptions.RemoveEmptyEntries)
            .Select(static part => part.Trim())
            .Where(static part => part.Length > 0)
            .Select(FormatTargetName)
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .ToArray();

        if (parts.Length == 0)
            return targets.ToString();

        if (parts.Length == 1)
            return parts[0];

        if (parts.Length == 2)
            return string.Concat(parts[0], " or ", parts[1]);

        return string.Concat(string.Join(", ", parts.Take(parts.Length - 1)), ", or ", parts[^1]);
    }

    private static string FormatTargetName(string name)
    {
        if (string.IsNullOrEmpty(name))
            return name;

        Span<char> buffer = stackalloc char[name.Length * 2];
        var index = 0;
        for (var i = 0; i < name.Length; i++)
        {
            var c = name[i];
            if (char.IsUpper(c) && i > 0)
            {
                buffer[index++] = ' ';
                buffer[index++] = char.ToLowerInvariant(c);
            }
            else
            {
                buffer[index++] = char.ToLowerInvariant(c);
            }
        }

        return new string(buffer[..index]);
    }
}

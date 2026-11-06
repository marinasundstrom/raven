using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

// Ordered by precedence when several caller information attributes apply.
internal enum CallerInfoKind
{
    None,
    ArgumentExpression,
    MemberName,
    FilePath,
    LineNumber
}

internal static class CallerInfoAttributeFacts
{
    internal static CallerInfoKind GetKind(AttributeData attribute) =>
        attribute.AttributeClass?.ToFullyQualifiedMetadataName() switch
        {
            "System.Runtime.CompilerServices.CallerArgumentExpressionAttribute" => CallerInfoKind.ArgumentExpression,
            "System.Runtime.CompilerServices.CallerMemberNameAttribute" => CallerInfoKind.MemberName,
            "System.Runtime.CompilerServices.CallerFilePathAttribute" => CallerInfoKind.FilePath,
            "System.Runtime.CompilerServices.CallerLineNumberAttribute" => CallerInfoKind.LineNumber,
            _ => CallerInfoKind.None
        };

    internal static AttributeData? GetEffectiveAttribute(ImmutableArray<AttributeData> attributes)
    {
        AttributeData? selected = null;
        var selectedKind = CallerInfoKind.None;
        foreach (var attribute in attributes)
        {
            var kind = GetKind(attribute);
            if (kind > selectedKind)
            {
                selected = attribute;
                selectedKind = kind;
            }
        }

        return selected;
    }

    internal static void Validate(Compilation compilation, AttributeBinder? binder, Symbol owner,
        AttributeSyntax syntax, AttributeData attribute)
    {
        if (binder is null || owner is not ParameterSymbol parameter)
            return;

        var kind = GetKind(attribute);
        if (kind == CallerInfoKind.None)
            return;

        var name = attribute.AttributeClass!.Name;
        var location = syntax.GetLocation();
        if (!parameter.HasExplicitDefaultValue)
        {
            binder.Diagnostics.ReportCallerInfoRequiresDefaultValue(name, location);
            return;
        }

        var sourceType = compilation.GetSpecialType(kind == CallerInfoKind.LineNumber
            ? SpecialType.System_Int32 : SpecialType.System_String);
        var conversion = compilation.ClassifyConversion(sourceType, parameter.Type, includeUserDefined: false);
        if (!conversion.Exists || !conversion.IsImplicit)
        {
            binder.Diagnostics.ReportCallerInfoRequiresStandardConversion(
                name, sourceType.ToDisplayString(), parameter.Type.ToDisplayString(), location);
            return;
        }

        var attributes = parameter is SourceSymbol source
            ? source.GetAttributesForUsageValidation()
            : parameter.GetAttributes();
        if (GetEffectiveAttribute(attributes) is { } effective && GetKind(effective) > kind)
        {
            binder.Diagnostics.ReportCallerInfoAttributeOverridden(name, parameter.Name, effective.AttributeClass!.Name, location);
            return;
        }

        if (kind != CallerInfoKind.ArgumentExpression)
            return;

        var sibling = attribute.ConstructorArguments is [{ Value: string value }] ? value : null;
        if (sibling == parameter.Name)
        {
            binder.Diagnostics.ReportCallerArgumentExpressionSelfReference(parameter.Name, location);
            return;
        }

        var parameters = parameter.ContainingSymbol switch
        {
            MethodSymbol method => method.Parameters,
            PropertySymbol property => property.Parameters,
            _ => ImmutableArray<ParameterSymbol>.Empty
        };
        if (!parameters.Any(candidate => candidate.Name == sibling))
            binder.Diagnostics.ReportCallerArgumentExpressionParameterNotFound(parameter.Name, location);
    }
}

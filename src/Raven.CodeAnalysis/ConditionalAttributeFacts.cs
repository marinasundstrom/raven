using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class ConditionalAttributeFacts
{
    private static bool IsConditional(AttributeData attribute)
        => attribute.AttributeClass.ToFullyQualifiedMetadataName() == "System.Diagnostics.ConditionalAttribute";

    internal static IEnumerable<string> GetConditions(Symbol symbol)
    {
        foreach (var attribute in symbol.GetAttributes())
            if (IsConditional(attribute) && attribute.ConstructorArguments is [{ Value: string condition }])
                yield return condition;

        Symbol? inherited = symbol switch
        {
            MethodSymbol method when method.IsOverride => FindOverriddenMethod(method),
            NamedTypeSymbol type => type.BaseType,
            _ => null
        };
        if (inherited is not null)
            foreach (var condition in GetConditions(inherited))
                yield return condition;
    }

    private static MethodSymbol? FindOverriddenMethod(MethodSymbol method)
    {
        if (method is SourceMethodSymbol { OverriddenMethod: { } overridden })
            return overridden;
        for (var type = method.ContainingType?.BaseType; type is not null; type = type.BaseType)
            foreach (var candidate in type.GetMembers(method.Name).OfType<MethodSymbol>())
                if (candidate.IsVirtual && HasSameSignature(method, candidate))
                    return candidate;
        return null;
    }

    private static bool HasSameSignature(MethodSymbol left, MethodSymbol right)
        => left.Name == right.Name && left.Arity == right.Arity &&
           left.Parameters.Length == right.Parameters.Length &&
           left.Parameters.Zip(right.Parameters).All(p => p.First.RefKind == p.Second.RefKind &&
               SymbolEqualityComparer.Default.Equals(p.First.Type, p.Second.Type));

    internal static bool HasConditions(MethodSymbol method) => GetConditions(method).Any();

    internal static bool IsOmitted(Symbol symbol, IEnumerable<string> definedSymbols)
    {
        var conditions = GetConditions(symbol).ToArray();
        return conditions.Length > 0 && !conditions.Any(c => definedSymbols.Contains(c, StringComparer.Ordinal));
    }

    internal static bool IsAttributeOmitted(AttributeData attribute)
        => attribute.ApplicationSyntaxReference is { } syntax &&
           IsOmitted(attribute.AttributeClass, syntax.SyntaxTree.Options.PreprocessorSymbolNames);

    internal static string? GetDeclarationError(Symbol owner, AttributeData attribute)
    {
        if (!IsConditional(attribute))
            return null;
        if (attribute.ConstructorArguments is not [{ Value: string condition }] ||
            condition.Length == 0 || !SyntaxFacts.IsIdentifierStartCharacter(condition[0]) ||
            !condition.Skip(1).All(SyntaxFacts.IsIdentifierPartCharacter))
            return "A conditional symbol must be a nonempty identifier";

        if (owner is MethodSymbol method)
        {
            if (method.MethodKind != MethodKind.Ordinary || method.IsOverride ||
                method.ContainingType?.TypeKind == TypeKind.Interface)
                return "ConditionalAttribute requires an ordinary non-override class or struct method";
            if (method.ReturnType.SpecialType is not (SpecialType.System_Void or SpecialType.System_Unit))
                return "Conditional methods must return unit (void)";
            if (method.Parameters.Any(p => p.RefKind == RefKind.Out))
                return "Conditional methods cannot have out parameters";
            if (!method.ExplicitInterfaceImplementations.IsEmpty ||
                method.ContainingType?.AllInterfaces.SelectMany(i => i.GetMembers()).OfType<MethodSymbol>()
                    .Any(m => HasSameSignature(method, m)) == true)
                return "Conditional methods cannot implement interface members";
            return null;
        }
        if (owner is NamedTypeSymbol type)
        {
            for (var parent = type.BaseType; parent is not null; parent = parent.BaseType)
                if (parent.ToFullyQualifiedMetadataName() == "System.Attribute")
                    return null;
        }
        return "ConditionalAttribute requires a method or an attribute class";
    }
}

using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class AsyncIteratorCancellationUtilities
{
    private const string CancellationTokenMetadataName = "CancellationToken";
    private const string EnumeratorCancellationAttributeMetadataName = "EnumeratorCancellationAttribute";
    private const string CancellationTokenNamespace = "System.Threading";
    private const string RuntimeCompilerServicesNamespace = "System.Runtime.CompilerServices";
    private const string AsyncEnumerableMetadataName = "IAsyncEnumerable`1";
    private const string CollectionsGenericNamespace = "System.Collections.Generic";

    public static ImmutableArray<IParameterSymbol> GetCancellationTokenParameters(Compilation compilation, IMethodSymbol method)
    {
        return method.Parameters
            .Where(parameter => IsCancellationTokenParameter(compilation, parameter))
            .ToImmutableArray();
    }

    public static ImmutableArray<IParameterSymbol> GetEnumeratorCancellationParameters(Compilation compilation, IMethodSymbol method)
    {
        return method.Parameters
            .Where(parameter =>
                IsCancellationTokenParameter(compilation, parameter) &&
                HasEnumeratorCancellationAttribute(parameter))
            .ToImmutableArray();
    }

    public static IParameterSymbol? GetEffectiveEnumeratorCancellationParameter(
        Compilation compilation,
        IMethodSymbol method,
        IteratorMethodKind iteratorKind)
    {
        if (iteratorKind != IteratorMethodKind.AsyncEnumerable || !method.IsAsync)
            return null;

        var candidates = GetEnumeratorCancellationParameters(compilation, method);
        return candidates.Length == 1 ? candidates[0] : null;
    }

    public static bool ShouldWarnAboutMissingEnumeratorCancellation(
        Compilation compilation,
        IMethodSymbol method,
        IteratorMethodKind iteratorKind)
    {
        if (iteratorKind != IteratorMethodKind.AsyncEnumerable || !method.IsAsync)
            return false;

        return !GetCancellationTokenParameters(compilation, method).IsDefaultOrEmpty &&
               GetEnumeratorCancellationParameters(compilation, method).IsDefaultOrEmpty;
    }

    public static bool IsAsyncEnumerable(Compilation compilation, ITypeSymbol type)
    {
        if (type is not INamedTypeSymbol named)
            return false;

        var definition = named.ConstructedFrom as INamedTypeSymbol ?? named;
        return definition.MetadataName == AsyncEnumerableMetadataName &&
               NamespaceEquals(definition.ContainingNamespace, CollectionsGenericNamespace);
    }

    public static bool IsCancellationTokenParameter(Compilation compilation, IParameterSymbol parameter)
    {
        var cancellationTokenType = compilation.GetTypeByMetadataName($"{CancellationTokenNamespace}.{CancellationTokenMetadataName}");
        return cancellationTokenType is not null &&
               SymbolEqualityComparer.Default.Equals(parameter.Type, cancellationTokenType);
    }

    public static bool HasEnumeratorCancellationAttribute(IParameterSymbol parameter)
    {
        if (parameter.GetAttributes().Any(static attribute =>
            attribute.AttributeClass is { MetadataName: EnumeratorCancellationAttributeMetadataName } attributeClass &&
            NamespaceEquals(attributeClass.ContainingNamespace, RuntimeCompilerServicesNamespace)))
        {
            return true;
        }

        return parameter.DeclaringSyntaxReferences
            .Select(reference => reference.GetSyntax())
            .OfType<ParameterSyntax>()
            .SelectMany(static parameterSyntax => parameterSyntax.AttributeLists)
            .SelectMany(static attributeList => attributeList.Attributes)
            .Any(static attribute => MatchesEnumeratorCancellationAttributeName(attribute.Name));
    }

    private static bool NamespaceEquals(INamespaceSymbol? namespaceSymbol, string qualifiedMetadataName)
    {
        if (namespaceSymbol is null)
            return false;

        var current = namespaceSymbol;
        var builder = ImmutableArray.CreateBuilder<string>();
        while (current is not null && !current.IsGlobalNamespace)
        {
            builder.Insert(0, current.MetadataName);
            current = current.ContainingNamespace;
        }

        return string.Join(".", builder) == qualifiedMetadataName;
    }

    private static bool MatchesEnumeratorCancellationAttributeName(TypeSyntax attributeName)
    {
        var text = attributeName.ToString();
        return text == "EnumeratorCancellation" ||
               text == "EnumeratorCancellationAttribute" ||
               text.EndsWith(".EnumeratorCancellation", System.StringComparison.Ordinal) ||
               text.EndsWith(".EnumeratorCancellationAttribute", System.StringComparison.Ordinal);
    }
}

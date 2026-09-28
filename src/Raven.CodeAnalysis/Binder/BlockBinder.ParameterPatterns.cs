using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal partial class BlockBinder
{
    private void ValidateParameterPattern(PatternSyntax syntax, ITypeSymbol inputType, BoundPattern pattern)
    {
        // Binding errors already explain invalid shapes and types. Coverage is a
        // separate check on a successfully bound parameter, not a recovery diagnostic.
        if (_diagnostics.AsEnumerable().Any(diagnostic =>
                diagnostic.Severity == DiagnosticSeverity.Error &&
                diagnostic.Location.SourceTree == syntax.SyntaxTree &&
                syntax.Span.Contains(diagnostic.Location.SourceSpan)))
        {
            return;
        }

        if (FindRefutableParameterPattern(inputType, pattern) is not { } failure)
            return;

        var failureSyntax = syntax.DescendantNodesAndSelf().OfType<PatternSyntax>()
            .FirstOrDefault(candidate => ReferenceEquals(TryGetCachedBoundNode(candidate), failure.Pattern))
            ?? syntax;

        _diagnostics.ReportRefutableParameterPattern(
            failure.InputType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
            failureSyntax.GetLocation());
    }

    private (ITypeSymbol InputType, BoundPattern Pattern)? FindRefutableParameterPattern(
        ITypeSymbol inputType,
        BoundPattern pattern)
    {
        inputType = UnwrapAlias(inputType);
        if (inputType.TypeKind == TypeKind.Error || pattern.Type?.TypeKind == TypeKind.Error ||
            pattern.Reason != BoundExpressionReason.None)
        {
            return null;
        }

        switch (pattern)
        {
            case BoundPositionalPattern positional:
                {
                    if (CanBeNull(inputType))
                        return (inputType, pattern);

                    if (positional.IsSequence)
                    {
                        var requiredLength = positional.ElementWidths.Where(width => width > 0).Sum();
                        var hasRest = positional.RestIndex >= 0;
                        var coversLength = inputType is IArrayTypeSymbol { FixedLength: int length }
                            ? hasRest ? requiredLength <= length : requiredLength == length
                            : hasRest && requiredLength == 0;

                        if (!coversLength)
                            return (inputType, pattern);

                        if (!TryGetSequenceDeconstructionElementType(inputType, out var elementType))
                            return (inputType, pattern);

                        for (var i = 0; i < positional.Elements.Length; i++)
                        {
                            var expectedType = GetSequencePatternElementType(
                                inputType, elementType, positional.ElementWidths,
                                positional.ElementKinds, positional.RestIndex, i);
                            if (FindRefutableParameterPattern(expectedType, positional.Elements[i]) is { } failure)
                                return failure;
                        }

                        return null;
                    }

                    var elementTypes = GetTupleElementTypes(inputType);
                    if (elementTypes.IsDefaultOrEmpty)
                        elementTypes = GetPrimaryConstructorDeconstructionElementTypes(inputType);

                    if (elementTypes.Length != positional.Elements.Length)
                        return (inputType, pattern);

                    for (var i = 0; i < positional.Elements.Length; i++)
                    {
                        if (FindRefutableParameterPattern(elementTypes[i], positional.Elements[i]) is { } failure)
                            return failure;
                    }

                    return null;
                }
            case BoundDeconstructPattern deconstruction:
                {
                    if (CanBeNull(inputType) ||
                        deconstruction.NarrowedType is { } narrowedType && !IsAssignable(narrowedType, inputType, out _))
                    {
                        return (inputType, pattern);
                    }

                    var parameters = deconstruction.DeconstructMethod.Parameters;
                    var offset = GetDeconstructParameterOffset(deconstruction.DeconstructMethod);
                    for (var i = 0; i < deconstruction.Arguments.Length; i++)
                    {
                        if (FindRefutableParameterPattern(parameters[i + offset].Type, deconstruction.Arguments[i]) is { } failure)
                            return failure;
                    }

                    return null;
                }
            case BoundDictionaryPattern dictionary:
                // Ordinary dictionary types do not guarantee that any particular
                // key exists. An empty pattern imposes no key requirement.
                return CanBeNull(inputType) || !dictionary.Entries.IsEmpty ? (inputType, pattern) : null;
            default:
                return IsTotalPattern(inputType, pattern) ? null : (inputType, pattern);
        }
    }
}

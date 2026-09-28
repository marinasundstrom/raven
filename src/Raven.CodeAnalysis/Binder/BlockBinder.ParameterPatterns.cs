using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal partial class BlockBinder
{
    internal ImmutableArray<BoundStatement> BindNamedParameterPatterns(IMethodSymbol method)
    {
        var statements = ImmutableArray.CreateBuilder<BoundStatement>();
        var names = new HashSet<string>(method.Parameters.Where(parameter => !parameter.HasImplicitName)
            .Select(parameter => parameter.Name));
        foreach (var parameter in method.Parameters)
        {
            if (parameter.RefKind.IsByRef)
                continue;
            if (parameter.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax() is not ParameterSyntax { Pattern: { } pattern })
                continue;

            foreach (var designation in pattern.DescendantNodesAndSelf().OfType<SingleVariableDesignationSyntax>())
            {
                var name = designation.Identifier.ValueText;
                if (name != "_" && !names.Add(name))
                    _diagnostics.ReportVariableAlreadyDefined(name, designation.GetLocation());
            }

            var assignment = BindPatternAssignment(pattern, new BoundParameterAccess(parameter), pattern, SyntaxKind.ValKeyword);
            if (assignment is BoundPatternAssignmentExpression patternAssignment)
                ValidateParameterPattern(pattern, parameter.Type, patternAssignment.Pattern);
            statements.Add(new BoundExpressionStatement(assignment));
        }

        return statements.ToImmutable();
    }

    private BoundPattern BindNominalParameterPattern(
        NominalDeconstructionPatternSyntax syntax, ITypeSymbol inputType, SyntaxKind bindingKeyword)
    {
        var previousKeyword = _ambientPatternDeclarationBindingKeyword;
        _ambientPatternDeclarationBindingKeyword = bindingKeyword;
        try
        {
            return BindPattern(syntax, inputType);
        }
        finally
        {
            _ambientPatternDeclarationBindingKeyword = previousKeyword;
        }
    }

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
        {
            if (!CanEmitParameterDeconstruction(pattern))
                _diagnostics.ReportParameterPatternContextNotSupported("with this nested pattern form", syntax.GetLocation());
            return;
        }

        var failureSyntax = syntax.DescendantNodesAndSelf().OfType<PatternSyntax>()
            .FirstOrDefault(candidate => ReferenceEquals(TryGetCachedBoundNode(candidate), failure.Pattern))
            ?? syntax;

        _diagnostics.ReportRefutableParameterPattern(
            failure.InputType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
            failureSyntax.GetLocation());
    }

    private static bool CanEmitParameterDeconstruction(BoundPattern pattern)
        => pattern switch
        {
            BoundDeclarationPattern or BoundDiscardPattern => true,
            BoundPositionalPattern positional => positional.Elements.All(CanEmitParameterDeconstruction),
            BoundDeconstructPattern deconstruction => deconstruction.Arguments.All(CanEmitParameterDeconstruction),
            BoundDictionaryPattern dictionary => dictionary.Entries.All(entry => CanEmitParameterDeconstruction(entry.Pattern)),
            _ => false
        };

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
            case BoundPropertyPattern property:
                {
                    if (CanBeNull(inputType) ||
                        property.NarrowedType is { } narrowedType && !IsAssignable(narrowedType, inputType, out _))
                    {
                        return (inputType, pattern);
                    }

                    foreach (var member in property.Properties)
                    {
                        if (FindRefutableParameterPattern(member.Type, member.Pattern) is { } failure)
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

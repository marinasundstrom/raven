using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal partial class BoundIsPatternExpression : BoundExpression
{
    public BoundExpression Expression { get; }
    public BoundPattern Pattern { get; }
    public ITypeSymbol BooleanType { get; }

    public BoundIsPatternExpression(
        BoundExpression expression,
        BoundPattern pattern,
        ITypeSymbol booleanType,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(booleanType, null, reason)
    {
        BooleanType = booleanType;
        Expression = expression;
        Pattern = pattern;
    }
}

internal abstract class BoundPattern : BoundExpression
{
    public BoundPattern(ITypeSymbol type, BoundExpressionReason reason = BoundExpressionReason.None) : base(type, null, reason)
    {

    }

    public virtual IEnumerable<BoundDesignator> GetDesignators() => [];
}

internal abstract class BoundUnaryPattern : BoundPattern
{
    public BoundPattern Pattern { get; }

    public BoundUnaryPatternKind Kind { get; }

    public BoundUnaryPattern(BoundUnaryPatternKind kind, BoundPattern pattern)
        : base(pattern.Type)
    {
        Kind = kind;
        Pattern = pattern;
    }
}

enum BoundUnaryPatternKind
{
    Not
}

internal sealed partial class BoundNotPattern : BoundUnaryPattern
{
    public BoundNotPattern(BoundPattern pattern)
        : base(BoundUnaryPatternKind.Not, pattern)
    {

    }
}

internal abstract class BoundBinaryPattern : BoundPattern
{
    public BoundBinaryPattern(BoundPatternKind kind, BoundPattern left, BoundPattern right)
    : base(left.Type) // Typval kan justeras
    {
        Kind = kind;
        Left = left;
        Right = right;
    }

    public BoundPatternKind Kind { get; set; }
    public BoundPattern Left { get; set; }
    public BoundPattern Right { get; set; }
}

enum BoundPatternKind
{
    And,
    Or
}

internal sealed partial class BoundAndPattern : BoundBinaryPattern
{
    public BoundAndPattern(BoundPattern left, BoundPattern right)
        : base(BoundPatternKind.And, left, right) // Typval kan justeras
    {
        Left = left;
        Right = right;
    }
}

internal sealed partial class BoundOrPattern : BoundBinaryPattern
{
    public BoundOrPattern(BoundPattern left, BoundPattern right)
        : base(BoundPatternKind.Or, left, right) // Typval kan justeras
    {
        Left = left;
        Right = right;
    }
}

internal partial class BoundDeclarationPattern : BoundPattern
{
    public ITypeSymbol DeclaredType { get; }
    public BoundDesignator Designator { get; }

    public BoundDeclarationPattern(
        ITypeSymbol declaredType,
        BoundDesignator designator,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(declaredType, reason)
    {
        DeclaredType = declaredType;
        Designator = designator;
    }

    public override IEnumerable<BoundDesignator> GetDesignators()
    {
        if (Designator is not BoundDiscardDesignator)
            yield return Designator;
    }
}

internal sealed class BoundTuplePattern : BoundPattern
{
    public BoundTuplePattern(
        ITypeSymbol tupleType,
        ImmutableArray<BoundPattern> elements,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(tupleType, reason)
    {
        Elements = elements;
    }

    public ImmutableArray<BoundPattern> Elements { get; }

    public override IEnumerable<BoundDesignator> GetDesignators()
    {
        foreach (var element in Elements)
        {
            foreach (var designator in element.GetDesignators())
                yield return designator;
        }
    }

    public override void Accept(BoundTreeVisitor visitor)
    {
        visitor.DefaultVisit(this);
    }

    public override TResult Accept<TResult>(BoundTreeVisitor<TResult> visitor)
    {
        return visitor.DefaultVisit(this);
    }
}

internal sealed class BoundUnionCasePattern : BoundPattern
{
    public BoundUnionCasePattern(
        INamedTypeSymbol unionType,
        INamedTypeSymbol caseType,
        IMethodSymbol tryGetMethod,
        ImmutableArray<IFieldSymbol> payloadFields,
        ImmutableArray<BoundPattern> arguments)
        : base(unionType)
    {
        UnionType = unionType;
        CaseType = caseType;
        TryGetMethod = tryGetMethod;
        PayloadFields = payloadFields;
        Arguments = arguments;
    }

    public INamedTypeSymbol UnionType { get; }

    public INamedTypeSymbol CaseType { get; }

    public IMethodSymbol TryGetMethod { get; }

    public ImmutableArray<IFieldSymbol> PayloadFields { get; }

    public ImmutableArray<BoundPattern> Arguments { get; }

    public override IEnumerable<BoundDesignator> GetDesignators()
    {
        foreach (var argument in Arguments)
        {
            foreach (var designator in argument.GetDesignators())
                yield return designator;
        }
    }

    public override void Accept(BoundTreeVisitor visitor)
    {
        visitor.DefaultVisit(this);
    }

    public override TResult Accept<TResult>(BoundTreeVisitor<TResult> visitor)
    {
        return visitor.DefaultVisit(this);
    }
}

internal sealed class BoundConstantPattern : BoundPattern
{
    public BoundConstantPattern(LiteralTypeSymbol literalType, BoundExpressionReason reason = BoundExpressionReason.None)
        : base(literalType, reason)
    {
        LiteralType = literalType;
    }

    public LiteralTypeSymbol LiteralType { get; }

    public object ConstantValue => LiteralType.ConstantValue;

    public override void Accept(BoundTreeVisitor visitor)
    {
        visitor.DefaultVisit(this);
    }

    public override TResult Accept<TResult>(BoundTreeVisitor<TResult> visitor)
    {
        return visitor.DefaultVisit(this);
    }
}

internal sealed class BoundDiscardPattern : BoundPattern
{
    public BoundDiscardPattern(ITypeSymbol type, BoundExpressionReason reason = BoundExpressionReason.None)
        : base(type, reason)
    {
    }

    public override void Accept(BoundTreeVisitor visitor)
    {
        visitor.DefaultVisit(this);
    }

    public override TResult Accept<TResult>(BoundTreeVisitor<TResult> visitor)
    {
        return visitor.DefaultVisit(this);
    }
}

internal abstract class BoundDesignator : BoundExpression
{
    protected BoundDesignator(ITypeSymbol type, ISymbol? symbol = null, BoundExpressionReason reason = BoundExpressionReason.None) : base(type, symbol, reason)
    {
    }
}

internal partial class BoundSingleVariableDesignator : BoundDesignator
{
    public BoundSingleVariableDesignator(ILocalSymbol local, BoundExpressionReason reason = BoundExpressionReason.None)
        : base(local.Type, local, reason)
    {
        Local = local;
    }

    public ILocalSymbol Local { get; set; }
}

internal sealed class BoundDiscardDesignator : BoundDesignator
{
    public BoundDiscardDesignator(ITypeSymbol type, BoundExpressionReason reason = BoundExpressionReason.None)
        : base(type, null, reason)
    {
    }

    public override void Accept(BoundTreeVisitor visitor)
    {
        visitor.DefaultVisit(this);
    }

    public override TResult Accept<TResult>(BoundTreeVisitor<TResult> visitor)
    {
        return visitor.DefaultVisit(this);
    }
}

internal partial class BlockBinder
{
    public virtual BoundPattern BindPattern(PatternSyntax syntax, ITypeSymbol? expectedType = null)
    {
        return syntax switch
        {
            DiscardPatternSyntax discard => BindDiscardPattern(discard),
            VariablePatternSyntax variable => BindVariablePattern(variable),
            DeclarationPatternSyntax d => BindDeclarationPattern(d),
            TuplePatternSyntax t => BindTuplePattern(t, expectedType),
            UnaryPatternSyntax u => BindUnaryPattern(u, expectedType),
            BinaryPatternSyntax b => BindBinaryPattern(b, expectedType),
            TargetMemberPatternSyntax targetMember => BindTargetMemberPattern(targetMember, expectedType),
            _ => throw new NotImplementedException($"Unknown pattern kind: {syntax.Kind}")
        };
    }

    private BoundPattern BindDeclarationPattern(DeclarationPatternSyntax syntax)
    {
        var type = BindTypeSyntax(syntax.Type);

        BoundDesignator designator = syntax.Designation switch
        {
            SingleVariableDesignationSyntax single when !single.Identifier.IsMissing &&
                                                      single.Identifier.Kind != SyntaxKind.None &&
                                                      !string.IsNullOrEmpty(single.Identifier.ValueText) &&
                                                      single.Identifier.ValueText != "_"
                => BindSingleVariableDesignation(single)!,
            _ => new BoundDiscardDesignator(type.Type)
        };

        if (type is BoundTypeExpression { TypeSymbol: LiteralTypeSymbol literalType } &&
            designator is BoundDiscardDesignator)
        {
            return new BoundConstantPattern(literalType);
        }

        if (type is BoundTypeExpression { TypeSymbol: NullTypeSymbol } &&
            designator is BoundDiscardDesignator)
        {
            var objectType = Compilation.GetSpecialType(SpecialType.System_Object);
            var nullLiteralType = new LiteralTypeSymbol(objectType, constantValue: null!, Compilation);
            return new BoundConstantPattern(nullLiteralType);
        }

        return new BoundDeclarationPattern(type.Type, designator);
    }

    private BoundPattern BindTargetMemberPattern(TargetMemberPatternSyntax syntax, ITypeSymbol? expectedType)
    {
        var simpleName = syntax.Name;
        var caseIdentifier = simpleName switch
        {
            IdentifierNameSyntax identifier => identifier.Identifier,
            GenericNameSyntax generic => generic.Identifier,
            _ => SyntaxFactory.IdentifierToken(simpleName.GetFirstToken().Text)
        };

        var caseName = caseIdentifier.ValueText;
        var location = simpleName.GetLocation();
        var candidates = GetDiscriminatedUnionCandidates(expectedType);

        if (candidates.IsDefaultOrEmpty)
        {
            var targetDisplay = expectedType is null
                ? "<unknown>"
                : expectedType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
            _diagnostics.ReportUnionCasePatternRequiresDiscriminatedUnion(caseName, targetDisplay, location);
            return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.TypeMismatch);
        }

        var matches = new List<(INamedTypeSymbol UnionType, INamedTypeSymbol CaseType, IMethodSymbol TryGet)>();

        foreach (var unionType in candidates)
        {
            var caseType = unionType.GetMembers(caseName)
                .OfType<INamedTypeSymbol>()
                .FirstOrDefault();

            if (caseType is null)
                continue;

            var tryGetMethod = unionType.GetMembers($"TryGet{caseName}")
                .OfType<IMethodSymbol>()
                .FirstOrDefault();

            if (tryGetMethod is null)
                continue;

            matches.Add((unionType, caseType, tryGetMethod));
        }

        if (matches.Count == 0)
        {
            foreach (var unionType in candidates)
            {
                var unionDisplay = unionType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
                _diagnostics.ReportUnionCasePatternCaseNotFound(unionDisplay, caseName, location);
            }

            return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.TypeMismatch);
        }

        if (matches.Count > 1)
        {
            _diagnostics.ReportUnionCasePatternAmbiguous(caseName, location);
            return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.TypeMismatch);
        }

        var (unionSymbol, caseSymbol, tryGet) = matches[0];

        if (syntax.Name is GenericNameSyntax genericName)
        {
            var typeArguments = TryBindTypeArguments(genericName);
            if (typeArguments is null)
                return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.TypeMismatch);

            if (caseSymbol.TypeParameters.Length != typeArguments.Value.Length)
            {
                _diagnostics.ReportTypeRequiresTypeArguments(caseSymbol.Name, caseSymbol.TypeParameters.Length, genericName.Identifier.GetLocation());
                return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.TypeMismatch);
            }

            caseSymbol = (INamedTypeSymbol)caseSymbol.Construct(typeArguments.Value.ToArray());

            if (tryGet.TypeParameters.Length == typeArguments.Value.Length)
                tryGet = tryGet.Construct(typeArguments.Value.ToArray());
        }
        else if (caseSymbol.TypeParameters.Length > 0)
        {
            _diagnostics.ReportTypeRequiresTypeArguments(caseSymbol.Name, caseSymbol.TypeParameters.Length, location);
            return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.TypeMismatch);
        }

        var payloadMembers = GetUnionCaseFields(caseSymbol);
        ImmutableArray<BoundPattern> boundArguments;

        if (syntax.ArgumentList is null)
        {
            boundArguments = ImmutableArray<BoundPattern>.Empty;
        }
        else
        {
            var arguments = syntax.ArgumentList.Arguments;

            if (arguments.Count != payloadMembers.Length)
            {
                _diagnostics.ReportUnionCasePatternArgumentCountMismatch(caseName, payloadMembers.Length, arguments.Count, syntax.ArgumentList.GetLocation());
                return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.TypeMismatch);
            }

            var builder = ImmutableArray.CreateBuilder<BoundPattern>(arguments.Count);

            for (var i = 0; i < arguments.Count; i++)
            {
                var fieldType = payloadMembers[i].Type;
                var argumentPattern = BindPattern(arguments[i].Pattern, fieldType);
                builder.Add(argumentPattern);
            }

            boundArguments = builder.MoveToImmutable();
        }

        return new BoundUnionCasePattern(unionSymbol, caseSymbol, tryGet, payloadMembers, boundArguments);
    }

    private BoundPattern BindTuplePattern(TuplePatternSyntax syntax, ITypeSymbol? expectedType)
    {
        var elementPatterns = ImmutableArray.CreateBuilder<BoundPattern>(syntax.Elements.Count);
        var elementTypes = expectedType is null
            ? ImmutableArray<ITypeSymbol>.Empty
            : GetTupleElementTypes(expectedType);

        foreach (var elementSyntax in syntax.Elements)
        {
            var elementIndex = elementPatterns.Count;
            ITypeSymbol? elementExpectedType = null;
            if (!elementTypes.IsDefaultOrEmpty && elementTypes.Length > elementIndex)
                elementExpectedType = elementTypes[elementIndex];

            var boundElement = BindPattern(elementSyntax.Pattern, elementExpectedType);
            boundElement = BindTuplePatternElementDesignation(elementSyntax, boundElement);
            elementPatterns.Add(boundElement);
        }

        var tupleElements = new List<(string? name, ITypeSymbol type)>(elementPatterns.Count);
        for (var i = 0; i < elementPatterns.Count; i++)
        {
            var element = elementPatterns[i];
            var elementSyntax = syntax.Elements[i];
            var elementType = element.Type ?? Compilation.ErrorTypeSymbol;
            string? elementName = null;

            if (elementSyntax.NameColon is { Name: IdentifierNameSyntax identifier } &&
                !identifier.Identifier.IsMissing)
            {
                elementName = identifier.Identifier.ValueText;
            }

            tupleElements.Add((elementName, elementType));
        }

        var tupleType = Compilation.CreateTupleTypeSymbol(tupleElements);

        return new BoundTuplePattern(tupleType, elementPatterns.ToImmutable());
    }

    private BoundPattern BindVariablePattern(VariablePatternSyntax syntax)
    {
        var isMutable = syntax.BindingKeyword.IsKind(SyntaxKind.VarKeyword);
        return BindVariableDesignation(syntax.Designation, isMutable, expectedType: null);
    }

    private BoundPattern BindVariableDesignation(
        VariableDesignationSyntax designation,
        bool isMutable,
        ITypeSymbol? expectedType)
    {
        expectedType ??= Compilation.GetSpecialType(SpecialType.System_Object);

        return designation switch
        {
            SingleVariableDesignationSyntax single => BindVariableDesignation(single, isMutable, expectedType),
            ParenthesizedVariableDesignationSyntax parenthesized => BindVariableDesignation(parenthesized, isMutable, expectedType),
            TypedVariableDesignationSyntax typed => BindTypedVariableDesignation(typed, isMutable, expectedType),
            _ => new BoundDiscardPattern(expectedType)
        };
    }

    private BoundPattern BindTypedVariableDesignation(
        TypedVariableDesignationSyntax typedDesignation,
        bool isMutable,
        ITypeSymbol? expectedType)
    {
        var declaredType = ResolveType(typedDesignation.TypeAnnotation.Type);
        declaredType = EnsureTypeAccessible(declaredType, typedDesignation.TypeAnnotation.Type.GetLocation());

        return BindVariableDesignation(typedDesignation.Designation, isMutable, declaredType);
    }

    private BoundPattern BindVariableDesignation(
        SingleVariableDesignationSyntax single,
        bool isMutable,
        ITypeSymbol expectedType)
    {
        var normalizedType = TypeSymbolNormalization.NormalizeForInference(expectedType);

        if (single.Identifier.IsMissing || single.Identifier.ValueText == "_")
        {
            var discardType = normalizedType.TypeKind == TypeKind.Error
                ? Compilation.ErrorTypeSymbol
                : normalizedType;

            return new BoundDiscardPattern(discardType);
        }

        var type = normalizedType.TypeKind == TypeKind.Error
            ? Compilation.ErrorTypeSymbol
            : normalizedType;

        type = EnsureTypeAccessible(type, single.Identifier.GetLocation());

        var local = CreateLocalSymbol(single, single.Identifier.ValueText, isMutable, type);
        var designator = new BoundSingleVariableDesignator(local);

        return new BoundDeclarationPattern(type, designator);
    }

    private BoundPattern BindVariableDesignation(
        ParenthesizedVariableDesignationSyntax parenthesized,
        bool isMutable,
        ITypeSymbol expectedType)
    {
        var variables = parenthesized.Variables;
        var elementTypes = GetTupleElementTypes(expectedType);

        var boundElements = ImmutableArray.CreateBuilder<BoundPattern>(variables.Count);

        for (var i = 0; i < variables.Count; i++)
        {
            var variable = variables[i];
            var elementType = elementTypes.Length > i
                ? elementTypes[i]
                : Compilation.GetSpecialType(SpecialType.System_Object);

            var boundElement = BindVariableDesignation(variable, isMutable, elementType);
            boundElements.Add(boundElement);
        }

        var tupleElements = new List<(string? name, ITypeSymbol type)>(boundElements.Count);

        foreach (var element in boundElements)
        {
            var elementType = element.Type ?? Compilation.ErrorTypeSymbol;
            tupleElements.Add((null, elementType));
        }

        var tupleType = Compilation.CreateTupleTypeSymbol(tupleElements);

        return new BoundTuplePattern(tupleType, boundElements.ToImmutable());
    }

    private ImmutableArray<INamedTypeSymbol> GetDiscriminatedUnionCandidates(ITypeSymbol? expectedType)
    {
        if (expectedType is null)
            return ImmutableArray<INamedTypeSymbol>.Empty;

        var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
        CollectDiscriminatedUnionCandidates(builder, expectedType);
        return builder.ToImmutable();
    }

    private void CollectDiscriminatedUnionCandidates(ImmutableArray<INamedTypeSymbol>.Builder builder, ITypeSymbol type)
    {
        type = UnwrapAlias(type);

        if (type is IUnionTypeSymbol unionType)
        {
            foreach (var member in unionType.Types)
                CollectDiscriminatedUnionCandidates(builder, member);
            return;
        }

        if (type is INamedTypeSymbol named && IsDiscriminatedUnion(named))
        {
            if (!builder.Any(candidate => SymbolEqualityComparer.Default.Equals(candidate, named)))
                builder.Add(named);
        }
    }

    private static bool TryGetDiscriminatedUnionCaseTypes(ITypeSymbol type, out ImmutableArray<INamedTypeSymbol> caseTypes)
    {
        caseTypes = ImmutableArray<INamedTypeSymbol>.Empty;
        type = UnwrapAlias(type);

        if (type is not INamedTypeSymbol named || !IsDiscriminatedUnion(named))
            return false;

        var cases = named.GetMembers()
            .OfType<INamedTypeSymbol>()
            .ToImmutableArray();

        if (cases.IsDefaultOrEmpty || cases.Length == 0)
            return false;

        caseTypes = cases;
        return true;
    }

    private static ImmutableArray<IFieldSymbol> GetUnionCaseFields(INamedTypeSymbol caseType)
    {
        return caseType.GetMembers()
            .OfType<IFieldSymbol>()
            .Where(field => !field.IsStatic)
            .ToImmutableArray();
    }

    private static bool IsDiscriminatedUnion(INamedTypeSymbol type)
    {
        if (type is SourceNamedTypeSymbol source && source.IsUnionDeclaration)
            return true;

        if (type.OriginalDefinition is SourceNamedTypeSymbol originalSource && originalSource.IsUnionDeclaration)
            return true;

        foreach (var attribute in type.GetAttributes())
        {
            if (string.Equals(attribute.AttributeClass?.Name, "DiscriminatedUnionAttribute", StringComparison.Ordinal))
                return true;
        }

        return false;
    }

    private BoundPattern BindTuplePatternElementDesignation(TuplePatternElementSyntax elementSyntax, BoundPattern pattern)
    {
        if (elementSyntax.NameColon is null)
            return pattern;

        var identifier = elementSyntax.NameColon.Name.Identifier;
        if (identifier.IsMissing)
            return pattern;

        if (pattern is not BoundDeclarationPattern declaration || declaration.Designator is not BoundDiscardDesignator)
            return pattern;

        var local = CreateLocalSymbol(elementSyntax.NameColon.Name, identifier.ValueText, isMutable: false, declaration.DeclaredType);
        var designator = new BoundSingleVariableDesignator(local);

        return new BoundDeclarationPattern(declaration.DeclaredType, designator, declaration.Reason);
    }

    private BoundPattern BindDiscardPattern(DiscardPatternSyntax syntax)
    {
        var objectType = Compilation.GetSpecialType(SpecialType.System_Object);
        return new BoundDiscardPattern(objectType);
    }

    private BoundPattern BindUnaryPattern(UnaryPatternSyntax syntax, ITypeSymbol? expectedType)
    {
        var operand = BindPattern(syntax.Pattern, expectedType);
        return syntax.Kind switch
        {
            SyntaxKind.NotPattern => new BoundNotPattern(operand),
            _ => throw new NotImplementedException($"Unsupported unary pattern: {syntax.Kind}")
        };
    }

    private BoundPattern BindBinaryPattern(BinaryPatternSyntax syntax, ITypeSymbol? expectedType)
    {
        var left = BindPattern(syntax.Left, expectedType);
        var right = BindPattern(syntax.Right, expectedType);

        return syntax.Kind switch
        {
            SyntaxKind.AndPattern => new BoundAndPattern(left, right),
            SyntaxKind.OrPattern => new BoundOrPattern(left, right),
            _ => throw new NotImplementedException($"Unsupported binary pattern: {syntax.Kind}")
        };
    }
}

internal partial class BlockBinder
{
    private BoundExpression BindIsPatternExpression(IsPatternExpressionSyntax syntax)
    {
        var expression = BindExpression(syntax.Expression);
        var pattern = BindPattern(syntax.Pattern, expression.Type);

        var booleanType = Compilation.GetSpecialType(SpecialType.System_Boolean);

        return new BoundIsPatternExpression(expression, pattern, booleanType);
    }

    private BoundSingleVariableDesignator? BindSingleVariableDesignation(SingleVariableDesignationSyntax singleVariableDesignation)
    {
        if (singleVariableDesignation.Identifier.IsMissing)
            return null;

        var name = singleVariableDesignation.Identifier.ValueText;
        if (string.IsNullOrEmpty(name) || name == "_")
            return null;

        var (typeSyntax, isMutable) = GetPatternDesignationContext(singleVariableDesignation);
        var type = typeSyntax is null
            ? Compilation.GetSpecialType(SpecialType.System_Object)
            : ResolveType(typeSyntax);

        var local = CreateLocalSymbol(singleVariableDesignation, name, isMutable, type);

        return new BoundSingleVariableDesignator(local);
    }

    private (TypeSyntax? Type, bool IsMutable) GetPatternDesignationContext(SingleVariableDesignationSyntax designation)
    {
        TypeSyntax? typeSyntax = null;
        var isMutable = false;

        for (SyntaxNode? node = designation.Parent; node is not null; node = node.Parent)
        {
            switch (node)
            {
                case DeclarationPatternSyntax declarationPattern:
                    typeSyntax ??= declarationPattern.Type;
                    return (typeSyntax, isMutable);
                case TypedVariableDesignationSyntax typedDesignation:
                    typeSyntax ??= typedDesignation.TypeAnnotation.Type;
                    break;
                case VariablePatternSyntax variablePattern:
                    if (variablePattern.BindingKeyword.IsKind(SyntaxKind.VarKeyword))
                        isMutable = true;
                    break;
            }
        }

        return (typeSyntax, isMutable);
    }
}

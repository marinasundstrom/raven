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

internal sealed class BoundCasePattern : BoundPattern
{
    public BoundCasePattern(
        IDiscriminatedUnionCaseSymbol caseSymbol,
        IMethodSymbol tryGetMethod,
        ImmutableArray<BoundPattern> arguments,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(caseSymbol, reason)
    {
        CaseSymbol = caseSymbol;
        TryGetMethod = tryGetMethod;
        Arguments = arguments;
    }

    public IDiscriminatedUnionCaseSymbol CaseSymbol { get; }

    public IMethodSymbol TryGetMethod { get; }

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
    public virtual BoundPattern BindPattern(PatternSyntax syntax, ITypeSymbol? inputType = null)
    {
        var bound = syntax switch
        {
            DiscardPatternSyntax discard => BindDiscardPattern(discard),
            ConstantPatternSyntax constant => BindConstantPattern(constant),
            VariablePatternSyntax variable => BindVariablePattern(variable, inputType),
            DeclarationPatternSyntax d => BindDeclarationPattern(d),
            TuplePatternSyntax t => BindTuplePattern(t, inputType),
            UnaryPatternSyntax u => BindUnaryPattern(u, inputType),
            BinaryPatternSyntax b => BindBinaryPattern(b, inputType),
            CasePatternSyntax c => BindCasePattern(c, inputType),
            PropertyPatternSyntax p => BindPropertyPattern(p, inputType),
            _ => throw new NotImplementedException($"Unknown pattern kind: {syntax.Kind}")
        };

        CacheBoundNode(syntax, bound);
        return bound;
    }

    private BoundPattern BindConstantPattern(ConstantPatternSyntax syntax)
    {
        /*var type = BindTypeSyntax(syntax.Expression);

        if (type is BoundTypeExpression { TypeSymbol: LiteralTypeSymbol literalType })
        {
            return new BoundConstantPattern(literalType);
        }*/

        throw new Exception();
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

        if (designator is BoundDiscardDesignator)
            CacheBoundNode(syntax.Designation, designator);

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

    private BoundPattern BindTuplePattern(TuplePatternSyntax syntax, ITypeSymbol? inputType)
    {
        var elementPatterns = ImmutableArray.CreateBuilder<BoundPattern>(syntax.Elements.Count);

        var elementTypes = inputType is null
            ? ImmutableArray<ITypeSymbol>.Empty
            : GetTupleElementTypes(inputType);

        for (var i = 0; i < syntax.Elements.Count; i++)
        {
            var elementSyntax = syntax.Elements[i];
            var expectedElementType = elementTypes.Length > i
                ? elementTypes[i]
                : null;

            var boundElement = BindTuplePatternElement(elementSyntax.Pattern, expectedElementType);
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

    private BoundPattern BindTuplePatternElement(PatternSyntax syntax, ITypeSymbol? expectedType)
    {
        expectedType ??= Compilation.GetSpecialType(SpecialType.System_Object);

        // Enables (a, b) to bind locals like implicit payload designations.
        if (TryBindImplicitPayloadDesignation(syntax, expectedType) is { } implicitDesignation)
            return implicitDesignation;

        return BindPattern(syntax, expectedType);
    }

    private BoundPattern BindVariablePattern(VariablePatternSyntax syntax, ITypeSymbol? expectedType)
    {
        var isMutable = syntax.BindingKeyword.IsKind(SyntaxKind.VarKeyword);
        return BindVariableDesignation(syntax.Designation, isMutable, expectedType);
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

    private BoundPattern BindUnaryPattern(UnaryPatternSyntax syntax, ITypeSymbol? inputType)
    {
        var operand = BindPattern(syntax.Pattern, inputType);
        return syntax.Kind switch
        {
            SyntaxKind.NotPattern => new BoundNotPattern(operand),
            _ => throw new NotImplementedException($"Unsupported unary pattern: {syntax.Kind}")
        };
    }

    private BoundPattern BindBinaryPattern(BinaryPatternSyntax syntax, ITypeSymbol? inputType)
    {
        var left = BindPattern(syntax.Left, inputType);
        var right = BindPattern(syntax.Right, inputType);

        return syntax.Kind switch
        {
            SyntaxKind.AndPattern => new BoundAndPattern(left, right),
            SyntaxKind.OrPattern => new BoundOrPattern(left, right),
            _ => throw new NotImplementedException($"Unsupported binary pattern: {syntax.Kind}")
        };
    }

    private BoundPattern BindCasePattern(CasePatternSyntax syntax, ITypeSymbol? inputType)
    {
        var qualifierType = syntax.Path.Qualifier is null
            ? null
            : ResolveType(syntax.Path.Qualifier);

        var lookupType = qualifierType ?? inputType;

        if (lookupType is null)
        {
            _diagnostics.ReportCasePatternRequiresDiscriminatedUnion(
                syntax.Path.Identifier.ValueText,
                syntax.GetLocation());
            return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.MissingType);
        }

        var unionType = lookupType.TryGetDiscriminatedUnion()
            ?? lookupType.TryGetDiscriminatedUnionCase()?.Union;

        if (unionType is null)
        {
            _diagnostics.ReportCasePatternRequiresDiscriminatedUnion(
                syntax.Path.Identifier.ValueText,
                syntax.GetLocation());
            return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.TypeMismatch);
        }

        var caseName = syntax.Path.Identifier.ValueText;
        var caseSymbol = unionType.Cases.FirstOrDefault(c => c.Name == caseName);

        if (caseSymbol is null)
        {
            _diagnostics.ReportCasePatternCaseNotFound(
                caseName,
                unionType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                syntax.Path.Identifier.GetLocation());
            return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.NotFound);
        }

        var tryGetMethodName = $"TryGet{caseSymbol.Name}";
        var tryGetMethod = FindTryGetMethod(tryGetMethodName, lookupType, unionType, caseSymbol);

        if (tryGetMethod is null)
        {
            return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.NotFound);
        }

        var parameters = caseSymbol.ConstructorParameters;
        var argumentList = syntax.ArgumentList;
        var argumentCount = argumentList?.Arguments.Count ?? 0;

        if (argumentCount != parameters.Length)
        {
            _diagnostics.ReportCasePatternArgumentCountMismatch(
                caseSymbol.Name,
                parameters.Length,
                argumentCount,
                syntax.ArgumentList?.GetLocation() ?? syntax.Path.GetLocation());
        }

        var boundArguments = ImmutableArray.CreateBuilder<BoundPattern>(Math.Max(parameters.Length, argumentCount));
        var elementCount = Math.Min(parameters.Length, argumentCount);

        for (var i = 0; i < elementCount; i++)
        {
            var argumentSyntax = argumentList!.Arguments[i];
            var boundArgument = BindCasePatternArgument(argumentSyntax, parameters[i].Type);
            boundArguments.Add(boundArgument);
        }

        for (var i = elementCount; i < parameters.Length; i++)
        {
            boundArguments.Add(new BoundDiscardPattern(parameters[i].Type, BoundExpressionReason.TypeMismatch));
        }

        for (var i = elementCount; i < argumentCount; i++)
        {
            boundArguments.Add(BindPattern(argumentList!.Arguments[i]));
        }

        return new BoundCasePattern(caseSymbol, tryGetMethod, boundArguments.ToImmutable());
    }

    private IMethodSymbol? FindTryGetMethod(
        string methodName,
        ITypeSymbol? lookupType,
        IDiscriminatedUnionSymbol unionType,
        IDiscriminatedUnionCaseSymbol caseSymbol)
    {
        var candidateTypes = new List<INamedTypeSymbol>();

        void AddCandidate(INamedTypeSymbol? candidate)
        {
            if (candidate is null)
                return;

            candidate = (INamedTypeSymbol)UnwrapAlias(candidate);

            if (candidateTypes.Any(t => SymbolEqualityComparer.Default.Equals(t, candidate)))
                return;

            candidateTypes.Add(candidate);

            if (candidate.ConstructedFrom is INamedTypeSymbol constructedFrom &&
                !SymbolEqualityComparer.Default.Equals(constructedFrom, candidate))
            {
                AddCandidate(constructedFrom);
            }
        }

        AddCandidate(unionType);
        AddCandidate(caseSymbol);

        if (lookupType is INamedTypeSymbol namedLookup)
            AddCandidate(namedLookup);

        foreach (var candidate in candidateTypes)
        {
            var method = candidate
                .GetMembers(methodName)
                .OfType<IMethodSymbol>()
                .FirstOrDefault(m => !m.IsStatic);

            if (method is not null)
                return method;
        }

        return null;
    }

    private BoundPattern BindCasePatternArgument(PatternSyntax syntax, ITypeSymbol parameterType)
    {
        if (TryBindImplicitPayloadDesignation(syntax, parameterType) is { } implicitDesignation)
            return implicitDesignation;

        return BindPattern(syntax, parameterType);
    }

    private BoundPattern? TryBindImplicitPayloadDesignation(PatternSyntax syntax, ITypeSymbol parameterType)
    {
        if (parameterType.TypeKind == TypeKind.Error)
            return null;

        if (syntax is DeclarationPatternSyntax
            {
                Type: IdentifierNameSyntax identifierName,
                Designation: SingleVariableDesignationSyntax
                {
                    Identifier: { Kind: SyntaxKind.None }
                },
            })
        {
            var name = identifierName.Identifier.ValueText;

            if (string.IsNullOrEmpty(name) || name == "_")
            {
                var discardType = parameterType.TypeKind == TypeKind.Error
                    ? Compilation.ErrorTypeSymbol
                    : parameterType;

                return new BoundDiscardPattern(discardType);
            }

            parameterType = EnsureTypeAccessible(parameterType, identifierName.GetLocation());

            var local = CreateLocalSymbol(identifierName, name, isMutable: false, parameterType);
            var designator = new BoundSingleVariableDesignator(local);

            return new BoundDeclarationPattern(parameterType, designator);
        }

        return null;
    }

    private BoundPattern BindPropertyPattern(PropertyPatternSyntax syntax, ITypeSymbol? inputType)
    {
        inputType ??= Compilation.GetSpecialType(SpecialType.System_Object);

        ITypeSymbol? narrowedType = null;

        // Empty property pattern: { } matches any non-null scrutinee.
        // It never needs inference and never does member lookup.
        if (syntax.PropertyPatternClause.Properties.Count == 0)
        {
            if (syntax.Type is not null)
            {
                var boundType = BindTypeSyntax(syntax.Type);
                narrowedType = EnsureTypeAccessible(boundType.Type, syntax.Type.GetLocation());

                // Optional: diagnose incompatibility between input and narrowed type
                if (narrowedType.TypeKind != TypeKind.Error &&
                    inputType.TypeKind != TypeKind.Error &&
                    !Compilation.ClassifyConversion(inputType, narrowedType).Exists)
                {
                    _diagnostics.ReportPropertyPatternTypeMismatch(
                        inputType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        narrowedType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        syntax.Type!.GetLocation());
                }
            }

            // ReceiverType is irrelevant for empty patterns; keep it as inputType to avoid ErrorType.
            return new BoundPropertyPattern(
                inputType: inputType,
                receiverType: inputType,
                narrowedType: narrowedType,
                properties: ImmutableArray<BoundPropertySubpattern>.Empty);
        }

        // 1) Bind explicit type filter if present: Foo in Foo { ... }
        if (syntax.Type is not null)
        {
            var boundType = BindTypeSyntax(syntax.Type);
            narrowedType = EnsureTypeAccessible(boundType.Type, syntax.Type.GetLocation());
        }

        // 2) Decide receiver type:
        //    - explicit type: receiver = narrowed
        //    - implicit type: receiver inferred from input type
        ITypeSymbol receiverType;
        if (narrowedType is not null)
        {
            receiverType = narrowedType;

            // Optional: diagnose incompatibility (same as you had)
            if (receiverType.TypeKind != TypeKind.Error &&
                inputType.TypeKind != TypeKind.Error &&
                !Compilation.ClassifyConversion(inputType, receiverType).Exists)
            {
                _diagnostics.ReportPropertyPatternTypeMismatch(
                    inputType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    receiverType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    syntax.Type!.GetLocation());
            }
        }
        else
        {
            receiverType = InferPropertyPatternReceiverType(inputType, syntax);

            if (receiverType.TypeKind == TypeKind.Error)
            {
                var props = BindPropertySubpatternsAsDiscards(syntax);
                return new BoundPropertyPattern(
                    inputType: inputType,
                    receiverType: Compilation.ErrorTypeSymbol,
                    narrowedType: null,
                    properties: props,
                    reason: BoundExpressionReason.MissingType);
            }
        }

        // 3) Bind subpatterns using receiverType for member lookup
        var boundProps = ImmutableArray.CreateBuilder<BoundPropertySubpattern>(
            syntax.PropertyPatternClause.Properties.Count);

        foreach (var sub in syntax.PropertyPatternClause.Properties)
        {
            var name = sub.NameColon.Name.Identifier.ValueText;

            var member = LookupPatternMember(receiverType, name, sub.NameColon.Name.GetLocation());

            if (member is null)
            {
                boundProps.Add(new BoundPropertySubpattern(
                    Member: Compilation.ErrorSymbol,
                    Type: Compilation.ErrorTypeSymbol,
                    Pattern: new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.NotFound)));

                continue;
            }

            ITypeSymbol memberType =
                member is IPropertySymbol p ? p.Type :
                member is IFieldSymbol f ? f.Type :
                Compilation.ErrorTypeSymbol;

            memberType = EnsureTypeAccessible(memberType, sub.GetLocation());

            var boundPattern = BindPattern(sub.Pattern, memberType);

            boundProps.Add(new BoundPropertySubpattern(
                Member: member,
                Type: memberType,
                Pattern: boundPattern));
        }

        return new BoundPropertyPattern(
            inputType: inputType,
            receiverType: receiverType,
            narrowedType: narrowedType,
            properties: boundProps.ToImmutable());
    }

    private ITypeSymbol InferPropertyPatternReceiverType(ITypeSymbol inputType, PropertyPatternSyntax syntax)
    {
        // Strip nullable reference wrapper (object? -> object)
        inputType = StripNullableReference(inputType);

        // Collect required member names from the pattern: { Value: ..., Data: ... }
        var requiredMembers = syntax.PropertyPatternClause.Properties
            .Select(p => p.NameColon.Name.Identifier.ValueText)
            .Where(n => !string.IsNullOrEmpty(n))
            .ToImmutableArray();

        // If the input is a concrete named type (and not object), use it directly
        if (inputType is INamedTypeSymbol named &&
            named.TypeKind != TypeKind.Error &&
            named.SpecialType != SpecialType.System_Object)
        {
            return named;
        }

        /*
        // If it's a union, pick a unique candidate union member that satisfies all members
        if (inputType.IsTypeUnion)
        {
            var unionTypes = inputType.GetTypeUnionTypes(); // <-- use your existing helper for union constituents
            var candidates = new List<INamedTypeSymbol>();

            foreach (var t in unionTypes)
            {
                var tt = StripNullableReference(t);

                if (tt is not INamedTypeSymbol nt)
                    continue;

                if (nt.TypeKind == TypeKind.Error)
                    continue;

                if (nt.SpecialType == SpecialType.System_Object)
                    continue;

                if (HasAllPatternMembers(nt, requiredMembers))
                    candidates.Add(nt);
            }

            if (candidates.Count == 1)
                return candidates[0];

            if (candidates.Count > 1)
            {
                // Ambiguous: multiple possible receiver types
                // You can add a dedicated diagnostic if you want;
                // using TypeMismatch keeps it simple.
                _diagnostics.ReportPropertyPatternTypeMismatch(
                    inputType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    "<inferred>",
                    syntax.GetLocation());

                return Compilation.ErrorTypeSymbol;
            }
        }
        */

        // Otherwise: we can't infer from object / type parameter / etc.
        // This is where your "Should warn" case lands (x: object?).
        _diagnostics.ReportPropertyPatternRequiresType(
            syntax.GetLocation()); // <-- add a WARNING diagnostic for this

        return Compilation.ErrorTypeSymbol;
    }

    private ITypeSymbol StripNullableReference(ITypeSymbol type)
    {
        // If you model nullable reference types with a wrapper symbol, unwrap here.
        // If you model it via annotations, adapt accordingly.
        // Simple fallback: treat "T?" (for ref types) as T when you can detect it.

        if (IsNullableReferenceType(type, out var underlying))
            return underlying;

        return type;
    }

    private bool HasAllPatternMembers(INamedTypeSymbol type, ImmutableArray<string> requiredMembers)
    {
        foreach (var name in requiredMembers)
        {
            var members = type.GetMembers(name);

            // readable instance property (no indexers) OR instance field
            var ok =
                members.OfType<IPropertySymbol>().Any(p => !p.IsStatic && p.GetMethod is not null && p.Parameters.Length == 0) ||
                members.OfType<IFieldSymbol>().Any(f => !f.IsStatic);

            if (!ok)
                return false;
        }

        return true;
    }

    internal static bool IsNullableReferenceType(ITypeSymbol type, out INamedTypeSymbol? underlyingType)
    {
        if (type is NullableTypeSymbol { IsValueType: false })
        {
            underlyingType = (INamedTypeSymbol?)type.GetNullableUnderlyingType();
            return true;
        }
        underlyingType = null;
        return false;
    }

    private ISymbol? LookupPatternMember(ITypeSymbol receiverType, string name, Location location)
    {
        if (receiverType is not INamedTypeSymbol named)
            return null;

        var members = named.GetMembers(name);

        var prop = members.OfType<IPropertySymbol>()
            .FirstOrDefault(p => !p.IsStatic && p.GetMethod is not null && p.Parameters.Length == 0);

        if (prop is not null)
            return prop;

        var field = members.OfType<IFieldSymbol>()
            .FirstOrDefault(f => !f.IsStatic);

        if (field is not null)
            return field;

        _diagnostics.ReportPropertyPatternMemberNotFound(
            name,
            receiverType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
            location);

        return null;
    }

    private ImmutableArray<BoundPropertySubpattern> BindPropertySubpatternsAsDiscards(PropertyPatternSyntax syntax)
    {
        var builder = ImmutableArray.CreateBuilder<BoundPropertySubpattern>(
            syntax.PropertyPatternClause.Properties.Count);

        foreach (var sub in syntax.PropertyPatternClause.Properties)
        {
            builder.Add(new BoundPropertySubpattern(
                Member: Compilation.ErrorSymbol,
                Type: Compilation.ErrorTypeSymbol,
                Pattern: new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.TypeMismatch)));
        }

        return builder.ToImmutable();
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

    private BoundSingleVariableDesignator? BindSingleVariableDesignation(SingleVariableDesignationSyntax single)
    {
        // Not a real symbol
        if (single.Identifier.IsMissing || single.Identifier.ValueText == "_" || string.IsNullOrEmpty(single.Identifier.ValueText))
            return null;

        var name = single.Identifier.ValueText;

        // Walk up to figure out:
        //  - declared type (if any)
        //  - mutability (var vs let/val)
        ITypeSymbol type = Compilation.GetSpecialType(SpecialType.System_Object);
        bool isMutable = false;

        for (SyntaxNode? current = single.Parent; current is not null; current = current.Parent)
        {
            // If there's an explicit type annotation on the designation, it wins.
            if (current is TypedVariableDesignationSyntax typed)
            {
                var declaredType = ResolveType(typed.TypeAnnotation.Type);
                type = EnsureTypeAccessible(declaredType, typed.TypeAnnotation.Type.GetLocation());
                break;
            }

            // Declaration pattern provides the declared type: `T x`
            if (current is DeclarationPatternSyntax decl)
            {
                var declaredType = ResolveType(decl.Type);
                type = EnsureTypeAccessible(declaredType, decl.Type.GetLocation());
                break;
            }

            // Variable pattern provides mutability: `let/val/var x`
            if (current is VariablePatternSyntax vp)
            {
                isMutable = vp.BindingKeyword.IsKind(SyntaxKind.VarKeyword);
                // keep walking in case we later hit a TypedVariableDesignationSyntax,
                // but if we don't, `object` is a fine default for symbol info.
            }

            // Stop climbing once we leave "designation territory" enough.
            // (Optional, but avoids walking too far.)
            if (current is PatternSyntax)
                break;
        }

        type = EnsureTypeAccessible(type, single.Identifier.GetLocation());

        var local = CreateLocalSymbol(single, name, isMutable, type);
        var designator = new BoundSingleVariableDesignator(local);

        CacheBoundNode(single, designator);
        return designator;
    }
}

internal sealed class BoundPropertyPattern : BoundPattern
{
    public BoundPropertyPattern(
        ITypeSymbol inputType,
        ITypeSymbol receiverType,
        ITypeSymbol? narrowedType,
        ImmutableArray<BoundPropertySubpattern> properties,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(inputType, reason)
    {
        InputType = inputType;
        ReceiverType = receiverType;
        NarrowedType = narrowedType;
        Properties = properties;
    }

    public ITypeSymbol InputType { get; }
    public ITypeSymbol ReceiverType { get; }
    public ITypeSymbol? NarrowedType { get; }
    public ImmutableArray<BoundPropertySubpattern> Properties { get; }

    public override IEnumerable<BoundDesignator> GetDesignators()
    {
        foreach (var p in Properties)
            foreach (var d in p.Pattern.GetDesignators())
                yield return d;
    }

    public override void Accept(BoundTreeVisitor visitor) => visitor.DefaultVisit(this);
    public override TResult Accept<TResult>(BoundTreeVisitor<TResult> visitor) => visitor.DefaultVisit(this);
}

internal readonly record struct BoundPropertySubpattern(
    ISymbol Member,
    ITypeSymbol Type,
    BoundPattern Pattern);

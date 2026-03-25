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

internal sealed class BoundGuardedPattern : BoundPattern
{
    public BoundGuardedPattern(
        BoundPattern pattern,
        BoundPattern? guardPattern,
        BoundExpression? guardExpression,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(pattern.Type, reason)
    {
        Pattern = pattern;
        GuardPattern = guardPattern;
        GuardExpression = guardExpression;
    }

    public BoundPattern Pattern { get; }

    public BoundPattern? GuardPattern { get; }

    public BoundExpression? GuardExpression { get; }

    public override IEnumerable<BoundDesignator> GetDesignators() => Pattern.GetDesignators();

    public override void Accept(BoundTreeVisitor visitor)
    {
        visitor.DefaultVisit(this);
    }

    public override TResult Accept<TResult>(BoundTreeVisitor<TResult> visitor)
    {
        return visitor.DefaultVisit(this);
    }
}

internal sealed class BoundCasePattern : BoundPattern
{
    public BoundCasePattern(
        IUnionCaseTypeSymbol caseSymbol,
        IMethodSymbol tryGetMethod,
        ImmutableArray<BoundPattern> arguments,
        BoundDesignator? designator = null,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(caseSymbol, reason)
    {
        CaseSymbol = caseSymbol;
        TryGetMethod = tryGetMethod;
        Arguments = arguments;
        Designator = designator;
    }

    public IUnionCaseTypeSymbol CaseSymbol { get; }

    public IMethodSymbol TryGetMethod { get; }

    public ImmutableArray<BoundPattern> Arguments { get; }
    public BoundDesignator? Designator { get; }

    public override IEnumerable<BoundDesignator> GetDesignators()
    {
        if (Designator is not null && Designator is not BoundDiscardDesignator)
            yield return Designator;

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

internal sealed class BoundUnionMemberPattern : BoundPattern
{
    public BoundUnionMemberPattern(
        ITypeSymbol unionType,
        ITypeSymbol memberType,
        IMethodSymbol tryGetMethod,
        BoundPattern pattern,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(unionType, reason)
    {
        UnionType = unionType;
        MemberType = memberType;
        TryGetMethod = tryGetMethod;
        Pattern = pattern;
    }

    public ITypeSymbol UnionType { get; }
    public ITypeSymbol MemberType { get; }
    public IMethodSymbol TryGetMethod { get; }
    public BoundPattern Pattern { get; }

    public override IEnumerable<BoundDesignator> GetDesignators() => Pattern.GetDesignators();

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

internal sealed class BoundPositionalPattern : BoundPattern
{
    internal enum SequenceElementKind
    {
        Single = 0,
        FixedSegment = 1,
        RestSegment = 2,
    }

    public BoundPositionalPattern(
        ITypeSymbol tupleType,
        ImmutableArray<BoundPattern> elements,
        BoundDesignator? designator = null,
        BoundExpressionReason reason = BoundExpressionReason.None,
        int restIndex = -1,
        ImmutableArray<int> elementWidths = default,
        ImmutableArray<SequenceElementKind> elementKinds = default)
        : base(tupleType, reason)
    {
        Elements = elements;
        Designator = designator;
        RestIndex = restIndex;
        ElementWidths = elementWidths.IsDefaultOrEmpty
            ? ImmutableArray.CreateRange(Enumerable.Repeat(1, elements.Length))
            : elementWidths;
        ElementKinds = elementKinds.IsDefaultOrEmpty
            ? ImmutableArray.CreateRange(Enumerable.Repeat(SequenceElementKind.Single, elements.Length))
            : elementKinds;
    }

    public ImmutableArray<BoundPattern> Elements { get; }
    public BoundDesignator? Designator { get; }
    public int RestIndex { get; }
    public ImmutableArray<int> ElementWidths { get; }
    public ImmutableArray<SequenceElementKind> ElementKinds { get; }

    public override IEnumerable<BoundDesignator> GetDesignators()
    {
        if (Designator is not null && Designator is not BoundDiscardDesignator)
            yield return Designator;

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

internal sealed class BoundDeconstructPattern : BoundPattern
{
    public BoundDeconstructPattern(
        ITypeSymbol inputType,
        ITypeSymbol receiverType,
        ITypeSymbol? narrowedType,
        IMethodSymbol deconstructMethod,
        ImmutableArray<BoundPattern> arguments,
        BoundDesignator? designator = null,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(inputType, reason)
    {
        InputType = inputType;
        ReceiverType = receiverType;
        NarrowedType = narrowedType;
        DeconstructMethod = deconstructMethod;
        Arguments = arguments;
        Designator = designator;
    }

    public ITypeSymbol InputType { get; }
    public ITypeSymbol ReceiverType { get; }
    public ITypeSymbol? NarrowedType { get; }
    public IMethodSymbol DeconstructMethod { get; }
    public ImmutableArray<BoundPattern> Arguments { get; }
    public BoundDesignator? Designator { get; }

    public override IEnumerable<BoundDesignator> GetDesignators()
    {
        if (Designator is not null && Designator is not BoundDiscardDesignator)
            yield return Designator;

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
    // Literal-backed constant pattern (fast path)
    public BoundConstantPattern(
        LiteralTypeSymbol literalType,
        BoundDesignator? designator = null,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(literalType, reason)
    {
        LiteralType = literalType;
        Expression = null;
        Designator = designator;
    }

    // Expression-backed value pattern (e.g. matching against an in-scope variable)
    public BoundConstantPattern(
        BoundExpression expression,
        BoundDesignator? designator = null,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(expression.Type ?? throw new System.ArgumentNullException(nameof(expression.Type)), reason)
    {
        Expression = expression;
        LiteralType = null;
        Designator = designator;
    }

    /// <summary>
    /// When non-null, this constant pattern was produced from a literal and can be treated as a compile-time constant.
    /// </summary>
    public LiteralTypeSymbol? LiteralType { get; }

    /// <summary>
    /// When non-null, this constant pattern compares against a runtime value expression.
    /// </summary>
    public BoundExpression? Expression { get; }
    public BoundDesignator? Designator { get; }

    public object? ConstantValue => LiteralType?.ConstantValue ?? TryGetExpressionConstantValue(Expression);

    public override IEnumerable<BoundDesignator> GetDesignators()
    {
        if (Designator is not null && Designator is not BoundDiscardDesignator)
            yield return Designator;
    }

    private static object? TryGetExpressionConstantValue(BoundExpression? expression)
    {
        return expression switch
        {
            BoundLiteralExpression literal => literal.Value,
            BoundConversionExpression conversion => TryGetExpressionConstantValue(conversion.Expression),
            _ => null
        };
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
        // Disambiguate bare identifiers in pattern position.
        // The parser currently represents a lone identifier in a match arm as a DeclarationPatternSyntax
        // with a missing designation. If the identifier binds as an in-scope value, treat it as a
        // value pattern (expression-backed constant pattern) rather than a type/declaration pattern.
        if (syntax is DeclarationPatternSyntax
            {
                Type: IdentifierNameSyntax identifierType,
                Designation: SingleVariableDesignationSyntax { Identifier: { Kind: SyntaxKind.None } }
            })
        {
            // Prefer DU-case interpretation for bare identifiers in pattern position.
            // Example: `match r { Ok => ...; Error(val e) => ... }`
            var lookupType = inputType;
            var unionType = lookupType?.TryGetUnion()
                ?? lookupType?.TryGetUnionCase()?.Union;

            if (unionType is not null)
            {
                var caseName = identifierType.Identifier.ValueText;
                var caseSymbol = unionType.CaseTypes.FirstOrDefault(c => c.Name == caseName);

                if (caseSymbol is not null)
                {
                    if (caseSymbol is INamedTypeSymbol caseNamed &&
                        unionType is INamedTypeSymbol unionNamed)
                    {
                        caseSymbol = ProjectCaseSymbolToUnionArguments(caseNamed, unionNamed) as IUnionCaseTypeSymbol
                            ?? caseSymbol;
                    }

                    var tryGetMethod = FindTryGetMethod(lookupType!, unionType, caseSymbol);
                    if (tryGetMethod is not null)
                    {
                        var parameters = caseSymbol.ConstructorParameters;
                        var boundArguments = ImmutableArray.CreateBuilder<BoundPattern>(parameters.Length);
                        var implicitUnitArgument = parameters.Length == 1;

                        if (implicitUnitArgument && IsUnitType(parameters[0].Type))
                        {
                            var unitType = Compilation.GetSpecialType(SpecialType.System_Unit);
                            boundArguments.Add(new BoundConstantPattern(new BoundUnitExpression(unitType)));
                        }
                        else
                        {
                            if (parameters.Length != 0)
                            {
                                _diagnostics.ReportCasePatternArgumentCountMismatch(
                                    caseSymbol.Name,
                                    parameters.Length,
                                    0,
                                    identifierType.GetLocation());
                            }

                            for (var i = 0; i < parameters.Length; i++)
                                boundArguments.Add(new BoundDiscardPattern(parameters[i].Type, BoundExpressionReason.TypeMismatch));
                        }

                        var casePattern = new BoundCasePattern(caseSymbol, tryGetMethod, boundArguments.ToImmutable());
                        CacheBoundNode(syntax, casePattern);
                        return casePattern;
                    }
                }
            }

            // Try bind as a value expression first.
            var valueExpr = BindExpression(identifierType);

            // If it resolved to a value symbol, reinterpret as a value/constant pattern.
            if (valueExpr.Symbol is not null && valueExpr.Type is not null && valueExpr.Type.TypeKind != TypeKind.Error)
            {
                var expected = inputType ?? Compilation.GetSpecialType(SpecialType.System_Object);

                var conversion = Compilation.ClassifyConversion(valueExpr.Type, expected);
                if (!conversion.Exists)
                {
                    _diagnostics.ReportMatchExpressionArmPatternInvalid(
                        valueExpr.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        expected.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        identifierType.GetLocation());

                    var discard = new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.TypeMismatch);
                    CacheBoundNode(syntax, discard);
                    return discard;
                }

                var constant = new BoundConstantPattern(valueExpr);
                CacheBoundNode(syntax, constant);
                return constant;
            }
        }

        var bound = syntax switch
        {
            GuardedPatternSyntax guarded => BindGuardedPattern(guarded, inputType),
            DiscardPatternSyntax discard => BindDiscardPattern(discard),
            ConstantPatternSyntax constant => BindConstantPattern(constant, inputType),
            VariablePatternSyntax variable => BindVariablePattern(variable, inputType),
            DeclarationPatternSyntax d => BindDeclarationPattern(d, inputType),
            PositionalPatternSyntax t => BindPositionalPattern(t, inputType),
            SequencePatternSyntax s => BindSequencePattern(s, inputType),
            DictionaryPatternSyntax d => BindDictionaryPattern(d, inputType),
            UnaryPatternSyntax u => BindUnaryPattern(u, inputType),
            BinaryPatternSyntax b => BindBinaryPattern(b, inputType),
            MemberPatternSyntax c => BindCasePattern(c, inputType),
            NominalDeconstructionPatternSyntax r => BindNominalDeconstructionPattern(r, inputType),
            PropertyPatternSyntax p => BindPropertyPattern(p, inputType),
            ComparisonPatternSyntax r => BindComparisonPattern(r, inputType),
            RangePatternSyntax rp => BindRangePattern(rp, inputType?.GetPlainType()),
            _ => throw new NotImplementedException($"Unknown pattern kind: {syntax.Kind}")
        };

        CacheBoundNode(syntax, bound);
        return bound;
    }

    private BoundPattern BindGuardedPattern(GuardedPatternSyntax syntax, ITypeSymbol? inputType)
    {
        var pattern = BindPattern(syntax.Pattern, inputType);

        BoundPattern? guardPattern = null;
        BoundExpression? guardExpression = null;

        switch (syntax.WhenClause.Guard)
        {
            case PatternSyntax patternSyntax:
                guardPattern = BindPattern(patternSyntax, inputType);
                break;

            case ExpressionSyntax expressionSyntax:
                guardExpression = BindBooleanComprehensionCondition(expressionSyntax);
                break;
        }

        return new BoundGuardedPattern(pattern, guardPattern, guardExpression, pattern.Reason);
    }

    private BoundPattern BindComparisonPattern(ComparisonPatternSyntax syntax, ITypeSymbol? inputType)
    {
        inputType ??= Compilation.GetSpecialType(SpecialType.System_Object);
        inputType = inputType.GetPlainType();

        var @operator = syntax.Kind switch
        {
            SyntaxKind.EqualsPattern => BoundComparisonPatternOperator.Equals,
            SyntaxKind.NotEqualsPattern => BoundComparisonPatternOperator.NotEquals,
            SyntaxKind.GreaterThanPattern => BoundComparisonPatternOperator.GreaterThan,
            SyntaxKind.GreaterThanOrEqualPattern => BoundComparisonPatternOperator.GreaterThanOrEqual,
            SyntaxKind.LessThanPattern => BoundComparisonPatternOperator.LessThan,
            SyntaxKind.LessThanOrEqualPattern => BoundComparisonPatternOperator.LessThanOrEqual,
            _ => throw new NotImplementedException($"Unsupported comparison pattern kind: {syntax.Kind}")
        };

        // RHS is an EXPRESSION (already parsed that way)
        var value = BindExpression(syntax.Expression);

        if (inputType.TypeKind == TypeKind.Error || value.Type?.TypeKind == TypeKind.Error)
            return new BoundComparisonPattern(inputType, @operator, value, BoundExpressionReason.TypeMismatch);

        // The pattern compares: (inputType <op> valueConvertedToInputType)
        // so we want RHS to be convertible to inputType.
        if (value.Type is null)
        {
            // Defensive: should not happen often
            return new BoundComparisonPattern(inputType, @operator, value, BoundExpressionReason.MissingType);
        }

        var valueType = value.Type.GetPlainType();
        if (!SymbolEqualityComparer.Default.Equals(valueType, inputType))
        {
            _diagnostics.Report(Diagnostic.Create(
                CompilerDiagnostics.ComparisonPatternCannotConvert,
                syntax.Expression.GetLocation(),
                value.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                inputType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat)));

            return new BoundComparisonPattern(inputType, @operator, value, BoundExpressionReason.TypeMismatch);
        }

        if (IsEqualityOperator(@operator))
            return new BoundComparisonPattern(inputType, @operator, value);

        if (!IsOrderableType(inputType))
        {
            _diagnostics.ReportComparisonPatternNotSupported(
                syntax.OperatorToken,
                inputType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                syntax.GetLocation());

            return new BoundComparisonPattern(inputType, @operator, value, BoundExpressionReason.TypeMismatch);
        }

        return new BoundComparisonPattern(inputType, @operator, value);
    }

    private BoundPattern BindRangePattern(RangePatternSyntax syntax, ITypeSymbol? inputType)
    {
        inputType ??= Compilation.GetSpecialType(SpecialType.System_Object);
        inputType = inputType.GetPlainType();

        if (inputType.TypeKind == TypeKind.Error)
            return new BoundRangePattern(inputType, null, null, reason: BoundExpressionReason.TypeMismatch);

        if (!IsOrderableType(inputType))
        {
            // Relational-pattern diagnostic reused for range patterns.
            var dotDotToken = syntax.DotDotToken;
            _diagnostics.ReportComparisonPatternNotSupported(
                dotDotToken,
                inputType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                syntax.GetLocation());

            return new BoundRangePattern(inputType, null, null, reason: BoundExpressionReason.TypeMismatch);
        }

        BoundExpression? lowerBound = null;
        BoundExpression? upperBound = null;

        if (syntax.LowerBound is not null)
        {
            lowerBound = BindExpression(syntax.LowerBound);
            if (lowerBound.Type is not null && lowerBound.Type.TypeKind != TypeKind.Error)
            {
                var lowerType = lowerBound.Type.GetPlainType();
                if (!SymbolEqualityComparer.Default.Equals(lowerType, inputType))
                {
                    _diagnostics.Report(Diagnostic.Create(
                        CompilerDiagnostics.ComparisonPatternCannotConvert,
                        syntax.LowerBound.GetLocation(),
                        lowerBound.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        inputType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat)));
                    return new BoundRangePattern(inputType, lowerBound, null, reason: BoundExpressionReason.TypeMismatch);
                }
            }
        }

        if (syntax.UpperBound is not null)
        {
            upperBound = BindExpression(syntax.UpperBound);
            if (upperBound.Type is not null && upperBound.Type.TypeKind != TypeKind.Error)
            {
                var upperType = upperBound.Type.GetPlainType();
                if (!SymbolEqualityComparer.Default.Equals(upperType, inputType))
                {
                    _diagnostics.Report(Diagnostic.Create(
                        CompilerDiagnostics.ComparisonPatternCannotConvert,
                        syntax.UpperBound.GetLocation(),
                        upperBound.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        inputType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat)));
                    return new BoundRangePattern(inputType, lowerBound, upperBound, reason: BoundExpressionReason.TypeMismatch);
                }
            }
        }

        return new BoundRangePattern(
            inputType,
            lowerBound,
            upperBound,
            isUpperExclusive: syntax.LessThanToken.Kind == SyntaxKind.LessThanToken);
    }

    private static bool IsOrderableType(ITypeSymbol type)
    {
        // Keep it conservative to start. You can later expand to:
        // - string (ordinal compare?) if you want
        // - IComparable / IComparable<T> if you want
        // - custom operators if Raven supports them
        if (type is null)
            return false;

        if (type.TypeKind == TypeKind.Enum)
            return true;

        return type.SpecialType is
            SpecialType.System_SByte or
            SpecialType.System_Byte or
            SpecialType.System_Int16 or
            SpecialType.System_UInt16 or
            SpecialType.System_Int32 or
            SpecialType.System_UInt32 or
            SpecialType.System_Int64 or
            SpecialType.System_UInt64 or
            SpecialType.System_Single or
            SpecialType.System_Double or
            SpecialType.System_Char;
    }

    private static bool IsEqualityOperator(BoundComparisonPatternOperator @operator)
    {
        return @operator is BoundComparisonPatternOperator.Equals or BoundComparisonPatternOperator.NotEquals;
    }

    private BoundPattern BindConstantPattern(ConstantPatternSyntax syntax, ITypeSymbol? inputType)
    {
        inputType ??= Compilation.GetSpecialType(SpecialType.System_Object);

        var expression = syntax.Expression is MemberBindingExpressionSyntax memberBinding
            ? BindMemberBindingExpression(memberBinding, inputType)
            : BindExpression(syntax.Expression);

        if (expression is BoundErrorExpression)
        {
            // HACK
            var syntax2 = SemanticModel.GetSyntax(expression);
            if (syntax2 is IdentifierNameSyntax identifierName)
            {
                Diagnostics.ReportUndeclaredConstantPatternHint(identifierName.Identifier.ValueText, identifierName.GetLocation());
            }
        }

        return BindConstantPatternFromExpression(expression, syntax.Expression, inputType);
    }

    private BoundPattern BindConstantPatternFromExpression(
        BoundExpression expression,
        ExpressionSyntax expressionSyntax,
        ITypeSymbol inputType,
        BoundDesignator? designator = null)
    {
        // null literal stays a literal-backed constant pattern.
        // NOTE: `null` may be represented either as a null literal expression OR as a `NullType` type-expression
        // depending on how the parser produced the syntax (e.g. `null => ...` in a match arm).
        if (expression is BoundLiteralExpression { Kind: BoundLiteralExpressionKind.NullLiteral }
            or BoundTypeExpression { Type: NullTypeSymbol })
        {
            var nullLiteral = new BoundLiteralExpression(BoundLiteralExpressionKind.NullLiteral, null!, Compilation.NullTypeSymbol);
            return new BoundConstantPattern(nullLiteral, designator);
        }

        // Runtime "value pattern" (identifier/member access/etc.)
        // Ensure the RHS can convert to the input type so codegen can compare meaningfully.
        if (expression.Type is null)
        {
            _diagnostics.ReportMatchExpressionArmPatternInvalid(
                "unknown",
                inputType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                expressionSyntax.GetLocation());

            return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.MissingType);
        }

        if (expression.Type.TypeKind == TypeKind.Error || inputType.TypeKind == TypeKind.Error)
            return new BoundConstantPattern(expression, designator, BoundExpressionReason.TypeMismatch);

        var conversion = Compilation.ClassifyConversion(expression.Type, inputType);
        if (!conversion.Exists)
        {
            _diagnostics.ReportMatchExpressionArmPatternInvalid(
                expression.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                inputType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                expressionSyntax.GetLocation());

            return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.TypeMismatch);
        }

        // NOTE: If you later introduce explicit conversion bound nodes, bind it here so codegen is simpler.
        // expression = BindConversion(expression, inputType, expressionSyntax.GetLocation());

        return new BoundConstantPattern(expression, designator);
    }

    private BoundPattern BindDeclarationPattern(DeclarationPatternSyntax syntax, ITypeSymbol? inputType)
    {
        inputType ??= Compilation.GetSpecialType(SpecialType.System_Object);
        inputType = inputType.GetPlainType();

        if (_ambientPatternDeclarationBindingKeyword is SyntaxKind.LetKeyword or SyntaxKind.ValKeyword or SyntaxKind.VarKeyword &&
            syntax.Type is IdentifierNameSyntax identifier &&
            (syntax.Designation is null ||
             syntax.Designation is SingleVariableDesignationSyntax { Identifier.IsMissing: true } ||
             syntax.Designation is SingleVariableDesignationSyntax { Identifier.Kind: SyntaxKind.None }))
        {
            return BindIdentifierBindingPattern(identifier, inputType, _ambientPatternDeclarationBindingKeyword);
        }

        var typeExpression = BindTypeSyntaxAsExpression(syntax.Type);
        var declaredType = TryInferDeclarationPatternTypeFromIdentifierSyntax(syntax.Type, inputType, out var inferredType)
            ? inferredType
            : InferDeclarationPatternTypeFromInput(typeExpression.Type, inputType);
        declaredType = EnsureTypeAccessible(declaredType, syntax.Type.GetLocation());

        BoundDesignator designator;
        if (syntax.Designation is SingleVariableDesignationSyntax single &&
            !single.Identifier.IsMissing &&
            single.Identifier.Kind != SyntaxKind.None &&
            !string.IsNullOrEmpty(single.Identifier.ValueText) &&
            single.Identifier.ValueText != "_")
        {
            var isMutable = !single.BindingKeyword.IsMissing && single.BindingKeyword.IsKind(SyntaxKind.VarKeyword);
            var localType = EnsureTypeAccessible(declaredType, single.Identifier.GetLocation());
            var local = DeclarePatternLocal(single, single.Identifier.ValueText, isMutable, localType);
            var singleDesignator = new BoundSingleVariableDesignator(local);
            CacheBoundNode(single, singleDesignator);
            designator = singleDesignator;
        }
        else
        {
            designator = new BoundDiscardDesignator(declaredType);
        }

        if (typeExpression is BoundTypeExpression { TypeSymbol: NullTypeSymbol } &&
            designator is BoundDiscardDesignator)
        {
            var nullLiteral = new BoundLiteralExpression(BoundLiteralExpressionKind.NullLiteral, null!, Compilation.NullTypeSymbol);
            return new BoundConstantPattern(nullLiteral);
        }

        var declarationPattern = new BoundDeclarationPattern(declaredType, designator);
        return TryWrapUnionMemberPattern(syntax.Type, inputType, declaredType, declarationPattern, out var unionMemberPattern)
            ? unionMemberPattern
            : declarationPattern;
    }

    private bool TryInferDeclarationPatternTypeFromIdentifierSyntax(
        TypeSyntax syntax,
        ITypeSymbol inputType,
        out ITypeSymbol inferredType)
    {
        inferredType = Compilation.ErrorTypeSymbol;

        if (syntax is not IdentifierNameSyntax identifier ||
            inputType is not INamedTypeSymbol inputNamed)
        {
            return false;
        }

        var candidateSymbol = LookupType(identifier.Identifier.ValueText);
        if (candidateSymbol is not INamedTypeSymbol candidateNamed)
            return false;

        var candidateDefinition = NormalizeDefinition(candidateNamed);
        var inputDefinition = (inputNamed.OriginalDefinition as INamedTypeSymbol) ?? inputNamed;

        if (!SymbolEqualityComparer.Default.Equals(candidateDefinition, inputDefinition))
            return false;

        if (candidateDefinition.Arity == 0 ||
            inputNamed.TypeArguments.IsDefaultOrEmpty ||
            inputNamed.TypeArguments.Length != candidateDefinition.Arity)
        {
            return false;
        }

        inferredType = inputNamed;
        return true;
    }

    private ITypeSymbol InferDeclarationPatternTypeFromInput(ITypeSymbol declaredType, ITypeSymbol inputType)
    {
        if (declaredType is not INamedTypeSymbol declaredNamed ||
            inputType is not INamedTypeSymbol inputNamed)
        {
            return declaredType;
        }

        var declaredDefinition = (declaredNamed.OriginalDefinition as INamedTypeSymbol) ?? declaredNamed;
        var inputDefinition = (inputNamed.OriginalDefinition as INamedTypeSymbol) ?? inputNamed;

        if (!CanInferDeclarationPatternTypeArguments(declaredNamed, declaredDefinition))
            return declaredType;

        if (!SymbolEqualityComparer.Default.Equals(declaredDefinition, inputDefinition))
            return declaredType;

        if (inputNamed.TypeArguments.IsDefaultOrEmpty ||
            inputNamed.TypeArguments.Length != declaredDefinition.Arity)
            return declaredType;

        return inputNamed;
    }

    private static bool CanInferDeclarationPatternTypeArguments(INamedTypeSymbol type, INamedTypeSymbol definition)
    {
        if (definition.Arity == 0)
            return false;

        if (type.TypeArguments.IsDefaultOrEmpty)
            return true;

        if (type.TypeArguments.Length != definition.Arity)
            return false;

        return type.TypeArguments.All(static argument => argument is ITypeParameterSymbol);
    }

    private BoundPattern BindPositionalPattern(PositionalPatternSyntax syntax, ITypeSymbol? inputType)
    {
        if (inputType is not null)
        {
            var deconstructMethod = FindDeconstructMethod(inputType, syntax.Elements.Count);
            if (deconstructMethod is not null)
                return BindDeconstructPattern(
                    syntax.Elements,
                    deconstructMethod,
                    inputType,
                    narrowedType: null,
                    designation: syntax.Designation);
        }

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

            var boundElement = BindPositionalPatternElement(elementSyntax.Pattern, expectedElementType);
            boundElement = BindPositionalPatternElementDesignation(elementSyntax, boundElement);
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
        var designator = BindWholePatternDesignation(syntax.Designation, inputType ?? tupleType);

        return new BoundPositionalPattern(tupleType, elementPatterns.ToImmutable(), designator);
    }

    private BoundPattern BindSequencePattern(SequencePatternSyntax syntax, ITypeSymbol? inputType)
    {
        inputType ??= Compilation.GetSpecialType(SpecialType.System_Object);

        var elementPatterns = ImmutableArray.CreateBuilder<BoundPattern>(syntax.Elements.Count);
        var patternType = inputType;
        var reason = BoundExpressionReason.None;
        var (elementWidths, elementKinds, restIndex) = GetSequencePatternLayout(syntax.Elements);
        var elementType = Compilation.ErrorTypeSymbol;
        if (!TryGetSequencePatternElementType(inputType, out elementType))
        {
            if (inputType.TypeKind != TypeKind.Error)
            {
                _diagnostics.ReportMatchExpressionArmPatternInvalid(
                    "for a sequence pattern",
                    inputType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    syntax.GetLocation());
            }

            elementType = Compilation.ErrorTypeSymbol;
            reason = BoundExpressionReason.TypeMismatch;
        }

        for (var i = 0; i < syntax.Elements.Count; i++)
        {
            var elementSyntax = syntax.Elements[i];
            var expectedType = GetSequencePatternElementType(
                inputType,
                elementType,
                elementWidths,
                elementKinds,
                restIndex,
                i);
            var boundElement = BindPositionalPatternElement(elementSyntax.Pattern, expectedType);
            elementPatterns.Add(boundElement);
        }

        var designator = BindWholePatternDesignation(syntax.Designation, patternType);

        return new BoundPositionalPattern(
            patternType,
            elementPatterns.ToImmutable(),
            designator,
            reason,
            restIndex,
            elementWidths,
            elementKinds);
    }

    private BoundPattern BindDictionaryPattern(DictionaryPatternSyntax syntax, ITypeSymbol? inputType)
    {
        inputType ??= Compilation.GetSpecialType(SpecialType.System_Object);

        var reason = BoundExpressionReason.None;
        var receiverType = Compilation.ErrorTypeSymbol;
        var keyType = Compilation.ErrorTypeSymbol;
        var valueType = Compilation.ErrorTypeSymbol;

        if (inputType is INamedTypeSymbol namedInput &&
            TryGetDictionaryInterfaceInfo(namedInput, out var dictionaryReceiverType, out var dictionaryKeyType, out var dictionaryValueType))
        {
            receiverType = dictionaryReceiverType;
            keyType = dictionaryKeyType;
            valueType = dictionaryValueType;
        }
        else
        {
            if (inputType.TypeKind != TypeKind.Error)
            {
                _diagnostics.Report(Diagnostic.Create(
                    CompilerDiagnostics.DictionaryPatternRequiresDictionaryType,
                    syntax.GetLocation(),
                    inputType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat)));
            }

            reason = BoundExpressionReason.TypeMismatch;
        }

        var entries = ImmutableArray.CreateBuilder<BoundDictionarySubpattern>(syntax.Entries.Count);

        foreach (var entrySyntax in syntax.Entries)
        {
            var key = keyType.TypeKind == TypeKind.Error
                ? BindExpression(entrySyntax.Key)
                : BindExpressionWithTargetType(entrySyntax.Key, keyType);

            if (keyType.TypeKind != TypeKind.Error &&
                key.Type?.TypeKind != TypeKind.Error &&
                ShouldAttemptConversion(key))
            {
                if (IsAssignable(keyType, key.Type!, out var keyConversion))
                    key = ApplyConversion(key, keyType, keyConversion, entrySyntax.Key);
                else
                    ReportCannotConvertFromTypeToType(
                        key.Type!.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        keyType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        entrySyntax.Key.GetLocation());
            }

            var boundPattern = BindPattern(entrySyntax.Pattern, valueType);
            entries.Add(new BoundDictionarySubpattern(key, boundPattern));
        }

        ReportDuplicateDictionaryKeys(
            entries.Select((entry, index) => (entry.Key, (SyntaxNode)syntax.Entries[index].Key)));

        var designator = BindWholePatternDesignation(syntax.Designation, inputType);
        return new BoundDictionaryPattern(
            inputType,
            receiverType,
            keyType,
            valueType,
            designator,
            entries.ToImmutable(),
            reason);
    }

    private bool TryGetSequencePatternElementType(ITypeSymbol inputType, out ITypeSymbol elementType)
    {
        elementType = Compilation.ErrorTypeSymbol;

        if (inputType.TypeKind == TypeKind.Error)
            return false;

        if (inputType is IArrayTypeSymbol arrayType)
        {
            elementType = arrayType.ElementType;
            return true;
        }

        if (inputType.SpecialType == SpecialType.System_String)
        {
            elementType = Compilation.GetSpecialType(SpecialType.System_Char);
            return true;
        }

        if (inputType is INamedTypeSymbol namedType)
        {
            foreach (var candidate in EnumerateSelfAndInterfaces(namedType))
            {
                if (!TryGetIndexableElementType(candidate, out var indexerElementType))
                    continue;

                elementType = indexerElementType;
                return true;
            }
        }

        return false;

        static IEnumerable<INamedTypeSymbol> EnumerateSelfAndInterfaces(INamedTypeSymbol type)
        {
            yield return type;
            foreach (var iface in type.AllInterfaces)
                yield return iface;
        }

        static bool TryGetIndexableElementType(INamedTypeSymbol type, out ITypeSymbol indexerElementType)
        {
            indexerElementType = default!;

            var hasCount = type
                .GetMembers("Count")
                .OfType<IPropertySymbol>()
                .Any(static property =>
                    property.Parameters.Length == 0 &&
                    property.Type.SpecialType == SpecialType.System_Int32 &&
                    property.GetMethod is not null);

            if (!hasCount)
                return false;

            var indexer = type
                .GetMembers("Item")
                .OfType<IPropertySymbol>()
                .FirstOrDefault(static property =>
                    property.Parameters.Length == 1 &&
                    property.Parameters[0].Type.SpecialType == SpecialType.System_Int32 &&
                    property.GetMethod is not null);

            if (indexer is null)
                return false;

            indexerElementType = indexer.Type;
            return true;
        }
    }

    private BoundPattern BindPositionalPatternElement(PatternSyntax syntax, ITypeSymbol? expectedType)
    {
        expectedType ??= Compilation.GetSpecialType(SpecialType.System_Object);

        return BindPattern(syntax, expectedType);
    }

    private BoundPattern BindDeconstructPattern(
        SeparatedSyntaxList<PositionalPatternElementSyntax> elements,
        IMethodSymbol deconstructMethod,
        ITypeSymbol inputType,
        ITypeSymbol? narrowedType,
        VariableDesignationSyntax? designation)
    {
        var fallbackLocation = elements.Count > 0 ? elements[0].GetLocation() : Location.None;
        var parameterOffset = GetDeconstructParameterOffset(deconstructMethod);
        var parameters = deconstructMethod.Parameters;
        var parameterCount = parameters.Length - parameterOffset;
        var boundElements = ImmutableArray.CreateBuilder<BoundPattern>(parameterCount);
        var elementCount = Math.Min(elements.Count, parameterCount);

        for (var i = 0; i < elementCount; i++)
        {
            var elementSyntax = elements[i];
            var expectedType = EnsureTypeAccessible(parameters[i + parameterOffset].Type, elementSyntax.GetLocation());
            var boundElement = BindPositionalPatternElement(elementSyntax.Pattern, expectedType);
            boundElement = BindPositionalPatternElementDesignation(elementSyntax, boundElement);
            boundElements.Add(boundElement);
        }

        for (var i = elementCount; i < parameterCount; i++)
        {
            var parameterType = EnsureTypeAccessible(parameters[i + parameterOffset].Type, fallbackLocation);
            boundElements.Add(new BoundDiscardPattern(parameterType, BoundExpressionReason.TypeMismatch));
        }

        for (var i = elementCount; i < elements.Count; i++)
            _ = BindPositionalPatternElement(elements[i].Pattern, null);

        var designator = BindWholePatternDesignation(designation, narrowedType ?? inputType);

        return new BoundDeconstructPattern(
            inputType: inputType,
            receiverType: GetDeconstructReceiverType(deconstructMethod),
            narrowedType: narrowedType,
            deconstructMethod: deconstructMethod,
            arguments: boundElements.ToImmutable(),
            designator: designator);
    }

    private BoundPattern BindDeconstructPattern(
        SeparatedSyntaxList<PatternSyntax> arguments,
        IMethodSymbol deconstructMethod,
        ITypeSymbol inputType,
        ITypeSymbol? narrowedType,
        VariableDesignationSyntax? designation = null)
    {
        var parameterOffset = GetDeconstructParameterOffset(deconstructMethod);
        var parameters = deconstructMethod.Parameters;
        var parameterCount = parameters.Length - parameterOffset;
        var boundElements = ImmutableArray.CreateBuilder<BoundPattern>(parameterCount);
        var elementCount = Math.Min(arguments.Count, parameterCount);

        for (var i = 0; i < elementCount; i++)
        {
            var argumentSyntax = arguments[i];
            var expectedType = EnsureTypeAccessible(parameters[i + parameterOffset].Type, argumentSyntax.GetLocation());
            boundElements.Add(BindPattern(argumentSyntax, expectedType));
        }

        for (var i = elementCount; i < parameterCount; i++)
        {
            var parameterType = EnsureTypeAccessible(parameters[i + parameterOffset].Type, Location.None);
            boundElements.Add(new BoundDiscardPattern(parameterType, BoundExpressionReason.TypeMismatch));
        }

        for (var i = elementCount; i < arguments.Count; i++)
            _ = BindPattern(arguments[i]);

        var designator = BindWholePatternDesignation(designation, narrowedType ?? inputType);

        return new BoundDeconstructPattern(
            inputType: inputType,
            receiverType: GetDeconstructReceiverType(deconstructMethod),
            narrowedType: narrowedType,
            deconstructMethod: deconstructMethod,
            arguments: boundElements.ToImmutable(),
            designator: designator);
    }

    private void ReportTypedPatternBindingsMissingKeyword(VariableDesignationSyntax designation, bool hasAmbientBindingKeyword)
    {
        // Report on each single variable under a typed designation when the single variable
        // is a real binding and its BindingKeyword is missing/None.
        switch (designation)
        {
            case TypedVariableDesignationSyntax typed:
                {
                    void WalkInner(VariableDesignationSyntax inner)
                    {
                        switch (inner)
                        {
                            case SingleVariableDesignationSyntax single when
                                !single.Identifier.IsMissing &&
                                single.Identifier.ValueText != "_" &&
                                !string.IsNullOrEmpty(single.Identifier.ValueText):
                                {
                                    // Typed designations require an explicit binding keyword on each single variable.
                                    // Example that should diagnose: (a: bool, b: string)
                                    // Example that should NOT diagnose: (val a: bool, var b: string)
                                    if (!HasExplicitBindingKeyword(single, hasAmbientBindingKeyword))
                                    {
                                        _diagnostics.ReportPatternTypedBindingRequiresKeyword(
                                            single.Identifier.ValueText,
                                            typed.TypeAnnotation.Type.ToString(),
                                            single.Identifier.GetLocation());
                                    }
                                    break;
                                }
                            case ParenthesizedVariableDesignationSyntax p:
                                foreach (var v in p.Variables)
                                    WalkInner(v);
                                break;
                            case TypedVariableDesignationSyntax t:
                                // Nested typed designations: report using the nested type annotation.
                                ReportTypedPatternBindingsMissingKeyword(t, hasAmbientBindingKeyword);
                                break;
                        }
                    }

                    WalkInner(typed.Designation);
                    break;
                }

            case ParenthesizedVariableDesignationSyntax parenthesized:
                foreach (var v in parenthesized.Variables)
                    ReportTypedPatternBindingsMissingKeyword(v, hasAmbientBindingKeyword);
                break;
        }
    }

    private static bool HasExplicitBindingKeyword(SingleVariableDesignationSyntax single, bool hasAmbientBindingKeyword)
    {
        // Explicit on the single variable itself?
        if (!single.BindingKeyword.IsMissing && single.BindingKeyword.Kind != SyntaxKind.None)
            return true;

        if (hasAmbientBindingKeyword)
            return true;

        // Or provided by an enclosing variable pattern (`val`/`var`/`let`)?
        for (SyntaxNode? current = single.Parent; current is not null; current = current.Parent)
        {
            if (current is VariablePatternSyntax vp)
                return !vp.BindingKeyword.IsMissing && vp.BindingKeyword.Kind != SyntaxKind.None;

            // Stop once we leave the pattern subtree.
            if (current is PatternSyntax)
                break;
        }

        return false;
    }

    private BoundPattern BindVariablePattern(VariablePatternSyntax syntax, ITypeSymbol? expectedType)
    {
        // No longer enforce here; enforcement is now handled at the single variable level.
        var isMutable = syntax.BindingKeyword.IsKind(SyntaxKind.VarKeyword) ||
                        (syntax.BindingKeyword.Kind == SyntaxKind.None && _ambientPatternDeclarationBindingKeyword == SyntaxKind.VarKeyword);
        return BindVariableDesignation(syntax.Designation, isMutable, expectedType);
    }

    private BoundPattern BindIdentifierBindingPattern(
        IdentifierNameSyntax identifier,
        ITypeSymbol inputType,
        SyntaxKind declarationBindingKeywordKind)
    {
        var name = identifier.Identifier.ValueText;

        if (string.IsNullOrEmpty(name))
            return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.TypeMismatch);

        if (name == "_")
            return new BoundDiscardPattern(inputType.TypeKind == TypeKind.Error ? Compilation.ErrorTypeSymbol : inputType);

        var declaredType = EnsureTypeAccessible(inputType, identifier.GetLocation());
        var isMutable = declarationBindingKeywordKind == SyntaxKind.VarKeyword;
        var local = DeclarePatternLocal(identifier, name, isMutable, declaredType);
        var designator = new BoundSingleVariableDesignator(local);
        return new BoundDeclarationPattern(declaredType, designator);
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
        // Enforce: typed bindings in patterns require explicit let/val/var on each single variable.
        ReportTypedPatternBindingsMissingKeyword(
            typedDesignation,
            hasAmbientBindingKeyword: _ambientPatternDeclarationBindingKeyword is SyntaxKind.LetKeyword or SyntaxKind.ValKeyword or SyntaxKind.VarKeyword);

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

        // If a binding keyword is present on this single variable, it overrides the ambient mutability.
        if (!single.BindingKeyword.IsMissing && single.BindingKeyword.Kind != SyntaxKind.None)
            isMutable = single.BindingKeyword.IsKind(SyntaxKind.VarKeyword);

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

        var local = DeclarePatternLocal(single, single.Identifier.ValueText, isMutable, type);
        var designator = new BoundSingleVariableDesignator(local);
        CacheBoundNode(single, designator);

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

        return new BoundPositionalPattern(tupleType, boundElements.ToImmutable());
    }

    private BoundPattern BindPositionalPatternElementDesignation(PositionalPatternElementSyntax elementSyntax, BoundPattern pattern)
    {
        if (elementSyntax.NameColon is null)
            return pattern;

        var identifier = elementSyntax.NameColon.Name.Identifier;
        if (identifier.IsMissing)
            return pattern;

        if (pattern is not BoundDeclarationPattern declaration || declaration.Designator is not BoundDiscardDesignator)
            return pattern;

        _diagnostics.ReportPatternTypedBindingRequiresKeyword(
            identifier.ValueText,
            declaration.DeclaredType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
            identifier.GetLocation());
        var local = DeclarePatternLocal(elementSyntax.NameColon.Name, identifier.ValueText, isMutable: false, declaration.DeclaredType);
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

    private BoundPattern BindCasePattern(MemberPatternSyntax syntax, ITypeSymbol? inputType)
    {
        var qualifierType = syntax.Path.Qualifier is null
            ? null
            : ResolveType(syntax.Path.Qualifier);

        if (TryBindDiscriminatedUnionCasePattern(
                caseName: syntax.Path.Identifier.ValueText,
                qualifierType: qualifierType,
                inputType: inputType,
                arguments: syntax.ArgumentList is null ? SeparatedSyntaxList<PatternSyntax>.Empty : syntax.ArgumentList.Arguments,
                designation: syntax.Designation,
                caseNameLocation: syntax.Path.Identifier.GetLocation(),
                argumentListLocation: syntax.ArgumentList?.GetLocation() ?? syntax.Path.GetLocation(),
                out var pattern))
        {
            return pattern;
        }

        return BindCasePatternAsConstant(syntax, qualifierType, inputType);
    }

    private static INamedTypeSymbol ProjectCaseSymbolToUnionArguments(INamedTypeSymbol caseType, INamedTypeSymbol unionType)
    {
        if (!caseType.IsGenericType || caseType.TypeParameters.IsDefaultOrEmpty)
            return caseType;

        var unionTypeParameters = unionType.TypeParameters;
        var unionTypeArguments = unionType.TypeArguments;
        if (unionTypeParameters.IsDefaultOrEmpty || unionTypeArguments.IsDefaultOrEmpty)
            return caseType;

        var nameToArgument = new Dictionary<string, ITypeSymbol>(StringComparer.Ordinal);
        var count = Math.Min(unionTypeParameters.Length, unionTypeArguments.Length);
        for (var i = 0; i < count; i++)
            nameToArgument[unionTypeParameters[i].Name] = unionTypeArguments[i];

        var projectedArguments = new ITypeSymbol[caseType.TypeParameters.Length];
        var changed = false;
        for (var i = 0; i < caseType.TypeParameters.Length; i++)
        {
            var parameter = caseType.TypeParameters[i];
            if (nameToArgument.TryGetValue(parameter.Name, out var mapped))
            {
                projectedArguments[i] = mapped;
                if (!SymbolEqualityComparer.Default.Equals(mapped, parameter))
                    changed = true;
            }
            else
            {
                projectedArguments[i] = parameter;
            }
        }

        return changed ? (INamedTypeSymbol)caseType.Construct(projectedArguments) : caseType;
    }

    private BoundPattern BindCasePatternAsConstant(
        MemberPatternSyntax syntax,
        ITypeSymbol? qualifierType,
        ITypeSymbol? inputType)
    {
        if (syntax.ArgumentList is not null)
        {
            _diagnostics.ReportCasePatternRequiresDiscriminatedUnion(
                syntax.Path.Identifier.ValueText,
                syntax.GetLocation());
            return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.TypeMismatch);
        }

        var targetType = qualifierType ?? inputType;
        if (targetType is null)
        {
            _diagnostics.ReportMemberAccessRequiresTargetType(
                syntax.Path.Identifier.ValueText,
                syntax.Path.Identifier.GetLocation());
            return new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.MissingType);
        }

        var nameSyntax = SyntaxFactory.IdentifierName(syntax.Path.Identifier);
        var expression = BindTargetTypedMemberAccess(nameSyntax, targetType);
        return BindConstantPatternFromExpression(
            expression,
            nameSyntax,
            inputType ?? targetType,
            BindWholePatternDesignation(syntax.Designation, inputType ?? targetType));
    }

    private BoundPattern BindNominalDeconstructionPattern(NominalDeconstructionPatternSyntax syntax, ITypeSymbol? inputType)
    {
        if (TryBindUnionMemberNominalDeconstructionPattern(syntax, inputType, out var unionMemberPattern))
            return unionMemberPattern;

        if (TryBindNominalDeconstructionPatternAsCasePattern(syntax, inputType, out var casePattern))
            return casePattern;

        inputType ??= Compilation.GetSpecialType(SpecialType.System_Object);

        var boundType = BindTypeSyntaxAsExpression(syntax.Type);
        var recordType = EnsureTypeAccessible(boundType.Type, syntax.Type.GetLocation());

        if (recordType.TypeKind == TypeKind.Error)
        {
            var props = BindNominalDeconstructionPatternSubpatternsAsDiscards(syntax);
            return new BoundPropertyPattern(
                inputType: inputType,
                receiverType: Compilation.ErrorTypeSymbol,
                narrowedType: recordType,
                designator: BindWholePatternDesignation(syntax.Designation, inputType),
                properties: props,
                reason: BoundExpressionReason.TypeMismatch);
        }

        if (recordType is not INamedTypeSymbol)
        {
            _diagnostics.ReportNominalDeconstructionPatternRequiresDeconstructableType(
                recordType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                syntax.Type.GetLocation());

            var props = BindNominalDeconstructionPatternSubpatternsAsDiscards(syntax);
            return new BoundPropertyPattern(
                inputType: inputType,
                receiverType: Compilation.ErrorTypeSymbol,
                narrowedType: recordType,
                designator: BindWholePatternDesignation(syntax.Designation, inputType),
                properties: props,
                reason: BoundExpressionReason.TypeMismatch);
        }

        if (recordType.TypeKind != TypeKind.Error &&
            inputType.TypeKind != TypeKind.Error &&
            !CanPatternMatchInputType(inputType, recordType))
        {
            _diagnostics.ReportNominalDeconstructionPatternTypeMismatch(
                inputType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                recordType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                syntax.Type.GetLocation());
        }

        var deconstructArity = GetDeconstructArity(recordType);
        if (deconstructArity is null)
        {
            _diagnostics.ReportNominalDeconstructionPatternRequiresDeconstructableType(
                recordType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                syntax.Type.GetLocation());

            var props = BindNominalDeconstructionPatternSubpatternsAsDiscards(syntax);
            return new BoundPropertyPattern(
                inputType: inputType,
                receiverType: Compilation.ErrorTypeSymbol,
                narrowedType: recordType,
                designator: BindWholePatternDesignation(syntax.Designation, recordType),
                properties: props,
                reason: BoundExpressionReason.TypeMismatch);
        }

        var argumentList = syntax.ArgumentList;
        var argumentCount = argumentList.Arguments.Count;

        if (argumentCount != deconstructArity.Value)
        {
            _diagnostics.ReportNominalDeconstructionPatternArgumentCountMismatch(
                recordType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                deconstructArity.Value,
                argumentCount,
                argumentList.GetLocation());
        }

        var deconstructMethod = FindDeconstructMethod(recordType, argumentCount);
        if (deconstructMethod is null)
        {
            var props = BindNominalDeconstructionPatternSubpatternsAsDiscards(syntax);
            return new BoundPropertyPattern(
                inputType: inputType,
                receiverType: recordType,
                narrowedType: recordType,
                designator: BindWholePatternDesignation(syntax.Designation, recordType),
                properties: props,
                reason: BoundExpressionReason.NotFound);
        }

        return BindDeconstructPattern(
            syntax.ArgumentList.Arguments,
            deconstructMethod,
            inputType,
            narrowedType: recordType,
            designation: syntax.Designation);
    }

    private bool TryBindUnionMemberNominalDeconstructionPattern(
        NominalDeconstructionPatternSyntax syntax,
        ITypeSymbol? inputType,
        out BoundPattern? pattern)
    {
        pattern = null;

        if (!TryResolveUnionMemberPatternTarget(syntax.Type, inputType, out var memberType, out var tryGetMethod))
            return false;

        inputType ??= Compilation.GetSpecialType(SpecialType.System_Object);

        var innerPattern = BindNominalDeconstructionPatternAgainstKnownType(syntax, inputType, memberType);
        pattern = new BoundUnionMemberPattern(inputType, memberType, tryGetMethod, innerPattern);
        return true;
    }

    private BoundPattern BindNominalDeconstructionPatternAgainstKnownType(
        NominalDeconstructionPatternSyntax syntax,
        ITypeSymbol inputType,
        ITypeSymbol memberType)
    {
        var recordType = EnsureTypeAccessible(memberType, syntax.Type.GetLocation());

        if (recordType.TypeKind == TypeKind.Error)
        {
            var props = BindNominalDeconstructionPatternSubpatternsAsDiscards(syntax);
            return new BoundPropertyPattern(
                inputType: inputType,
                receiverType: Compilation.ErrorTypeSymbol,
                narrowedType: recordType,
                designator: BindWholePatternDesignation(syntax.Designation, inputType),
                properties: props,
                reason: BoundExpressionReason.TypeMismatch);
        }

        if (recordType is not INamedTypeSymbol)
        {
            _diagnostics.ReportNominalDeconstructionPatternRequiresDeconstructableType(
                recordType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                syntax.Type.GetLocation());

            var props = BindNominalDeconstructionPatternSubpatternsAsDiscards(syntax);
            return new BoundPropertyPattern(
                inputType: inputType,
                receiverType: Compilation.ErrorTypeSymbol,
                narrowedType: recordType,
                designator: BindWholePatternDesignation(syntax.Designation, inputType),
                properties: props,
                reason: BoundExpressionReason.TypeMismatch);
        }

        if (recordType.TypeKind != TypeKind.Error &&
            inputType.TypeKind != TypeKind.Error &&
            !CanPatternMatchInputType(inputType, recordType))
        {
            _diagnostics.ReportNominalDeconstructionPatternTypeMismatch(
                inputType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                recordType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                syntax.Type.GetLocation());
        }

        var deconstructArity = GetDeconstructArity(recordType);
        if (deconstructArity is null)
        {
            _diagnostics.ReportNominalDeconstructionPatternRequiresDeconstructableType(
                recordType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                syntax.Type.GetLocation());

            var props = BindNominalDeconstructionPatternSubpatternsAsDiscards(syntax);
            return new BoundPropertyPattern(
                inputType: inputType,
                receiverType: Compilation.ErrorTypeSymbol,
                narrowedType: recordType,
                designator: BindWholePatternDesignation(syntax.Designation, recordType),
                properties: props,
                reason: BoundExpressionReason.TypeMismatch);
        }

        var argumentList = syntax.ArgumentList;
        var argumentCount = argumentList.Arguments.Count;

        if (argumentCount != deconstructArity.Value)
        {
            _diagnostics.ReportNominalDeconstructionPatternArgumentCountMismatch(
                recordType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                deconstructArity.Value,
                argumentCount,
                argumentList.GetLocation());
        }

        var deconstructMethod = FindDeconstructMethod(recordType, argumentCount);
        if (deconstructMethod is null)
        {
            var props = BindNominalDeconstructionPatternSubpatternsAsDiscards(syntax);
            return new BoundPropertyPattern(
                inputType: inputType,
                receiverType: recordType,
                narrowedType: recordType,
                designator: BindWholePatternDesignation(syntax.Designation, recordType),
                properties: props,
                reason: BoundExpressionReason.NotFound);
        }

        return BindDeconstructPattern(
            syntax.ArgumentList.Arguments,
            deconstructMethod,
            inputType,
            narrowedType: recordType,
            designation: syntax.Designation);
    }

    private bool TryBindNominalDeconstructionPatternAsCasePattern(
        NominalDeconstructionPatternSyntax syntax,
        ITypeSymbol? inputType,
        out BoundPattern? pattern)
    {
        pattern = null;

        if (!TryGetCasePatternHead(syntax.Type, out var caseName, out var qualifierType, out var caseNameLocation))
            return false;

        return TryBindDiscriminatedUnionCasePattern(
            caseName: caseName!,
            qualifierType: qualifierType,
            inputType: inputType,
            arguments: syntax.ArgumentList.Arguments,
            designation: syntax.Designation,
            caseNameLocation: caseNameLocation,
            argumentListLocation: syntax.ArgumentList.GetLocation(),
            out pattern);
    }

    private bool TryBindDiscriminatedUnionCasePattern(
        string caseName,
        ITypeSymbol? qualifierType,
        ITypeSymbol? inputType,
        SeparatedSyntaxList<PatternSyntax> arguments,
        VariableDesignationSyntax? designation,
        Location caseNameLocation,
        Location argumentListLocation,
        out BoundPattern? pattern)
    {
        pattern = null;

        var lookupType = qualifierType ?? inputType;
        if (lookupType is null)
            return false;

        var unionType = lookupType.TryGetUnion()
            ?? lookupType.TryGetUnionCase()?.Union;

        if (unionType is null)
            return false;

        var caseSymbol = unionType.CaseTypes.FirstOrDefault(c => c.Name == caseName);
        if (caseSymbol is null)
        {
            _diagnostics.ReportCasePatternCaseNotFound(
                caseName,
                unionType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                caseNameLocation);

            pattern = new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.NotFound);
            return true;
        }

        if (caseSymbol is INamedTypeSymbol caseNamed &&
            unionType is INamedTypeSymbol unionNamed)
        {
            caseSymbol = ProjectCaseSymbolToUnionArguments(caseNamed, unionNamed) as IUnionCaseTypeSymbol
                ?? caseSymbol;
        }

        var tryGetMethod = FindTryGetMethod(lookupType, unionType, caseSymbol);
        if (tryGetMethod is null)
        {
            pattern = new BoundDiscardPattern(Compilation.ErrorTypeSymbol, BoundExpressionReason.NotFound);
            return true;
        }

        var parameters = caseSymbol.ConstructorParameters;
        var argumentCount = arguments.Count;
        var implicitUnitArgument = argumentCount == 0 && parameters.Length == 1;

        if (!implicitUnitArgument && argumentCount != parameters.Length)
        {
            _diagnostics.ReportCasePatternArgumentCountMismatch(
                caseSymbol.Name,
                parameters.Length,
                argumentCount,
                argumentListLocation);
        }

        var boundArguments = ImmutableArray.CreateBuilder<BoundPattern>(Math.Max(parameters.Length, argumentCount));
        var elementCount = Math.Min(parameters.Length, argumentCount);
        var matchedArguments = elementCount;

        if (implicitUnitArgument)
        {
            var unitType = Compilation.GetSpecialType(SpecialType.System_Unit);
            boundArguments.Add(new BoundConstantPattern(new BoundUnitExpression(unitType)));
            elementCount = 0;
            matchedArguments = 1;
        }

        for (var i = 0; i < elementCount; i++)
        {
            var boundArgument = BindCasePatternArgument(arguments[i], parameters[i].Type);
            boundArguments.Add(boundArgument);
        }

        for (var i = matchedArguments; i < parameters.Length; i++)
            boundArguments.Add(new BoundDiscardPattern(parameters[i].Type, BoundExpressionReason.TypeMismatch));

        for (var i = elementCount; i < argumentCount; i++)
            boundArguments.Add(BindPattern(arguments[i]));

        pattern = new BoundCasePattern(
            caseSymbol,
            tryGetMethod,
            boundArguments.ToImmutable(),
            BindWholePatternDesignation(designation, caseSymbol));
        return true;
    }

    private bool TryGetCasePatternHead(
        TypeSyntax typeSyntax,
        out string? caseName,
        out ITypeSymbol? qualifierType,
        out Location caseNameLocation)
    {
        caseName = null;
        qualifierType = null;
        caseNameLocation = typeSyntax.GetLocation();

        switch (typeSyntax)
        {
            case IdentifierNameSyntax identifier:
                caseName = identifier.Identifier.ValueText;
                caseNameLocation = identifier.Identifier.GetLocation();
                return !string.IsNullOrEmpty(caseName);

            case GenericNameSyntax generic:
                caseName = generic.Identifier.ValueText;
                caseNameLocation = generic.Identifier.GetLocation();
                return !string.IsNullOrEmpty(caseName);

            case QualifiedNameSyntax qualified:
                qualifierType = ResolveType(qualified.Left);
                switch (qualified.Right)
                {
                    case IdentifierNameSyntax rightIdentifier:
                        caseName = rightIdentifier.Identifier.ValueText;
                        caseNameLocation = rightIdentifier.Identifier.GetLocation();
                        return !string.IsNullOrEmpty(caseName);

                    case GenericNameSyntax rightGeneric:
                        caseName = rightGeneric.Identifier.ValueText;
                        caseNameLocation = rightGeneric.Identifier.GetLocation();
                        return !string.IsNullOrEmpty(caseName);

                    default:
                        return false;
                }

            default:
                return false;
        }
    }

    private IMethodSymbol? FindDeconstructMethod(ITypeSymbol inputType, int parameterCount)
    {
        foreach (var method in inputType.GetMembers("Deconstruct").OfType<IMethodSymbol>())
        {
            if (IsInstanceDeconstructCandidate(method, parameterCount))
                return method;
        }

        foreach (var method in LookupExtensionMethods("Deconstruct", inputType))
        {
            if (IsExtensionDeconstructCandidate(method, inputType, parameterCount))
                return method;
        }

        return null;
    }

    private int? GetDeconstructArity(ITypeSymbol inputType)
    {
        foreach (var method in inputType.GetMembers("Deconstruct").OfType<IMethodSymbol>())
        {
            if (method.MethodKind == MethodKind.Ordinary &&
                !method.IsStatic &&
                method.ReturnType.SpecialType == SpecialType.System_Unit &&
                method.Parameters.All(static parameter => parameter.RefKind == RefKind.Out))
            {
                return method.Parameters.Length;
            }
        }

        foreach (var method in LookupExtensionMethods("Deconstruct", inputType))
        {
            if (method.MethodKind == MethodKind.Ordinary &&
                method.IsStatic &&
                method.Parameters.Length > 0 &&
                method.ReturnType.SpecialType == SpecialType.System_Unit &&
                SymbolEqualityComparer.Default.Equals(method.Parameters[0].Type, inputType) &&
                method.Parameters.Skip(1).All(static parameter => parameter.RefKind == RefKind.Out))
            {
                return method.Parameters.Length - 1;
            }
        }

        return null;
    }

    private static bool IsInstanceDeconstructCandidate(IMethodSymbol method, int parameterCount)
    {
        return method.MethodKind == MethodKind.Ordinary &&
               !method.IsStatic &&
               method.Parameters.Length == parameterCount &&
               method.ReturnType.SpecialType == SpecialType.System_Unit &&
               method.Parameters.All(parameter => parameter.RefKind == RefKind.Out);
    }

    private bool IsExtensionDeconstructCandidate(IMethodSymbol method, ITypeSymbol inputType, int parameterCount)
    {
        if (!method.IsExtensionMethod || method.MethodKind != MethodKind.Ordinary || !method.IsStatic)
            return false;

        if (method.Parameters.Length != parameterCount + 1)
            return false;

        if (method.Parameters[0].RefKind != RefKind.None)
            return false;

        if (method.ReturnType.SpecialType != SpecialType.System_Unit)
            return false;

        if (method.Parameters.Skip(1).Any(parameter => parameter.RefKind != RefKind.Out))
            return false;

        return IsAssignable(method.Parameters[0].Type, inputType, out _);
    }

    private static int GetDeconstructParameterOffset(IMethodSymbol deconstructMethod)
    {
        return deconstructMethod.IsExtensionMethod ? 1 : 0;
    }

    private static ITypeSymbol GetDeconstructReceiverType(IMethodSymbol deconstructMethod)
    {
        return deconstructMethod.IsExtensionMethod
            ? deconstructMethod.Parameters[0].Type
            : deconstructMethod.ContainingType;
    }

    private IMethodSymbol? FindTryGetMethod(
        ITypeSymbol? lookupType,
        IUnionSymbol unionType,
        IUnionCaseTypeSymbol caseSymbol)
        => FindTryGetMethod(lookupType, unionType, (ITypeSymbol)caseSymbol);

    private IMethodSymbol? FindTryGetMethod(
        ITypeSymbol? lookupType,
        IUnionSymbol unionType,
        ITypeSymbol targetType)
    {
        var targetCaseType = targetType.GetPlainType();
        var targetUnion = (INamedTypeSymbol)UnwrapAlias((INamedTypeSymbol)unionType);
        var targetUnionCase = targetType.TryGetUnionCase();

        bool MatchesTargetCase(IMethodSymbol method)
        {
            if (method.IsStatic || method.Parameters.Length != 1)
                return false;

            var parameter = method.Parameters[0];
            if (parameter.RefKind is not (RefKind.Out or RefKind.Ref))
                return false;

            var parameterType = parameter.GetByRefElementType().GetPlainType();
            var parameterCase = parameterType.TryGetUnionCase();
            if (parameterCase is not null)
            {
                var parameterUnion = (INamedTypeSymbol)UnwrapAlias((INamedTypeSymbol)parameterCase.Union);
                return targetUnionCase is not null &&
                    parameterCase.Ordinal == targetUnionCase.Ordinal
                    && AreSameUnionPatternTarget(parameterUnion, targetUnion);
            }

            return SymbolEqualityComparer.Default.Equals(parameterType, targetCaseType);
        }

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
        AddCandidate(targetType as INamedTypeSymbol);

        if (lookupType is INamedTypeSymbol namedLookup)
            AddCandidate(namedLookup);

        foreach (var candidate in candidateTypes)
        {
            var method = candidate
                .GetMembers("TryGetValue")
                .OfType<IMethodSymbol>()
                .FirstOrDefault(MatchesTargetCase);

            if (method is not null)
                return method;
        }

        return null;
    }

    private bool TryWrapUnionMemberPattern(
        TypeSyntax memberTypeSyntax,
        ITypeSymbol? inputType,
        ITypeSymbol memberType,
        BoundPattern innerPattern,
        out BoundPattern unionMemberPattern)
    {
        unionMemberPattern = innerPattern;

        if (!TryResolveUnionMemberPatternTarget(memberTypeSyntax, inputType, out var resolvedMemberType, out var tryGetMethod))
            return false;

        if (!AreSameUnionMemberPatternTarget(memberType, resolvedMemberType))
            return false;

        unionMemberPattern = new BoundUnionMemberPattern(inputType!, resolvedMemberType, tryGetMethod, innerPattern);
        return true;
    }

    private bool TryResolveUnionMemberPatternTarget(
        TypeSyntax memberTypeSyntax,
        ITypeSymbol? inputType,
        out ITypeSymbol memberType,
        out IMethodSymbol tryGetMethod)
    {
        memberType = Compilation.ErrorTypeSymbol;
        tryGetMethod = null!;

        if (inputType is null)
            return false;

        var unionType = inputType.TryGetUnion()
            ?? inputType.TryGetUnionCase()?.Union;

        if (unionType is null || unionType.MemberTypes.IsDefaultOrEmpty)
            return false;

        var boundType = BindTypeSyntaxAsExpression(memberTypeSyntax);
        var resolvedType = EnsureTypeAccessible(boundType.Type, memberTypeSyntax.GetLocation());
        if (resolvedType.TypeKind == TypeKind.Error)
            return false;

        var projectedMemberType = unionType.MemberTypes.FirstOrDefault(member =>
            AreSameUnionMemberPatternTarget(member, resolvedType));

        if (projectedMemberType is null)
            return false;

        var resolvedTryGetMethod = FindTryGetMethod(inputType, unionType, projectedMemberType);
        if (resolvedTryGetMethod is null)
            return false;

        memberType = projectedMemberType;
        tryGetMethod = resolvedTryGetMethod;
        return true;
    }

    private static bool AreSameUnionMemberPatternTarget(ITypeSymbol left, ITypeSymbol right)
    {
        if (SymbolEqualityComparer.Default.Equals(left, right))
            return true;

        var leftPlain = left.GetPlainType();
        var rightPlain = right.GetPlainType();
        if (SymbolEqualityComparer.Default.Equals(leftPlain, rightPlain))
            return true;

        var leftDefinition = left.OriginalDefinition ?? left;
        var rightDefinition = right.OriginalDefinition ?? right;
        return SymbolEqualityComparer.Default.Equals(leftDefinition, rightDefinition);
    }

    private BoundPattern BindCasePatternArgument(PatternSyntax syntax, ITypeSymbol parameterType)
    {
        return BindPattern(syntax, parameterType);
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
                var boundType = BindTypeSyntaxAsExpression(syntax.Type);
                narrowedType = EnsureTypeAccessible(boundType.Type, syntax.Type.GetLocation());

                // Optional: diagnose incompatibility between input and narrowed type
                if (narrowedType.TypeKind != TypeKind.Error &&
                    inputType.TypeKind != TypeKind.Error &&
                    !CanPatternMatchInputType(inputType, narrowedType))
                {
                    _diagnostics.ReportPropertyPatternTypeMismatch(
                        inputType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        narrowedType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        syntax.Type!.GetLocation());
                }
            }

            var designator3 = BindWholePatternDesignation(syntax.Designation, narrowedType ?? inputType);

            // ReceiverType is irrelevant for empty patterns; keep it as inputType to avoid ErrorType.
            return new BoundPropertyPattern(
                inputType: inputType,
                receiverType: inputType,
                narrowedType: narrowedType,
                designator3,
                properties: ImmutableArray<BoundPropertySubpattern>.Empty);
        }

        // 1) Bind explicit type filter if present: Foo in Foo { ... }
        if (syntax.Type is not null)
        {
            var boundType = BindTypeSyntaxAsExpression(syntax.Type);
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
                !CanPatternMatchInputType(inputType, receiverType))
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

                var designator2 = BindWholePatternDesignation(syntax.Designation, inputType);

                return new BoundPropertyPattern(
                    inputType: inputType,
                    receiverType: Compilation.ErrorTypeSymbol,
                    narrowedType: null,
                    designator2,
                    properties: props,
                    reason: BoundExpressionReason.MissingType);
            }
        }

        // 3) Bind subpatterns using receiverType for member lookup
        var boundProps = ImmutableArray.CreateBuilder<BoundPropertySubpattern>(
            syntax.PropertyPatternClause.Properties.Count);
        var seenPropertyNames = new HashSet<string>(StringComparer.Ordinal);

        foreach (var sub in syntax.PropertyPatternClause.Properties)
        {
            var name = sub.NameColon.Name.Identifier.ValueText;

            if (!string.IsNullOrEmpty(name) && !seenPropertyNames.Add(name))
            {
                _diagnostics.Report(Diagnostic.Create(
                    CompilerDiagnostics.DuplicatePropertyPatternMember,
                    sub.NameColon.Name.GetLocation(),
                    name));
            }

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

        var designator = BindWholePatternDesignation(syntax.Designation, narrowedType ?? inputType);

        return new BoundPropertyPattern(
            inputType: inputType,
            receiverType: receiverType,
            narrowedType: narrowedType,
            designator,
            properties: boundProps.ToImmutable());
    }

    private BoundDesignator? BindWholePatternDesignation(VariableDesignationSyntax? designation, ITypeSymbol expectedType)
    {
        if (designation is null)
            return null;

        expectedType = EnsureTypeAccessible(expectedType, designation.GetLocation());

        switch (designation)
        {
            case SingleVariableDesignationSyntax single:
                {
                    if (single.Identifier.IsMissing || string.IsNullOrEmpty(single.Identifier.ValueText) || single.Identifier.ValueText == "_")
                    {
                        var discard = new BoundDiscardDesignator(expectedType);
                        CacheBoundNode(designation, discard);
                        return discard;
                    }

                    var isMutable = single.BindingKeyword.IsKind(SyntaxKind.VarKeyword) ||
                                    (single.BindingKeyword.Kind == SyntaxKind.None && _ambientPatternDeclarationBindingKeyword == SyntaxKind.VarKeyword);
                    var local = DeclarePatternLocal(single, single.Identifier.ValueText, isMutable, expectedType);
                    var bound = new BoundSingleVariableDesignator(local);
                    CacheBoundNode(designation, bound);
                    return bound;
                }

            case TypedVariableDesignationSyntax typed:
                {
                    var declaredType = ResolveType(typed.TypeAnnotation.Type);
                    declaredType = EnsureTypeAccessible(declaredType, typed.TypeAnnotation.Type.GetLocation());

                    var inner = BindWholePatternDesignation(typed.Designation, declaredType);
                    if (inner is not null)
                        CacheBoundNode(designation, inner);
                    return inner;
                }

            // Parenthesized designations after a property pattern are parsed, but not bound (yet).
            default:
                return null;
        }
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

    private bool CanPatternMatchInputType(ITypeSymbol inputType, ITypeSymbol targetType)
    {
        inputType = StripNullableReference(UnwrapAlias(inputType));
        targetType = StripNullableReference(UnwrapAlias(targetType));

        if (inputType.TypeKind == TypeKind.Error || targetType.TypeKind == TypeKind.Error)
            return true;

        return Compilation.ClassifyConversion(inputType, targetType).Exists ||
               Compilation.ClassifyConversion(targetType, inputType).Exists;
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

    private ImmutableArray<BoundPropertySubpattern> BindNominalDeconstructionPatternSubpatternsAsDiscards(NominalDeconstructionPatternSyntax syntax)
    {
        var builder = ImmutableArray.CreateBuilder<BoundPropertySubpattern>(syntax.ArgumentList.Arguments.Count);

        foreach (var argument in syntax.ArgumentList.Arguments)
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
        ITypeSymbol type = Compilation.GetSpecialType(SpecialType.System_Object);

        for (SyntaxNode? current = single.Parent; current is not null; current = current.Parent)
        {
            // If there's an explicit type annotation on the designation, it wins.
            if (current is TypedVariableDesignationSyntax typed)
            {
                // Enforce: typed pattern bindings require explicit let/val/var.
                // Example that should diagnose: (a: bool, b: string)
                // Example that should NOT diagnose: (val a: bool, var b: string)
                if (!HasExplicitBindingKeyword(
                    single,
                    hasAmbientBindingKeyword: _ambientPatternDeclarationBindingKeyword is SyntaxKind.LetKeyword or SyntaxKind.ValKeyword or SyntaxKind.VarKeyword))
                {
                    _diagnostics.ReportPatternTypedBindingRequiresKeyword(
                        single.Identifier.ValueText,
                        typed.TypeAnnotation.Type.ToString(),
                        single.Identifier.GetLocation());
                }

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

            // Stop climbing once we leave "designation territory" enough.
            if (current is PatternSyntax)
                break;
        }

        type = EnsureTypeAccessible(type, single.Identifier.GetLocation());

        // If a binding keyword is present on this single variable, it overrides the mutability.
        var isMutable = !single.BindingKeyword.IsMissing && single.BindingKeyword.IsKind(SyntaxKind.VarKeyword);

        var local = DeclarePatternLocal(single, name, isMutable, type);
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
        BoundDesignator? designator,
        ImmutableArray<BoundPropertySubpattern> properties,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(inputType, reason)
    {
        InputType = inputType;
        ReceiverType = receiverType;
        NarrowedType = narrowedType;
        Designator = designator;
        Properties = properties;
    }

    public ITypeSymbol InputType { get; }
    public ITypeSymbol ReceiverType { get; }
    public ITypeSymbol? NarrowedType { get; }
    public BoundDesignator? Designator { get; }
    public ImmutableArray<BoundPropertySubpattern> Properties { get; }

    public override IEnumerable<BoundDesignator> GetDesignators()
    {
        if (Designator is not null && Designator is not BoundDiscardDesignator)
            yield return Designator;

        foreach (var p in Properties)
            foreach (var d in p.Pattern.GetDesignators())
                yield return d;
    }

    public override void Accept(BoundTreeVisitor visitor) => visitor.DefaultVisit(this);
    public override TResult Accept<TResult>(BoundTreeVisitor<TResult> visitor) => visitor.DefaultVisit(this);
}

internal sealed class BoundDictionaryPattern : BoundPattern
{
    public BoundDictionaryPattern(
        ITypeSymbol inputType,
        ITypeSymbol receiverType,
        ITypeSymbol keyType,
        ITypeSymbol valueType,
        BoundDesignator? designator,
        ImmutableArray<BoundDictionarySubpattern> entries,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(inputType, reason)
    {
        InputType = inputType;
        ReceiverType = receiverType;
        KeyType = keyType;
        ValueType = valueType;
        Designator = designator;
        Entries = entries;
    }

    public ITypeSymbol InputType { get; }
    public ITypeSymbol ReceiverType { get; }
    public ITypeSymbol KeyType { get; }
    public ITypeSymbol ValueType { get; }
    public BoundDesignator? Designator { get; }
    public ImmutableArray<BoundDictionarySubpattern> Entries { get; }

    public override IEnumerable<BoundDesignator> GetDesignators()
    {
        if (Designator is not null && Designator is not BoundDiscardDesignator)
            yield return Designator;

        foreach (var entry in Entries)
        {
            foreach (var designator in entry.Pattern.GetDesignators())
                yield return designator;
        }
    }

    public override void Accept(BoundTreeVisitor visitor) => visitor.DefaultVisit(this);
    public override TResult Accept<TResult>(BoundTreeVisitor<TResult> visitor) => visitor.DefaultVisit(this);
}

internal readonly record struct BoundDictionarySubpattern(
    BoundExpression Key,
    BoundPattern Pattern);

internal readonly record struct BoundPropertySubpattern(
    ISymbol Member,
    ITypeSymbol Type,
    BoundPattern Pattern);

internal enum BoundComparisonPatternOperator
{
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

internal sealed class BoundComparisonPattern : BoundPattern
{
    public BoundComparisonPattern(
        ITypeSymbol inputType,
        BoundComparisonPatternOperator @operator,
        BoundExpression value,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(inputType, reason)
    {
        InputType = inputType;
        Operator = @operator;
        Value = value;
    }

    public ITypeSymbol InputType { get; }
    public BoundComparisonPatternOperator Operator { get; }
    public BoundExpression Value { get; }

    public override void Accept(BoundTreeVisitor visitor) => visitor.DefaultVisit(this);
    public override TResult Accept<TResult>(BoundTreeVisitor<TResult> visitor) => visitor.DefaultVisit(this);
}

internal sealed class BoundRangePattern : BoundPattern
{
    public BoundRangePattern(
        ITypeSymbol inputType,
        BoundExpression? lowerBound,
        BoundExpression? upperBound,
        bool isUpperExclusive = false,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(inputType, reason)
    {
        LowerBound = lowerBound;
        UpperBound = upperBound;
        IsUpperExclusive = isUpperExclusive;
    }

    /// <summary>Lower inclusive bound expression, or null for open-ended lower bound.</summary>
    public BoundExpression? LowerBound { get; }

    /// <summary>Upper bound expression, or null for open-ended upper bound.</summary>
    public BoundExpression? UpperBound { get; }

    public bool IsUpperExclusive { get; }

    public override IEnumerable<BoundDesignator> GetDesignators() => [];

    public override void Accept(BoundTreeVisitor visitor) => visitor.DefaultVisit(this);
    public override TResult Accept<TResult>(BoundTreeVisitor<TResult> visitor) => visitor.DefaultVisit(this);
}

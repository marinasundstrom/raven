using System.Collections.Generic;
using System.Collections.Immutable;

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

internal sealed class BoundCasePattern : BoundPattern
{
    public BoundCasePattern(
        INamedTypeSymbol unionType,
        INamedTypeSymbol caseType,
        IMethodSymbol? tryGetMethod,
        ImmutableArray<BoundCasePatternArgument> arguments,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(unionType, reason)
    {
        UnionType = unionType;
        CaseType = caseType;
        TryGetMethod = tryGetMethod;
        Arguments = arguments;
    }

    public INamedTypeSymbol UnionType { get; }
    public INamedTypeSymbol CaseType { get; }
    public IMethodSymbol? TryGetMethod { get; }
    public ImmutableArray<BoundCasePatternArgument> Arguments { get; }

    public override IEnumerable<BoundDesignator> GetDesignators()
    {
        foreach (var argument in Arguments)
        {
            foreach (var designator in argument.Pattern.GetDesignators())
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

internal readonly struct BoundCasePatternArgument
{
    public BoundCasePatternArgument(IParameterSymbol parameter, IPropertySymbol? property, BoundPattern pattern)
    {
        Parameter = parameter;
        Property = property;
        Pattern = pattern;
    }

    public IParameterSymbol Parameter { get; }
    public IPropertySymbol? Property { get; }
    public BoundPattern Pattern { get; }
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
        var previousInput = _currentPatternInputType;
        if (inputType is not null)
            _currentPatternInputType = inputType;

        try
        {
            return syntax switch
            {
                DiscardPatternSyntax discard => BindDiscardPattern(discard),
                VariablePatternSyntax variable => BindVariablePattern(variable),
                DeclarationPatternSyntax d => BindDeclarationPattern(d),
                TuplePatternSyntax t => BindTuplePattern(t),
                UnaryPatternSyntax u => BindUnaryPattern(u),
                BinaryPatternSyntax b => BindBinaryPattern(b),
                CasePatternSyntax c => BindCasePattern(c),
                _ => throw new NotImplementedException($"Unknown pattern kind: {syntax.Kind}")
            };
        }
        finally
        {
            _currentPatternInputType = previousInput;
        }
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

    private BoundPattern BindTuplePattern(TuplePatternSyntax syntax)
    {
        var expectedElementTypes = ImmutableArray<ITypeSymbol>.Empty;

        if (_currentPatternInputType is ITypeSymbol tupleInput)
            expectedElementTypes = GetTupleElementTypes(tupleInput);

        var elementPatterns = ImmutableArray.CreateBuilder<BoundPattern>(syntax.Elements.Count);

        for (var i = 0; i < syntax.Elements.Count; i++)
        {
            var elementSyntax = syntax.Elements[i];
            var elementType = expectedElementTypes.Length > i ? expectedElementTypes[i] : null;
            var boundElement = BindPattern(elementSyntax.Pattern, elementType);
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

    private BoundPattern BindCasePattern(CasePatternSyntax syntax)
    {
        var caseName = syntax.Identifier.ValueText;
        if (string.IsNullOrEmpty(caseName))
            return CreateErrorCasePattern();

        var unionType = ResolveCasePatternTargetType(syntax);

        if (unionType is null)
        {
            _diagnostics.ReportCasePatternRequiresType(caseName, syntax.GetLocation());
            return CreateErrorCasePattern();
        }

        if (!DiscriminatedUnionFacts.IsDiscriminatedUnionType(unionType))
        {
            var location = syntax.Type?.GetLocation() ?? syntax.GetLocation();
            _diagnostics.ReportCasePatternRequiresDiscriminatedUnion(
                unionType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                location);
            return CreateErrorCasePattern(unionType);
        }

        var caseType = TryResolveCaseType(unionType, caseName, syntax.Identifier.GetLocation());

        if (caseType is null)
        {
            _diagnostics.ReportCasePatternCaseNotFound(
                unionType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                caseName,
                syntax.Identifier.GetLocation());
            return CreateErrorCasePattern(unionType);
        }

        var parameters = GetDiscriminatedUnionCaseParameters(caseType);
        var arguments = BindCasePatternArguments(caseName, syntax.ArgumentList, parameters, caseType);
        var tryGetMethod = FindTryGetMethod(unionType, caseName, caseType);

        return new BoundCasePattern(unionType, caseType, tryGetMethod, arguments);
    }

    private INamedTypeSymbol? ResolveCasePatternTargetType(CasePatternSyntax syntax)
    {
        if (syntax.Type is { } typeSyntax)
        {
            var boundType = BindTypeSyntax(typeSyntax);
            var typeSymbol = boundType.Type ?? Compilation.ErrorTypeSymbol;
            var accessible = EnsureTypeAccessible(typeSymbol, typeSyntax.GetLocation());
            var normalized = UnwrapAlias(accessible);
            return normalized as INamedTypeSymbol;
        }

        if (_currentPatternInputType is ITypeSymbol inputType)
        {
            var normalized = UnwrapAlias(inputType);
            return normalized as INamedTypeSymbol;
        }

        return null;
    }

    private INamedTypeSymbol? TryResolveCaseType(INamedTypeSymbol unionType, string caseName, Location location)
    {
        foreach (var member in unionType.GetMembers(caseName))
        {
            if (member is not INamedTypeSymbol typeMember)
                continue;

            if (!DiscriminatedUnionFacts.IsDiscriminatedUnionCaseType(typeMember))
                continue;

            var accessible = EnsureTypeAccessible(typeMember, location);
            var normalized = UnwrapAlias(accessible);

            if (normalized is INamedTypeSymbol named)
                return named;
        }

        return null;
    }

    private ImmutableArray<IParameterSymbol> GetDiscriminatedUnionCaseParameters(INamedTypeSymbol caseType)
    {
        if (caseType.TryGetDiscriminatedUnionCase() is { } caseSymbol &&
            !caseSymbol.ConstructorParameters.IsDefaultOrEmpty)
        {
            return caseSymbol.ConstructorParameters;
        }

        IMethodSymbol? bestMatch = null;

        foreach (var ctor in caseType.Constructors)
        {
            if (ctor.IsStatic)
                continue;

            if (bestMatch is null || ctor.Parameters.Length > bestMatch.Parameters.Length)
                bestMatch = ctor;
        }

        return bestMatch?.Parameters ?? ImmutableArray<IParameterSymbol>.Empty;
    }

    private ImmutableArray<BoundCasePatternArgument> BindCasePatternArguments(
        string caseName,
        CasePatternArgumentListSyntax? argumentList,
        ImmutableArray<IParameterSymbol> parameters,
        INamedTypeSymbol caseType)
    {
        if (argumentList is null)
            return ImmutableArray<BoundCasePatternArgument>.Empty;

        var argumentCount = argumentList.Arguments.Count;

        if (argumentCount != parameters.Length)
        {
            _diagnostics.ReportCasePatternArgumentCountMismatch(
                caseName,
                parameters.Length,
                argumentCount,
                argumentList.GetLocation());
        }

        var count = Math.Min(argumentCount, parameters.Length);
        var builder = ImmutableArray.CreateBuilder<BoundCasePatternArgument>(count);

        for (var i = 0; i < count; i++)
        {
            var parameter = parameters[i];
            var parameterType = parameter.Type ?? Compilation.ErrorTypeSymbol;
            var argumentSyntax = argumentList.Arguments[i];
            var boundArgument = BindPattern(argumentSyntax, parameterType);
            var property = FindCasePatternProperty(caseType, parameter);
            builder.Add(new BoundCasePatternArgument(parameter, property, boundArgument));
        }

        return builder.ToImmutable();
    }

    private IPropertySymbol? FindCasePatternProperty(INamedTypeSymbol caseType, IParameterSymbol parameter)
    {
        if (string.IsNullOrEmpty(parameter.Name))
            return null;

        var parameterType = parameter.Type;
        if (parameterType is null)
            return null;

        var propertyName = DiscriminatedUnionFacts.GetCasePropertyName(parameter.Name);

        foreach (var member in caseType.GetMembers(propertyName))
        {
            if (member is IPropertySymbol property &&
                !property.IsIndexer &&
                SymbolEqualityComparer.Default.Equals(property.Type, parameterType))
            {
                return property;
            }
        }

        return null;
    }

    private IMethodSymbol? FindTryGetMethod(
        INamedTypeSymbol unionType,
        string caseName,
        INamedTypeSymbol caseType)
    {
        var methodName = $"TryGet{caseName}";

        foreach (var member in unionType.GetMembers(methodName))
        {
            if (member is not IMethodSymbol method || method.IsStatic)
                continue;

            if (method.Parameters.Length != 1)
                continue;

            var parameter = method.Parameters[0];
            if (parameter.RefKind is not (RefKind.Ref or RefKind.RefReadOnly))
                continue;

            var parameterType = UnwrapAlias(parameter.Type);

            if (parameterType is INamedTypeSymbol named &&
                SymbolEqualityComparer.Default.Equals(named, caseType))
            {
                return method;
            }
        }

        return null;
    }

    private BoundCasePattern CreateErrorCasePattern(INamedTypeSymbol? unionType = null)
    {
        var errorType = (INamedTypeSymbol)Compilation.ErrorTypeSymbol;
        var resolvedUnion = unionType ?? errorType;
        return new BoundCasePattern(
            resolvedUnion,
            errorType,
            tryGetMethod: null,
            ImmutableArray<BoundCasePatternArgument>.Empty,
            BoundExpressionReason.TypeMismatch);
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

    private BoundPattern BindUnaryPattern(UnaryPatternSyntax syntax)
    {
        var operand = BindPattern(syntax.Pattern);
        return syntax.Kind switch
        {
            SyntaxKind.NotPattern => new BoundNotPattern(operand),
            _ => throw new NotImplementedException($"Unsupported unary pattern: {syntax.Kind}")
        };
    }

    private BoundPattern BindBinaryPattern(BinaryPatternSyntax syntax)
    {
        var left = BindPattern(syntax.Left);
        var right = BindPattern(syntax.Right);

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
        var declaration = singleVariableDesignation.Parent as DeclarationPatternSyntax;
        var name = singleVariableDesignation.Identifier.ValueText;
        var type = ResolveType(declaration.Type);

        var local = CreateLocalSymbol(singleVariableDesignation, name, isMutable: false, type);

        return new BoundSingleVariableDesignator(local);
    }
}

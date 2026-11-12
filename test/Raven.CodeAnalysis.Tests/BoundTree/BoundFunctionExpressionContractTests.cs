using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundFunctionExpressionContractTests
{
    [Fact]
    public void NodePreservesStateAndUpdateIdentity()
    {
        var type = Type("Value");
        var body = Literal(type);
        var symbol = Make<MethodSymbol>(SymbolKind.Method, "Lambda");
        var parameters = new[] { Param(type) };
        var captures = new Symbol[] { symbol };
        var candidates = ImmutableArray.Create(Delegate(type));
        var node = new BoundFunctionExpression(parameters, type, body, symbol, candidates[0], captures, candidates);
        Assert.Same(parameters, node.Parameters);
        Assert.Same(type, node.ReturnType);
        Assert.Same(body, node.Body);
        Assert.Same(symbol, node.Symbol);
        Assert.Same(candidates[0], node.Type);
        Assert.Same(candidates[0], node.DelegateType);
        Assert.Same(captures, node.CapturedVariables);
        Assert.Equal(candidates, node.CandidateDelegates);
        Assert.Equal(BoundExpressionReason.None, node.Reason);
        Assert.Same(node, node.Update(parameters, type, body, symbol, candidates[0], captures, candidates));
        var changedBody = Literal(type);
        var changed = node.Update(parameters, type, changedBody, symbol, candidates[0], captures, candidates);
        Assert.NotSame(node, changed);
        Assert.Same(changedBody, changed.Body);
    }

    [Theory]
    [InlineData(0, true)]
    [InlineData(1, false)]
    public void NoCandidatesUsesTargetArity(int count, bool expected)
    {
        var type = Type("Value");
        var target = Delegate(type, Enumerable.Repeat(Param(type), count).ToArray());
        var services = new Services();
        Assert.Equal(expected, Node(type).IsCompatibleWithDelegate(target, services));
        Assert.Empty(services.Conversions);
    }

    [Fact]
    public void NonDelegateOrMissingInvokeIsRejected()
    {
        var type = Type("Value");
        var missing = Make<NamedTypeSymbol>(SymbolKind.Type, "Missing", TypeKind.Delegate);
        Set(missing, "GetMembers", ImmutableArray<Symbol>.Empty);
        Assert.False(Node(type).IsCompatibleWithDelegate(type, new Services()));
        Assert.False(Node(type).IsCompatibleWithDelegate(missing, new Services()));
    }

    [Fact]
    public void ExactCandidateNeedsNoConversion()
    {
        var type = Type("Value");
        var target = Delegate(type);
        var services = new Services();
        Assert.True(Node(type, [target]).IsCompatibleWithDelegate(target, services));
        Assert.Empty(services.Conversions);
    }

    [Theory]
    [InlineData(true, true)]
    [InlineData(false, false)]
    public void ReturnConversionMustBeImplicit(bool implicitConversion, bool expected)
    {
        var sourceType = Make<TypeSymbol>(SymbolKind.Type, "Source", TypeKind.Error);
        var targetType = Make<TypeSymbol>(SymbolKind.Type, "Target", TypeKind.Error);
        var source = Delegate(sourceType);
        var target = Delegate(targetType);
        var services = new Services { Result = new Conversion(isImplicit: implicitConversion) };
        Assert.Equal(expected, Node(sourceType, [source]).IsCompatibleWithDelegate(target, services));
        var conversion = Assert.Single(services.Conversions);
        Assert.Same(sourceType, conversion.Source);
        Assert.Same(targetType, conversion.Destination);
        Assert.True(conversion.IncludeUserDefined);
    }

    [Theory]
    [InlineData(true, true)]
    [InlineData(false, false)]
    public void ParameterConversionMustBeImplicit(bool implicitConversion, bool expected)
    {
        var sourceType = Make<TypeSymbol>(SymbolKind.Type, "Source", TypeKind.Error);
        var targetType = Make<TypeSymbol>(SymbolKind.Type, "Target", TypeKind.Error);
        var resultType = Type("Result");
        var source = Delegate(resultType, Param(sourceType));
        var target = Delegate(resultType, Param(targetType));
        var services = new Services { Result = new Conversion(isImplicit: implicitConversion) };
        Assert.Equal(expected, Node(resultType, [source]).IsCompatibleWithDelegate(target, services));
        var conversion = Assert.Single(services.Conversions);
        Assert.Same(sourceType, conversion.Source);
        Assert.Same(targetType, conversion.Destination);
        Assert.True(conversion.IncludeUserDefined);
    }

    [Fact]
    public void InvalidCandidatesAreSkippedAndSignatureArityMustMatch()
    {
        var type = Type("Value");
        var missing = Make<NamedTypeSymbol>(SymbolKind.Type, "Missing", TypeKind.Delegate);
        Set(missing, "GetMembers", ImmutableArray<Symbol>.Empty);
        var target = Delegate(type);
        var services = new Services();
        Assert.True(Node(type, [type, missing, target]).IsCompatibleWithDelegate(target, services));
        Assert.False(Node(type, [Delegate(type, Param(type))]).IsCompatibleWithDelegate(target, services));
        Assert.Empty(services.Conversions);
    }

    [Fact]
    public void RefKindMismatchRejectsBeforeConversion()
    {
        var type = Type("Value");
        var source = Delegate(type, Param(type, RefKind.Ref));
        var target = Delegate(type, Param(type));
        var services = new Services();
        Assert.False(Node(type, [source]).IsCompatibleWithDelegate(target, services));
        Assert.Empty(services.Conversions);
    }

    [Fact]
    public void GenericParameterMappingReconstructsArrayWithRankAndLength()
    {
        var parameter = Parameter("T", 0);
        var concrete = Type("Value");
        var sourceArray = Array(parameter, 2, 8);
        var targetArray = Array(concrete, 2, 8);
        var source = Delegate(parameter, Param(sourceArray));
        var target = Delegate(concrete, Param(targetArray));
        var services = new Services { ArrayResult = targetArray };
        Assert.True(Node(concrete, [source]).IsCompatibleWithDelegate(target, services));
        var request = Assert.Single(services.Arrays);
        Assert.Same(concrete, request.Element);
        Assert.Equal(2, request.Rank);
        Assert.Equal(8, request.Length);
        Assert.Empty(services.Conversions);
    }

    [Theory]
    [InlineData("ref")]
    [InlineData("address")]
    [InlineData("nullable")]
    public void GenericMappingReconstructsWrapper(string kind)
    {
        var parameter = Parameter("T", 0);
        Set(parameter, "get_BaseType", null);
        var concrete = Type("Value");
        TypeSymbol Wrap(TypeSymbol element) => kind switch
        {
            "ref" => new RefTypeSymbol(element),
            "address" => new AddressTypeSymbolImplementation(element),
            _ => new NullableTypeSymbol(element, null, null, null, [], NullableAbiProjection.AnnotatedUnderlyingType)
        };
        var source = Delegate(parameter, Param(Wrap(parameter)));
        var target = Delegate(concrete, Param(Wrap(concrete)));
        var services = new Services();
        Assert.True(Node(concrete, [source]).IsCompatibleWithDelegate(target, services));
        Assert.Empty(services.Conversions);
        Assert.Empty(services.Arrays);
    }

    [Fact]
    public void GenericNamedArgumentsAreReconstructedThroughDefinition()
    {
        var parameter = Parameter("T", 0);
        var concrete = Type("Value");
        var definition = Type("Container");
        var sourceType = Type("Container");
        var targetType = Type("Container");
        foreach (var type in new[] { sourceType, targetType })
        {
            Set(type, "get_ConstructedFrom", definition);
            Set(type, "get_IsGenericType", true);
            Set(type, "get_Arity", 1);
        }
        Set(sourceType, "get_TypeArguments", ImmutableArray.Create<TypeSymbol>(parameter));
        Set(targetType, "get_TypeArguments", ImmutableArray.Create<TypeSymbol>(concrete));
        var constructed = 0;
        Set(definition, "Construct", (Func<object?[]?, object?>)(arguments =>
        {
            Assert.Same(concrete, Assert.Single((TypeSymbol[])arguments![0]!));
            constructed++;
            return targetType;
        }));
        var services = new Services();
        Assert.True(Node(concrete, [Delegate(concrete, Param(sourceType))])
            .IsCompatibleWithDelegate(Delegate(concrete, Param(targetType)), services));
        Assert.Equal(1, constructed);
        Assert.Empty(services.Conversions);
    }

    [Fact]
    public void ConflictingRepeatedTypeParameterMappingRejectsCandidate()
    {
        var parameter = Parameter("T", 0);
        var first = Type("First");
        var second = Type("Second");
        var source = Delegate(parameter, Param(parameter), Param(parameter));
        var target = Delegate(first, Param(first), Param(second));
        Assert.False(Node(first, [source]).IsCompatibleWithDelegate(target, new Services()));
    }

    [Fact]
    public void WalkerVisitsBodyAndGenericVisitorDispatches()
    {
        var node = Node(Type("Value"));
        var walker = new Walker();
        walker.Visit(node);
        Assert.Same(node.Body, Assert.Single(walker.Bodies));
        Assert.Equal(42, new Visitor().Visit(node));
    }

    private sealed class Walker : BoundTreeWalker
    {
        public List<BoundExpression> Bodies { get; } = [];
        public override void VisitLiteralExpression(BoundLiteralExpression node) => Bodies.Add(node);
    }

    private sealed class Visitor : BoundTreeVisitor<int>
    {
        public override int VisitFunctionExpression(BoundFunctionExpression node) => 42;
    }

    private sealed class Services : FunctionExpressionServices
    {
        public Conversion Result { get; init; } = Conversion.None;
        public TypeSymbol? ArrayResult { get; init; }
        public List<(TypeSymbol Source, TypeSymbol Destination, bool IncludeUserDefined)> Conversions { get; } = [];
        public List<(TypeSymbol Element, int Rank, int? Length)> Arrays { get; } = [];
        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination, bool includeUserDefined)
        {
            Conversions.Add((source, destination, includeUserDefined));
            return Result;
        }
        public TypeSymbol CreateArrayTypeSymbol(TypeSymbol elementType, int rank, int? fixedLength)
        {
            Arrays.Add((elementType, rank, fixedLength));
            return ArrayResult ?? throw new InvalidOperationException("Unexpected array construction.");
        }
    }

    private static BoundFunctionExpression Node(TypeSymbol type, ImmutableArray<NamedTypeSymbol> candidates = default) =>
        new([], type, Literal(type), Make<MethodSymbol>(SymbolKind.Method, "Lambda"), type, [], candidates);

    private static BoundLiteralExpression Literal(TypeSymbol type) => new(BoundLiteralExpressionKind.NumericLiteral, 1, type);

    private static NamedTypeSymbol Type(string name)
    {
        var type = Make<NamedTypeSymbol>(SymbolKind.Type, name, TypeKind.Class);
        Set(type, "get_TypeArguments", ImmutableArray<TypeSymbol>.Empty);
        Set(type, "get_ConstructedFrom", type);
        Set(type, "get_BaseType", null);
        Set(type, "get_IsNullable", false);
        return type;
    }

    private static ParameterSymbol Param(TypeSymbol type, RefKind refKind = RefKind.None)
    {
        var parameter = Make<ParameterSymbol>(SymbolKind.Parameter, "arg");
        Set(parameter, "get_Type", type);
        Set(parameter, "get_RefKind", refKind);
        return parameter;
    }

    private static NamedTypeSymbol Delegate(TypeSymbol result, params ParameterSymbol[] parameters)
    {
        var type = Make<NamedTypeSymbol>(SymbolKind.Type, "Callback" + Guid.NewGuid().ToString("N"), TypeKind.Delegate);
        var invoke = Make<MethodSymbol>(SymbolKind.Method, "Invoke");
        // Detached signatures exercise compatibility independently of declaration ownership.
        Set(invoke, "get_Parameters", parameters.ToImmutableArray());
        Set(invoke, "get_ReturnType", result);
        Set(type, "GetMembers", ImmutableArray.Create<Symbol>(invoke));
        return type;
    }

    private static ArrayTypeSymbol Array(TypeSymbol element, int rank, int? length)
    {
        var type = Make<ArrayTypeSymbol>(SymbolKind.Type, "Array", TypeKind.Array);
        Set(type, "get_ElementType", element);
        Set(type, "get_Rank", rank);
        Set(type, "get_FixedLength", length);
        return type;
    }
}

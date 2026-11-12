using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundNodeFactsContractTests
{
    private static NamedTypeSymbol Type() => Named("Facts", "Example.Value");

    [Fact]
    public void MissingExpressionsAndOrdinaryValuesAreNotAbrupt()
    {
        Assert.False(BoundNodeFacts.IsAbruptExpression(null));
        Assert.False(BoundNodeFacts.IsAbruptExpression(new BoundDefaultValueExpression(Type())));
        Assert.True(BoundNodeFacts.MatchArmGuardGuaranteesMatch(null));
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void BooleanGuardsUnwrapOnlyIdentityConversions(bool value)
    {
        var type = Type();
        var literal = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, value, type);
        Assert.Equal(value, BoundNodeFacts.MatchArmGuardGuaranteesMatch(literal));
        var wrapped = new BoundParenthesizedExpression(new BoundConversionExpression(literal, type, new Conversion(isImplicit: true, isIdentity: true)));
        Assert.Equal(value, BoundNodeFacts.MatchArmGuardGuaranteesMatch(wrapped));
        Assert.False(BoundNodeFacts.MatchArmGuardGuaranteesMatch(new BoundConversionExpression(literal, type, new Conversion(isNumeric: true))));
        Assert.True(BoundNodeFacts.MatchArmGuardGuaranteesMatch(new BoundLiteralExpression(BoundLiteralExpressionKind.TrueLiteral, null, type)));
        Assert.False(BoundNodeFacts.MatchArmGuardGuaranteesMatch(new BoundLiteralExpression(BoundLiteralExpressionKind.FalseLiteral, null, type)));
        Assert.False(BoundNodeFacts.MatchArmGuardGuaranteesMatch(new BoundDefaultValueExpression(type)));
    }

    [Fact]
    public void ReturnTransfersThroughParenthesesAndConversions()
    {
        var type = Type();
        var expression = new BoundParenthesizedExpression(new BoundConversionExpression(
            new BoundReturnExpression(null, type), type, new Conversion(isImplicit: true, isIdentity: true)));
        Assert.True(BoundNodeFacts.IsAbruptExpression(expression));
        Assert.True(BoundNodeFacts.ContainsControlTransfer(expression));
    }

    [Fact]
    public void ConditionalRequiresBothBranchesUnlessConditionTransfers()
    {
        var type = Type();
        var value = new BoundDefaultValueExpression(type);
        var exit = new BoundReturnExpression(null, type);
        Assert.True(BoundNodeFacts.IsAbruptExpression(new BoundIfExpression(value, exit, exit)));
        Assert.False(BoundNodeFacts.IsAbruptExpression(new BoundIfExpression(value, exit, value)));
        Assert.True(BoundNodeFacts.IsAbruptExpression(new BoundIfExpression(exit, value, value)));
        Assert.False(BoundNodeFacts.IsAbruptExpression(new BoundIfExpression(value, exit)));
        Assert.True(BoundNodeFacts.ContainsControlTransfer(new BoundIfExpression(value, exit, value)));
    }

    [Fact]
    public void BlockAbruptnessUsesLastStatementWhileTransferSearchWalksAllStatements()
    {
        var type = Type();
        var ordinary = new BoundExpressionStatement(new BoundDefaultValueExpression(type));
        var block = new BoundBlockExpression([new BoundReturnStatement(null), ordinary], type);
        Assert.False(BoundNodeFacts.IsAbruptExpression(block));
        Assert.True(BoundNodeFacts.ContainsControlTransfer(block));
        Assert.True(BoundNodeFacts.IsAbruptExpression(new BoundBlockExpression([ordinary, new BoundBreakStatement()], type)));
        Assert.False(BoundNodeFacts.IsAbruptExpression(new BoundBlockExpression([], type)));
    }

    [Fact]
    public void NestedFunctionTransfersStayWithinTheirOwnScope()
    {
        var type = Type();
        var body = new BoundReturnExpression(null, type);
        var function = new BoundFunctionExpression([], type, body,
            Make<MethodSymbol>(SymbolKind.Method, "lambda"), type, [], []);
        Assert.True(BoundNodeFacts.ContainsControlTransfer(body));
        Assert.False(BoundNodeFacts.ContainsControlTransfer(function));
        Assert.False(BoundNodeFacts.IsAbruptExpression(function));
    }

    [Fact]
    public void ConditionalAccessOnlyGuaranteesEvaluationOfReceiver()
    {
        var type = Type();
        var value = new BoundDefaultValueExpression(type);
        var exit = new BoundReturnExpression(null, type);
        Assert.False(BoundNodeFacts.IsAbruptExpression(new BoundConditionalAccessExpression(value, exit, type)));
        Assert.True(BoundNodeFacts.IsAbruptExpression(new BoundConditionalAccessExpression(exit, value, type)));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ObjectInitializerEntriesPropagateAbruptEvaluation(bool assignment)
    {
        var type = Type();
        var constructor = Make<MethodSymbol>(SymbolKind.Method, ".ctor");
        Set(constructor, "get_ContainingType", type);
        var exit = new BoundReturnExpression(null, type);
        BoundObjectInitializerEntry entry = assignment
            ? new BoundObjectInitializerAssignmentEntry(Make<PropertySymbol>(SymbolKind.Property, "Value"), exit)
            : new BoundObjectInitializerExpressionEntry(exit);
        var expression = new BoundObjectCreationExpression(constructor, [], initializer: new BoundObjectInitializer([entry]));
        Assert.True(BoundNodeFacts.IsAbruptExpression(expression));
        Assert.False(BoundNodeFacts.IsAbruptExpression(new BoundObjectCreationExpression(constructor, [])));
    }

    [Theory]
    [InlineData(Raven.CodeAnalysis.Syntax.SyntaxKind.AmpersandAmpersandToken, false)]
    [InlineData(Raven.CodeAnalysis.Syntax.SyntaxKind.BarBarToken, false)]
    [InlineData(Raven.CodeAnalysis.Syntax.SyntaxKind.AmpersandToken, true)]
    public void BinaryRightOperandAbruptnessRespectsShortCircuiting(Raven.CodeAnalysis.Syntax.SyntaxKind token, bool expected)
    {
        var type = Type();
        Set(type, "get_SpecialType", SpecialType.System_Boolean);
        Set(type, "get_EnumUnderlyingType", null);
        Assert.True(BoundBinaryOperator.TryLookup(new BooleanServices(type), token, type, type, out var op));
        var value = new BoundDefaultValueExpression(type);
        var exit = new BoundReturnExpression(null, type);
        Assert.Equal(expected, BoundNodeFacts.IsAbruptExpression(new BoundBinaryExpression(value, op, exit)));
        Assert.Equal(expected, BoundNodeFacts.IsAbruptExpression(new BoundBinaryExpression(value, op.WithChecked(), exit)));
        Assert.True(BoundNodeFacts.IsAbruptExpression(new BoundBinaryExpression(exit, op, value)));
    }

    private sealed class BooleanServices(NamedTypeSymbol type) : BinaryOperatorServices
    {
        public TypeSymbol ErrorTypeSymbol => type;
        public NamedTypeSymbol GetSpecialType(SpecialType specialType) => type;
        public bool HasRuntimeSelfContract => false;
        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination, bool includeUserDefined) => throw new NotSupportedException();
        public TypeSymbol CreateNullableType(TypeSymbol value) => throw new NotSupportedException();
        public MethodSymbol? ProjectRuntimeSelfOperator(TypeSymbol receiver, MethodSymbol method) => throw new NotSupportedException();
    }

    [Fact]
    public void StatementConditionalRequiresBothBranchesToTransfer()
    {
        var type = Type();
        var value = new BoundDefaultValueExpression(type);
        var both = new BoundIfStatement(value, new BoundReturnStatement(null), new BoundContinueStatement());
        var one = new BoundIfStatement(value, new BoundReturnStatement(null));
        Assert.True(BoundNodeFacts.IsAbruptExpression(new BoundBlockExpression([both], type)));
        Assert.False(BoundNodeFacts.IsAbruptExpression(new BoundBlockExpression([one], type)));
    }
}

[Trait("Target", "NeoCLR")]
public sealed class BoundTerminalFaultContractTests
{
    internal interface RuntimeContainer : NamedTypeSymbol, Metadata.ICliRuntimeNamespaceContainer { }

    [Theory]
    [InlineData("valid", true)]
    [InlineData("assembly", false)]
    [InlineData("namespace", false)]
    [InlineData("name", false)]
    [InlineData("instance", false)]
    [InlineData("generic", false)]
    [InlineData("unmarked", false)]
    [InlineData("byref", false)]
    [InlineData("parameter", false)]
    [InlineData("return", false)]
    public void LegacySignatureAndProviderOwnershipRemainExact(string difference, bool expected)
    {
        var container = Make<RuntimeContainer>(SymbolKind.Type, "ArbitraryCarrier");
        Set(container, "get_IsRuntimeNamespaceContainer", difference != "unmarked");
        var assembly = Make<AssemblySymbol>(SymbolKind.Assembly, difference == "assembly" ? "Other" : "NeoCLR.CoreProbe");
        var ns = Make<NamespaceSymbol>(SymbolKind.Namespace, "System");
        Set(ns, "ToMetadataName", difference == "namespace" ? "Other" : "System");
        var text = Named("System", "System.String");
        Set(text, "get_SpecialType", difference == "parameter" ? SpecialType.System_Int32 : SpecialType.System_String);
        var result = Named("System", "System.Void");
        Set(result, "get_SpecialType", difference == "return" ? SpecialType.System_Int32 : SpecialType.System_Void);
        var parameter = Make<ParameterSymbol>(SymbolKind.Parameter, "message");
        Set(parameter, "get_Type", text);
        Set(parameter, "get_RefKind", difference == "byref" ? RefKind.Ref : RefKind.None);
        var method = Make<MethodSymbol>(SymbolKind.Method, difference == "name" ? "Fail" : "Panic");
        Set(method, "get_IsStatic", difference != "instance");
        Set(method, "get_IsGenericMethod", difference == "generic");
        Set(method, "get_ContainingType", container);
        Set(method, "get_ContainingAssembly", assembly);
        Set(method, "get_ContainingNamespace", ns);
        Set(method, "get_Parameters", System.Collections.Immutable.ImmutableArray.Create(parameter));
        Set(method, "get_ReturnType", result);
        Assert.Equal(expected, BoundNodeFacts.IsTerminalRuntimeFault(method));
    }
}

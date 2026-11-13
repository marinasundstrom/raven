using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundTreeOptimizerContractTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void MissingOptimizationContextLeavesTheRootUntouched(bool hasAssembly)
    {
        var owner = Make<MethodSymbol>(SymbolKind.Method, "Owner");
        Set(owner, "get_ContainingAssembly", hasAssembly ? Make<AssemblySymbol>(SymbolKind.Assembly, "Metadata") : null);
        var expression = new BoundLiteralExpression(BoundLiteralExpressionKind.TrueLiteral, true, Named("Runtime", "Boolean"));
        Assert.Same(expression, BoundTreeOptimizer.Optimize(owner, expression));
    }

    [Fact]
    public void DisabledContextDoesNotReadOperatorTypesOrRewriteChildren()
    {
        var assembly = Make<OptimizationAssembly>(SymbolKind.Assembly, "Debug");
        Set(assembly, "get_IsReleaseOptimizationEnabled", false);
        // OperatorTypes is deliberately not configured: reading it fails.
        var owner = Owner(assembly);
        var type = Named("Runtime", "Boolean");
        var thenStatement = new BoundBlockStatement([]);
        var statement = new BoundIfStatement(new BoundLiteralExpression(BoundLiteralExpressionKind.FalseLiteral, false, type), thenStatement);
        Assert.Same(statement, BoundTreeOptimizer.Optimize(owner, statement));
    }

    [Fact]
    public void BooleanRewritingRunsBeforeControlFlowSelection()
    {
        var types = new Types();
        var owner = ReleaseOwner(types);
        var type = types.GetSpecialType(SpecialType.System_Boolean);
        Assert.True(BoundUnaryOperator.TryLookup(types, SyntaxKind.ExclamationToken, type, out var logicalNot));
        var condition = new BoundUnaryExpression(logicalNot,
            new BoundLiteralExpression(BoundLiteralExpressionKind.TrueLiteral, true, type));
        var thenStatement = new BoundExpressionStatement(new BoundDefaultValueExpression(type));
        var elseStatement = new BoundExpressionStatement(new BoundDefaultValueExpression(type));
        BoundStatement statement = new BoundIfStatement(condition, thenStatement, elseStatement);
        Assert.Same(elseStatement, BoundTreeOptimizer.Optimize(owner, statement));
    }

    [Fact]
    public void PipelineIncludesPatternSimplification()
    {
        var types = new Types();
        var type = types.GetSpecialType(SpecialType.System_Boolean);
        var pattern = new BoundDiscardPattern(type);
        var expression = new BoundIsPatternExpression(new BoundDefaultValueExpression(type),
            new BoundNotPattern(new BoundNotPattern(pattern)), type);
        var result = BoundTreeOptimizer.Optimize(ReleaseOwner(types), expression);
        Assert.Same(pattern, result.Pattern);
    }

    [Fact]
    public void PipelineDoesNotActivateUnreachableOrBranchRemoval()
    {
        var owner = ReleaseOwner(new Types());
        var target = Make<LabelSymbol>(SymbolKind.Label, "target");
        var terminal = new BoundReturnStatement(null);
        var jump = new BoundGotoStatement(target);
        var label = new BoundLabeledStatement(target, new BoundBlockStatement([]));
        var block = new BoundBlockStatement([terminal, jump, label], introduceILScope: false);
        var result = BoundTreeOptimizer.Optimize(owner, block);
        var statements = result.Statements.ToArray();
        Assert.Equal(3, statements.Length);
        Assert.Same(terminal, statements[0]);
        Assert.Same(jump, statements[1]);
        Assert.Same(target, Assert.IsType<BoundLabeledStatement>(statements[2]).Label);
        Assert.False(result.IntroduceILScope);
    }

    private static Symbol Owner(AssemblySymbol assembly)
    {
        var owner = Make<MethodSymbol>(SymbolKind.Method, "Owner");
        Set(owner, "get_ContainingAssembly", assembly);
        return owner;
    }

    private static Symbol ReleaseOwner(Types types)
    {
        var assembly = Make<OptimizationAssembly>(SymbolKind.Assembly, "Release");
        Set(assembly, "get_IsReleaseOptimizationEnabled", true);
        Set(assembly, "get_OperatorTypes", types);
        return Owner(assembly);
    }

    internal interface OptimizationAssembly : AssemblySymbol, BoundTreeOptimizationProvider { }

    private sealed class Types : OperatorTypeProvider
    {
        private readonly Dictionary<SpecialType, NamedTypeSymbol> _types = new();

        public TypeSymbol ErrorTypeSymbol { get; } = Make<TypeSymbol>(SymbolKind.Type, "error", TypeKind.Error);

        public NamedTypeSymbol GetSpecialType(SpecialType specialType)
        {
            if (!_types.TryGetValue(specialType, out var type))
            {
                type = Named("Runtime", specialType.ToString());
                Set(type, "get_SpecialType", specialType);
                Set(type, "get_EnumUnderlyingType", null);
                _types.Add(specialType, type);
            }
            return type;
        }
    }
}

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class PortablePreparedExpressionTests
{
    [Fact]
    public void LateLoweredArrayAssignmentValueCanReturnAtStatementBoundary()
    {
        var tree = SyntaxTree.ParseText("public static class Consumer { static func Read(values: int[]) -> int => 0 }");
        var compilation = Compilation.Create("PreparedArray", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var method = (IMethodSymbol)model.GetDeclaredSymbol(tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single())!;
        var integer = compilation.GetSpecialType(SpecialType.System_Int32);
        var unit = compilation.GetSpecialType(SpecialType.System_Unit);
        BoundExpression Constant(int value) => new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, value, integer);
        var block = new BoundBlockExpression([
            new BoundReturnStatement(Constant(42)),
            new BoundExpressionStatement(Constant(2))
        ], unit);
        var access = new BoundArrayAccessExpression(new BoundParameterAccess(method.Parameters[0]), [Constant(0)], integer);
        var body = new BoundBlockStatement([
            new BoundExpressionStatement(new BoundArrayAssignmentExpression(access, block, unit)),
            new BoundReturnStatement(Constant(0))
        ]);
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsArrays: true);
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, capabilities));
        plan = plan! with { PreparedBody = body };
        Assert.True(plan.TryLowerBody(compilation, _ => false, out _, out var failure, capabilities), failure?.Detail);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void LateLoweredValueBlocksCanReturnAtCallAndDiscardBoundaries(bool discard, bool terminalScope)
    {
        var tree = SyntaxTree.ParseText("""
            public static class Consumer {
                static func Pair(first: int, second: int) -> int => first + second
                static func Read() -> int => 0
            }
            """);
        var compilation = Compilation.Create("PreparedExpressions", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var methods = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().ToArray();
        var method = (IMethodSymbol)model.GetDeclaredSymbol(methods.Single(m => m.Identifier.ValueText == "Read"))!;
        var pair = (IMethodSymbol)model.GetDeclaredSymbol(methods.Single(m => m.Identifier.ValueText == "Pair"))!;
        var integer = compilation.GetSpecialType(SpecialType.System_Int32);
        var unit = compilation.GetSpecialType(SpecialType.System_Unit);
        var root = compilation.GetSpecialType(SpecialType.System_Object);
        BoundExpression Constant(int value) => new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, value, integer);
        // Runtime-await cancellation and cleanup return lowering introduce these
        // blocks after the ordinary source expression spill pass has already run.
        var block = new BoundBlockExpression([
            new BoundReturnStatement(Constant(42)),
            new BoundExpressionStatement(Constant(2))
        ], unit);
        if (terminalScope)
            block = new BoundBlockExpression([new BoundBlockStatement([new BoundReturnStatement(Constant(42))])], unit);
        BoundStatement statement = discard
            ? new BoundExpressionStatement(new BoundConversionExpression(new BoundRequiredResultExpression(block),
                root, compilation.ClassifyConversion(integer, root)))
            : new BoundReturnStatement(new BoundInvocationExpression(pair, [Constant(1), block]));
        var body = new BoundBlockStatement([statement, new BoundReturnStatement(Constant(0))]);
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsRootClassLocals: true, allowsRootClassSignatures: true, allowsExternalReferenceSignatures: true);
        Assert.True(SourceCallablePlan.TryCreate(method, out var plan, capabilities));
        plan = plan! with { PreparedBody = body };
        Assert.True(plan.TryLowerBody(compilation, _ => false, out _, out var failure, capabilities), failure?.Detail);
    }
}

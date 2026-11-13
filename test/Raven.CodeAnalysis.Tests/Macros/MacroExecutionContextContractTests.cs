using System;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests.Symbols;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class MacroExecutionContextContractTests
{
    [Fact]
    public void SnapshotPreservesContextAndArgumentOrdinals()
    {
        var execution = Create("Call(1, named: 2, 3)");
        Assert.False(execution.TypeArguments.IsDefault);
        Assert.Empty(execution.TypeArguments);
        Assert.Equal(new[] { 0, 1, 2 }, execution.Arguments.Select(a => a.Ordinal));
        Assert.Same(execution.Context, execution.GetContext<TestContext>());
        Assert.NotNull(execution.Diagnostics);
        Assert.Throws<InvalidOperationException>(() => execution.GetContext<AttachedMacroContext>());
    }

    [Fact]
    public void NamedArgumentsTakePrecedenceAndPositionalLookupSkipsNames()
    {
        var execution = Create("Call(1, named: 2, 3)");
        Assert.Equal(2, execution.GetArgument<int>(0, "named"));
        Assert.Equal(3, execution.GetArgument<int>(1, "other"));
        Assert.Equal(1, execution.GetArgument<int>(0, "Named"));
        Assert.Equal(9, execution.GetArgumentOrDefault(5, "absent", 9));
        Assert.Contains("requires argument 'absent'", Assert.Throws<InvalidOperationException>(() => execution.GetArgument<int>(5, "absent")).Message);
        Assert.Contains("cannot be converted", Assert.Throws<InvalidOperationException>(() => execution.GetArgument<Guid>(0, "other")).Message);
    }

    [Fact]
    public void GenericConversionsPreserveNullAndWidening()
    {
        var execution = Create("Call(null, 42)");
        Assert.Null(execution.GetArgument<string?>(0, "nullable"));
        Assert.Null(execution.GetArgument<int?>(0, "nullable"));
        Assert.Equal(42L, execution.GetArgument<long>(1, "number"));
        Assert.Throws<InvalidOperationException>(() => execution.GetArgument<int>(0, "required"));
    }

    [Fact]
    public void DeclarationAndAttachedTargetCapturesKeepOriginalNodes()
    {
        var tree = SyntaxTree.ParseText("[Example] class Model {}");
        var attribute = Assert.Single(tree.GetRoot().DescendantNodes().OfType<AttributeSyntax>());
        var target = Assert.Single(tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>());
        var context = new AttachedMacroContext(new Services(), attribute, target, target);
        var execution = new MacroExecutionContext(new Executor(), context, default, ImmutableArray<MacroArgument>.Empty, new DiagnosticBag());
        Assert.Same(target, execution.GetAttachedTarget<ClassDeclarationSyntax>());
        Assert.Same(attribute, execution.GetDeclarationInput<AttributeSyntax>());
        Assert.Throws<InvalidOperationException>(() => execution.GetAttachedTarget<MethodDeclarationSyntax>());
        Assert.Throws<InvalidOperationException>(() => execution.GetDeclarationInput<ClassDeclarationSyntax>());
        Assert.Throws<InvalidOperationException>(() => Create("Call()").GetAttachedTarget<ClassDeclarationSyntax>());
    }

    [Fact]
    public void ExecutorDefaultMembersReadConcreteTypeMetadata()
    {
        IMacroExecutor executor = new Executor();
        Assert.Equal(new[] { "T" }, executor.TypeParameters);
        var parameter = Assert.Single(executor.Parameters);
        Assert.Equal("value", parameter.Name);
        Assert.Equal(typeof(int), parameter.RuntimeType);
        Assert.Equal(0, parameter.InvocationArgumentOrdinal);
        Assert.False(executor.HasTokenBody);
        Assert.Equal(MacroTarget.None, executor.Targets);
        Assert.NotNull(executor.Expand(Create("Call()")).FreestandingResult);
    }

    private static MacroExecutionContext Create(string source)
    {
        var invocation = Assert.IsType<InvocationExpressionSyntax>(SyntaxFactory.ParseExpression(source));
        var arguments = invocation.ArgumentList.Arguments.Select(a => new MacroArgument(a, new Services())).ToImmutableArray();
        return new MacroExecutionContext(new Executor(), new TestContext(invocation), default, arguments, new DiagnosticBag());
    }

    [MacroExecutorTypeParameter("T", 0)]
    [MacroExecutorParameter("value", typeof(int), "int", MacroParameterSource.Value, 0, 0, true, "")]
    private sealed class Executor : IMacroExecutor
    {
        public string Name => "Execute";
        public MacroApplicationKind ApplicationKind => MacroApplicationKind.Freestanding;
        public MacroExecutionResult Expand(MacroExecutionContext context) => MacroExecutionResult.Freestanding(null);
    }

    private sealed class TestContext(SyntaxNode syntax) : MacroContext(syntax)
    {
        public override MacroExpansionDiagnostic CreateDiagnostic(string message, DiagnosticSeverity severity = DiagnosticSeverity.Error,
            SyntaxNode? syntax = null, string? code = null) => new(severity, message, syntax?.GetLocation(), code);
    }

    private sealed class Services : MacroArgumentServices
    {
        private readonly TypeSymbol _type = MetadataSymbolFixture.Named("Types", "Example.Value");
        public TypeSymbol GetSpecialType(SpecialType specialType) => _type;
        public TypeSymbol? GetArgumentType(Raven.CodeAnalysis.Syntax.ExpressionSyntax expression) => _type;
        public TypeSymbol? ResolveConstraint(Type runtimeType) => _type;
        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination) => new(isImplicit: true);
    }
}

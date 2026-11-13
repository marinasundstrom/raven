using System;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests.Symbols;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class MethodMacroAdapterContractTests
{
    [Fact]
    public void InvocationConvertsNamedArgumentsAndSuppliesDefaults()
    {
        var macro = new SumMacro();
        var adapter = Adapter(macro);
        Assert.Equal("12", Run(adapter, "Call(value: 5)").FreestandingResult!.Node!.ToString());
        Assert.Equal("9", Run(adapter, "Call(5, count: 4)").FreestandingResult!.Node!.ToString());
        Assert.True(adapter.AcceptsArguments);
        Assert.Empty(adapter.TypeParameters);
        Assert.Equal("sum", adapter.Name);
        Assert.Equal("alternate", adapter.Alias);
        Assert.Equal("Example", adapter.Namespace);
        Assert.Throws<InvalidOperationException>(() => Run(adapter, "Call()"));
        Assert.Throws<InvalidOperationException>(() => Run(adapter, "Call(\"bad\")"));
    }

    [Fact]
    public void FreestandingNormalizationPreservesResultObjects()
    {
        var macro = new ReturnMacro();
        var adapter = Adapter(macro);
        Assert.Same(FreestandingMacroExpansionResult.Empty, Run(adapter).FreestandingResult);
        var syntax = SyntaxFactory.ParseExpression("42");
        macro.Result = syntax;
        Assert.Same(syntax, Run(adapter).FreestandingResult!.Node);
        var expansion = FreestandingMacroExpansionResult.FromNode(syntax);
        macro.Result = expansion;
        Assert.Same(expansion, Run(adapter).FreestandingResult);
        var erased = MacroExecutionResult.Attached(null);
        macro.Result = erased;
        Assert.Same(erased, Run(adapter));
        macro.Result = 42;
        Assert.Contains("unsupported result type", Assert.Throws<InvalidOperationException>(() => Run(adapter)).Message);
    }

    [Fact]
    public void AttachedNormalizationAndContextInjectionPreserveIdentity()
    {
        var macro = new AttachedReturnMacro();
        var adapter = Adapter(macro);
        var context = AttachedContext();
        Assert.Same(MacroExpansionResult.Empty, Run(adapter, context: context).AttachedResult);
        Assert.Same(context, macro.Seen);
        macro.Result = context.TargetDeclaration;
        Assert.Same(context.TargetDeclaration, Run(adapter, context: context).AttachedResult!.ReplacementDeclaration);
        var expansion = MacroExpansionResult.FromReplacement(context.TargetDeclaration);
        macro.Result = expansion;
        Assert.Same(expansion, Run(adapter, context: context).AttachedResult);
        macro.Result = 1;
        Assert.Throws<InvalidOperationException>(() => Run(adapter, context: context));
        Assert.Throws<InvalidOperationException>(() => Run(adapter));
    }

    [Fact]
    public void AttachedTargetAndDeclarationInputsUseAuthoredNodes()
    {
        var context = AttachedContext();
        var targetAdapter = Adapter(new TargetMacro());
        Assert.Same(context.TargetDeclaration, Run(targetAdapter, context: context).AttachedResult!.ReplacementDeclaration);
        Assert.Throws<InvalidOperationException>(() => Run(targetAdapter));
        var invocation = ParseInvocation("Make! Widget() {}");
        var declarationContext = new FreestandingMacroContext(new Services(), invocation);
        var declarationAdapter = Adapter(new DeclarationMacro());
        Assert.Same(invocation.Syntax, Run(declarationAdapter, context: declarationContext).FreestandingResult!.Node);
        Assert.Throws<InvalidOperationException>(() => Run(declarationAdapter));
    }

    [Fact]
    public void TokenBodyInjectionCreatesUsableStream()
    {
        var macro = new TokenMacro();
        var adapter = Adapter(macro);
        var invocation = ParseInvocation("let value = Make! { hello }");
        var context = new TokenTreeMacroContext(new Services(), invocation, null, ImmutableArray<MacroKeyword>.Empty, default);
        Run(adapter, context: context);
        Assert.NotNull(macro.Stream);
        Assert.False(macro.Stream!.IsEndOfFile);
        Assert.True(adapter.HasTokenBody);
        Assert.Throws<InvalidOperationException>(() => Run(adapter));
    }

    [Fact]
    public void VoidStaticAndThrowingMethodsRetainCallBehavior()
    {
        var macro = new VoidMacro();
        Assert.Same(FreestandingMacroExpansionResult.Empty, Run(Adapter(macro)).FreestandingResult);
        Assert.Equal(1, macro.Calls);
        StaticMacro.Calls = 0;
        Run(Adapter(new StaticMacro()));
        Assert.Equal(1, StaticMacro.Calls);
        Assert.Throws<FormatException>(() => Run(Adapter(new ThrowingMacro())));
    }

    private static MethodMacroExecutorAdapter Adapter(IMacroDefinition definition)
        => new(definition, definition.GetType().GetMethod("Expand")!);

    private static MacroExecutionResult Run(MethodMacroExecutorAdapter adapter, string source = "Call()", MacroContext? context = null)
    {
        var syntax = Assert.IsType<InvocationExpressionSyntax>(SyntaxFactory.ParseExpression(source));
        var arguments = syntax.ArgumentList.Arguments.Select(a => new MacroArgument(a, new Services())).ToImmutableArray();
        return adapter.Expand(new MacroExecutionContext(adapter, context ?? new TestContext(syntax), default, arguments, new DiagnosticBag()));
    }

    private static AttachedMacroContext AttachedContext()
    {
        var tree = SyntaxTree.ParseText("[Example] class Model {}");
        var attribute = Assert.Single(tree.GetRoot().DescendantNodes().OfType<AttributeSyntax>());
        var target = Assert.Single(tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>());
        return new AttachedMacroContext(new Services(), attribute, target, target);
    }

    private static FreestandingMacroInvocation ParseInvocation(string source)
    {
        var syntax = SyntaxTree.ParseText(source).GetRoot().DescendantNodes().First(n => n is FreestandingMacroDeclarationSyntax or FreestandingMacroExpressionSyntax);
        Assert.True(FreestandingMacroInvocation.TryCreate(syntax, out var invocation));
        return invocation;
    }

    public sealed class SumMacro : IMacroDefinition
    {
        public string Name => "sum";
        public string Namespace => "Example";
        public string Alias => "alternate";
        public SyntaxNode Expand(int value, int count = 7) => SyntaxFactory.ParseExpression((value + count).ToString());
    }
    public sealed class ReturnMacro : IMacroDefinition { public object? Result; public object? Expand() => Result; }
    public sealed class AttachedReturnMacro : IMacroDefinition
    {
        public object? Result;
        public AttachedMacroContext? Seen;
        public object? Expand(AttachedMacroContext context) { Seen = context; return Result; }
    }
    public sealed class TargetMacro : IMacroDefinition
    {
        public MacroExpansionResult Expand(ClassDeclarationSyntax target) => MacroExpansionResult.FromReplacement(target);
    }
    public sealed class DeclarationMacro : IMacroDefinition { public SyntaxNode Expand(FreestandingMacroDeclarationSyntax declaration) => declaration; }
    public sealed class TokenMacro : IMacroDefinition { public IMacroTokenStream? Stream; public void Expand(IMacroTokenStream stream) => Stream = stream; }
    public sealed class VoidMacro : IMacroDefinition { public int Calls; public void Expand() => Calls++; }
    public sealed class StaticMacro : IMacroDefinition { public static int Calls; public static void Expand() => Calls++; }
    public sealed class ThrowingMacro : IMacroDefinition { public void Expand() => throw new FormatException("macro failure"); }

    private sealed class TestContext(SyntaxNode syntax) : MacroContext(syntax)
    {
        public override MacroExpansionDiagnostic CreateDiagnostic(string message, DiagnosticSeverity severity = DiagnosticSeverity.Error,
            SyntaxNode? syntax = null, string? code = null) => new(severity, message, syntax?.GetLocation(), code);
    }

    private sealed class Services : MacroArgumentServices, MacroFragmentServices
    {
        private readonly TypeSymbol _type = MetadataSymbolFixture.Named("Types", "Example.Value");
        public MacroArgumentServices ArgumentServices => this;
        public TypeSymbol ErrorType => _type;
        public TypeSymbol CharacterType => _type;
        public NamedTypeSymbol? GetSequenceDefinition(bool asynchronous) => null;
        public TypeSymbol GetSpecialType(SpecialType specialType) => _type;
        public TypeSymbol? GetArgumentType(Raven.CodeAnalysis.Syntax.ExpressionSyntax expression) => _type;
        public TypeSymbol? ResolveConstraint(Type runtimeType) => _type;
        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination) => new(isImplicit: true);
        public TypeSymbol? GetExpressionType(SyntaxNode invocation, Raven.CodeAnalysis.Syntax.ExpressionSyntax expression) => throw new InvalidOperationException();
        public TypeInfo GetTypeInfo(SyntaxNode invocation, Raven.CodeAnalysis.Syntax.ExpressionSyntax expression) => throw new InvalidOperationException();
        public SymbolInfo GetSymbolInfo(SyntaxNode invocation, Raven.CodeAnalysis.Syntax.ExpressionSyntax expression) => throw new InvalidOperationException();
    }
}

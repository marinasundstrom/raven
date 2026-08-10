using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Macros;

public sealed class MacroSignatureHelpTests
{
    [Fact]
    public void GetMacroSignatureHelp_TypedTokenTreeMacro_ReturnsNormalizedParametersAndActiveName()
    {
        const string code = """
import Raven.CodeAnalysis.Tests.Macros.*

class Host {
    func Test() {
        let value = typedQuery!(Dialect: "sql", Optimize: true) { from users }
    }
}
""";
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(new TypedQueryMacro()));

        var position = code.IndexOf("true", StringComparison.Ordinal) + 2;
        var signature = compilation.GetMacroSignatureHelp(syntaxTree, position);

        Assert.NotNull(signature);
        Assert.Equal("typedQuery", signature.Name);
        Assert.Equal(MacroKind.FreestandingExpression, signature.Kind);
        Assert.True(signature.HasTokenTreeBody);
        Assert.Collection(
            signature.Parameters,
            parameter =>
            {
                Assert.Equal("count", parameter.Name);
                Assert.Equal("int", parameter.TypeDisplayName);
                Assert.Equal(MacroParameterKind.Positional, parameter.Kind);
                Assert.False(parameter.IsRequired);
                Assert.Equal(1, parameter.DefaultValue);
            },
            parameter =>
            {
                Assert.Equal("Dialect", parameter.Name);
                Assert.Equal("string", parameter.TypeDisplayName);
                Assert.Equal(MacroParameterKind.Named, parameter.Kind);
            },
            parameter =>
            {
                Assert.Equal("Optimize", parameter.Name);
                Assert.Equal("bool", parameter.TypeDisplayName);
                Assert.Equal(MacroParameterKind.Named, parameter.Kind);
            });
        Assert.Equal(2, signature.ActiveParameter);
    }

    [Fact]
    public void GetMacroSignatureHelp_TypedAttachedMacro_ReturnsAttachedSignature()
    {
        const string code = """
import Raven.CodeAnalysis.Tests.Macros.*

class Host {
    #[typedObservable(Notify: true)]
    var Value: int = 0
}
""";
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(new TypedObservableMacro()));

        var position = code.IndexOf("true", StringComparison.Ordinal);
        var signature = compilation.GetSemanticModel(syntaxTree).GetMacroSignatureHelp(position);

        Assert.NotNull(signature);
        Assert.Equal("typedObservable", signature.Name);
        Assert.Equal(MacroKind.AttachedDeclaration, signature.Kind);
        Assert.False(signature.HasTokenTreeBody);
        Assert.Equal("Notify", Assert.Single(signature.Parameters).Name);
        Assert.Equal(0, signature.ActiveParameter);
    }

    [Fact]
    public void GetMacroSignatureHelp_ExpressionProjection_UsesLanguageRoleName()
    {
        const string code = """
import Raven.CodeAnalysis.Tests.Macros.*

class Host {
    func Test() {
        let value = project!(1 + 2)
    }
}
""";
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(new ExpressionProjectionMacro()));

        var signature = compilation.GetMacroSignatureHelp(
            syntaxTree,
            code.IndexOf("1 + 2", StringComparison.Ordinal) + 2);
        var parameter = Assert.Single(signature!.Parameters);

        Assert.Equal("expression", parameter.Name);
        Assert.Equal("ExpressionSyntax", parameter.TypeDisplayName);
        Assert.Equal(MacroParameterRole.SyntaxInput, parameter.Role);
    }

    private sealed class TypedQueryParameters
    {
        public TypedQueryParameters(int count = 1)
        {
            Count = count;
        }

        public int Count { get; }

        public string Dialect { get; set; } = string.Empty;

        public bool Optimize { get; set; }
    }

    private sealed class TypedQueryMacro : ITokenTreeExpressionMacro<TypedQueryParameters>
    {
        public string Name => "typedQuery";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext<TypedQueryParameters> context)
            => FreestandingMacroExpansionResult.Empty;
    }

    private sealed class TypedObservableParameters
    {
        public bool Notify { get; set; }
    }

    private sealed class TypedObservableMacro : IAttachedDeclarationMacro<TypedObservableParameters>
    {
        public string Name => "typedObservable";

        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext<TypedObservableParameters> context)
            => MacroExpansionResult.Empty;
    }

    private sealed class ExpressionProjectionParameters
    {
        public ExpressionProjectionParameters(ExpressionSyntax expression)
        {
            Expression = expression;
        }

        public ExpressionSyntax Expression { get; }
    }

    private sealed class ExpressionProjectionMacro :
        IFreestandingExpressionMacro<ExpressionProjectionParameters>
    {
        public string Name => "project";

        public FreestandingMacroExpansionResult Expand(
            FreestandingMacroContext<ExpressionProjectionParameters> context)
            => FreestandingMacroExpansionResult.Empty;
    }
}

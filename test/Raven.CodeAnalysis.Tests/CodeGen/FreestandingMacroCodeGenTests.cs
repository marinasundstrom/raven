using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public sealed class FreestandingMacroCodeGenTests
{
    [Fact]
    public void FreestandingMacro_ExpandedExpression_IsEmitted()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            class Harness {
                public static func Run() -> int {
                    return #add(20, Right: 22)
                }
            }
            """);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(typeof(AddMacroPlugin)));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;
        var method = assembly.GetType("Harness", true)!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(42, method!.Invoke(null, null));
    }

    public sealed class AddMacroPlugin : IRavenMacroPlugin
    {
        public string Name => nameof(AddMacroPlugin);

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new AddMacro()];
    }

    public sealed class AddMacro : IFreestandingExpressionMacro<AddMacroParameters>
    {
        public string Name => "add";
        public MacroKind Kind => MacroKind.FreestandingExpression;
        public MacroTarget Targets => MacroTarget.None;

        public FreestandingMacroExpansionResult Expand(FreestandingMacroContext<AddMacroParameters> context)
            => new()
            {
                Expression = ParseExpression($"{context.Parameters.Left} + {context.Parameters.Right}")
            };
    }

    public sealed class AddMacroParameters(int left)
    {
        public int Left { get; } = left;

        public int Right { get; set; }
    }

    private static ExpressionSyntax ParseExpression(string expressionText)
    {
        var tree = SyntaxTree.ParseText($$"""
            func Main() -> int => {{expressionText}}
            """);

        return tree.GetRoot()
            .DescendantNodes()
            .OfType<ArrowExpressionClauseSyntax>()
            .Single()
            .Expression;
    }
}

using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Macros;

public sealed class MacroFragmentLocalTests
{
    [Fact]
    public void ExplicitFragmentLocal_ParticipatesInMemberCompletion()
    {
        const string code = "let row = 1\nlet result = scoped!{ row. }";
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "MacroFragmentLocal",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(new ScopedMacro()));
        var invocation = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var position = code.IndexOf("row.", StringComparison.Ordinal) + "row.".Length;

        var region = compilation.GetMacroInputSnapshot(invocation).FindFragmentRegion(position);
        var local = Assert.Single(region!.Locals);
        var completions = compilation.GetSemanticModel(syntaxTree)
            .GetCompletions(position)
            .ToArray();

        Assert.Equal("row", local.Name);
        Assert.Equal(SpecialType.System_String, local.Type.SpecialType);
        Assert.Contains(completions, static item => item.DisplayText == "Length");
    }

    private sealed class ScopedMacro : ITokenTreeExpressionMacro, IMacroFragmentProvider
    {
        public string Namespace => string.Empty;

        public string Name => "scoped";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
        {
            var local = context.CreateFragmentLocal(
                "row",
                context.Compilation.GetSpecialType(SpecialType.System_String));
            return
            [
                context.CreateFragmentRegion(
                    MacroFragmentKind.Expression,
                    new TextSpan(0, context.BodySpan.Length),
                    [local]),
            ];
        }
    }
}

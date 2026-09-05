using System.Collections.Immutable;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Macros;

public sealed class MemberMacroToolingTests
{
    private const string Code = """
        class Host {
            members!(count: 1) {
                let value = 42
                System.Console.WriteLine(value)
            }
        }
        """;

    [Theory]
    [InlineData("signature")]
    [InlineData("tokens")]
    [InlineData("fragments")]
    public void TypeMemberMacro_ExposesToolingInformation(string query)
    {
        var tree = SyntaxTree.ParseText(Code);
        var compilation = Compilation.Create("MemberTooling")
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(new MemberMacro()));
        var model = compilation.GetSemanticModel(tree);
        var member = tree.GetRoot().DescendantNodes()
            .OfType<FreestandingMacroMemberDeclarationSyntax>().Single();

        switch (query)
        {
            case "signature":
                var signature = model.GetMacroSignatureHelp(Code.IndexOf("1)"));
                Assert.NotNull(signature);
                Assert.Equal("count", Assert.Single(signature.Parameters).Name);
                break;
            case "tokens":
                Assert.NotEmpty(model.GetMacroInputSnapshot(member).Tokens);
                break;
            case "fragments":
                var info = model.GetMacroFragmentSemanticInfo(member, Code.LastIndexOf("value"));
                var local = Assert.IsAssignableFrom<ILocalSymbol>(info?.SymbolInfo.Symbol);
                Assert.Equal(SpecialType.System_Int32, local.Type.SpecialType);
                break;
        }
    }

    [Fact]
    public void ArgumentListMemberMacro_TokenTreeQueriesReturnEmpty()
    {
        var tree = SyntaxTree.ParseText("class Host { argumentMembers!(count: 1) }");
        var compilation = Compilation.Create("MemberArguments")
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default)
            .AddMacroReferences(new MacroReference(new ArgumentMemberMacro()));
        var model = compilation.GetSemanticModel(tree);
        var member = tree.GetRoot().DescendantNodes()
            .OfType<FreestandingMacroMemberDeclarationSyntax>().Single();

        Assert.NotNull(model.GetMacroSignatureHelp(member.ArgumentList!.Arguments[0].Span.Start));
        Assert.Empty(compilation.GetMacroTokens(member));
        Assert.Empty(compilation.GetMacroFragmentRegions(member));
        Assert.Empty(compilation.GetMacroInputSnapshot(member).Tokens);
        Assert.Null(compilation.GetMacroFragmentSemanticInfo(member, member.Name.Span.Start));
        Assert.Null(model.GetMacroTokenInfo(member, member.Name.Span.Start));
        Assert.Empty(model.GetMacroFragmentInferredTypeAnnotations(member));
        Assert.Empty(model.GetMacroFragmentClassifications(member).Tokens);
        Assert.NotNull(model.GetMacroExpansion(member));
    }

    private sealed class ArgumentMemberMacro : IMacroDefinition
    {
        public string Name => "argumentMembers";
        public string Namespace => string.Empty;
        public MacroInvocationTargets InvocationTargets => MacroInvocationTargets.TypeMember;

        public MemberDeclarationSyntax Expand(int count, FreestandingMacroContext context)
            => SyntaxFactory.ParseMemberDeclaration("class Generated {}")!;
    }

    private sealed class MemberMacro : IMacroDefinition, IMacroFragmentProvider
    {
        public string Name => "members";
        public string Namespace => string.Empty;
        public MacroInvocationTargets InvocationTargets => MacroInvocationTargets.TypeMember;

        public MemberDeclarationSyntax Expand(int count, TokenTreeMacroContext context)
            => SyntaxFactory.ParseMemberDeclaration("class Generated {}")!;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
            => [context.CreateFragmentRegion(MacroFragmentKind.Block, new TextSpan(0, context.BodySpan.Length))];
    }
}

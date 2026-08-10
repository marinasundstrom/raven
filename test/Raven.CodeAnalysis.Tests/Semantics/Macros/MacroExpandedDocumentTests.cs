using System;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Semantics.Macros;

public sealed class MacroExpandedDocumentTests : CompilationTestBase
{
    private new (Compilation Compilation, SyntaxTree Tree) CreateCompilation(
        string source,
        CompilationOptions? options = null,
        MetadataReference[]? references = null,
        string assemblyName = "test")
    {
        var tree = SyntaxTree.ParseText(source);
        var imports = SyntaxTree.ParseText("""
            global {
                import Raven.CodeAnalysis.Tests.Semantics.Macros.*
                import Raven.CodeAnalysis.Tests.*
            }
            """);
        return (
            base.CreateCompilation([imports, tree], options, references, assemblyName),
            tree);
    }

    [Fact]
    public void GetExpandedRoot_RewritesAttachedAndInvocableMacros()
    {
        var (compilation, tree) = CreateCompilation("""
            class Harness {
                #[Observable]
                var Title: string

                func GetAnswer() -> int {
                    return add!(20, Right: 22)
                }
            }
            """);

        compilation = compilation.AddMacroReferences(
            new MacroReference(new MacroCodeGenTests.IntroducedMethodMacro()),
            new MacroReference(new MacroCodeGenTests.ObservablePropertyMacro()),
            new MacroReference(typeof(InvocableMacroCodeGenTests.AddMacro)));

        var model = compilation.GetSemanticModel(tree);
        var expandedRoot = model.GetExpandedRoot();
        var expandedText = expandedRoot.ToFullString();

        Assert.Contains("private var _Title: string", expandedText, StringComparison.Ordinal);
        Assert.Contains("return 20 + 22", expandedText, StringComparison.Ordinal);
        Assert.Contains("\n    private var _Title: string", expandedText, StringComparison.Ordinal);
        Assert.Contains("\n        get => _Title", expandedText, StringComparison.Ordinal);
        Assert.DoesNotContain("add!(", expandedText, StringComparison.Ordinal);
        Assert.DoesNotContain("#[Observable]", expandedText, StringComparison.Ordinal);

        var expandedProperty = expandedRoot.DescendantNodes()
            .OfType<PropertyDeclarationSyntax>()
            .Single(static property => property.Identifier.ValueText == "Title");

        Assert.NotNull(expandedProperty.AccessorList);
    }

    [Fact]
    public void GetExpandedRoot_PreservesBlankLinesAndFormatsInvocableLambdaBodies()
    {
        var (compilation, tree) = CreateCompilation("""
            func Main() {
                use subscription = wrap!((value) => {
                    WriteLine(value)
                })
            }

            class CounterViewModel {
                #[Observable]
                var Count: int = 0
            }
            """);

        compilation = compilation.AddMacroReferences(
            new MacroReference(new ObservableMacro()),
            new MacroReference(new WrapMacro()));

        var model = compilation.GetSemanticModel(tree);
        var expandedText = model.GetExpandedRoot().ToFullString();

        Assert.Contains(
            "Observe((value) => {\n        WriteLine(value)\n    })",
            expandedText,
            StringComparison.Ordinal);

        Assert.Contains("private var _Count: int", expandedText, StringComparison.Ordinal);
        Assert.Contains("private val _CountChanged", expandedText, StringComparison.Ordinal);
        Assert.Contains("val CountChanged", expandedText, StringComparison.Ordinal);
        Assert.Contains("var Count: int {", expandedText, StringComparison.Ordinal);
    }

    [Fact]
    public void GetExpandedRoot_RewritesEveryInvocableMacroInSameMember()
    {
        var (compilation, tree) = CreateCompilation("""
            func Main() {
                let first = add!(1, Right: 2)
                let second = add!(3, Right: 4)
            }
            """);
        compilation = compilation.AddMacroReferences(
            new MacroReference(typeof(InvocableMacroCodeGenTests.AddMacro)));

        var expandedText = compilation.GetSemanticModel(tree)
            .GetExpandedRoot()
            .ToFullString();

        Assert.Contains("let first = 1 + 2", expandedText, StringComparison.Ordinal);
        Assert.Contains("let second = 3 + 4", expandedText, StringComparison.Ordinal);
        Assert.DoesNotContain("add!(", expandedText, StringComparison.Ordinal);
    }

    [Fact]
    public void GetExpandedRoot_PreservesLineBreakAfterInvocableMacro()
    {
        var (compilation, tree) = CreateCompilation("""
            func Main() {
                let answer = raven! {
                    6
                }
                WriteLine(answer)
            }
            """);
        compilation = compilation.AddMacroReferences(
            new MacroReference(typeof(InvocableMacroSemanticTests.RavenBodyMacro)));

        var expandedText = compilation.GetSemanticModel(tree)
            .GetExpandedRoot()
            .ToFullString();

        Assert.Contains("let answer = 6\n    WriteLine(answer)", expandedText, StringComparison.Ordinal);
        Assert.DoesNotContain("raven!", expandedText, StringComparison.Ordinal);
    }

    [Fact]
    public void GetExpandedRoot_ReplacesTypeMemberInvocationWithOrderedMembers()
    {
        var (compilation, tree) = CreateCompilation("""
            class Model {
                GenerateMembers! { Id, Name }
                func Existing() -> int => 3
            }
            """);
        compilation = compilation.AddMacroReferences(
            new MacroReference(new GenerateMembersMacro()));

        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocableMacroMemberDeclarationSyntax>()
            .Single();
        var expansion = model.GetMacroExpansion(invocation);
        var expandedText = model.GetExpandedRoot().ToFullString();

        Assert.NotNull(expansion);
        Assert.True(expansion.HasMemberExpansion);
        Assert.Equal(2, expansion.Members.Length);
        Assert.True(expandedText.IndexOf("GeneratedFirst", StringComparison.Ordinal) <
                    expandedText.IndexOf("GeneratedSecond", StringComparison.Ordinal));
        Assert.True(expandedText.IndexOf("GeneratedSecond", StringComparison.Ordinal) <
                    expandedText.IndexOf("Existing", StringComparison.Ordinal));
        Assert.DoesNotContain("GenerateMembers!", expandedText, StringComparison.Ordinal);
    }

    [Fact]
    public void GetExpandedRoot_ExplicitEmptyMemberListRemovesInvocation()
    {
        var (compilation, tree) = CreateCompilation("""
            class Model {
                RemoveMember! { ignored }
                func Existing() -> int => 3
            }
            """);
        compilation = compilation.AddMacroReferences(
            new MacroReference(new RemoveMemberMacro()));

        var expandedText = compilation.GetSemanticModel(tree)
            .GetExpandedRoot()
            .ToFullString();

        Assert.DoesNotContain("RemoveMember!", expandedText, StringComparison.Ordinal);
        Assert.Contains("Existing", expandedText, StringComparison.Ordinal);
    }

    [Fact]
    public void GetExpandedRoot_AcceptsSingleMemberNodeAtTypeMemberInvocation()
    {
        var (compilation, tree) = CreateCompilation("""
            class Model {
                GenerateSingleMember! { ignored }
            }
            """);
        compilation = compilation.AddMacroReferences(
            new MacroReference(new GenerateSingleMemberMacro()));

        var expandedText = compilation.GetSemanticModel(tree)
            .GetExpandedRoot()
            .ToFullString();

        Assert.Contains("GeneratedSingle", expandedText, StringComparison.Ordinal);
        Assert.DoesNotContain("GenerateSingleMember!", expandedText, StringComparison.Ordinal);
    }

    [Fact]
    public void GetExpandedRoot_ReportsWrongCategoryAndPreservesTypeMemberInvocation()
    {
        var (compilation, tree) = CreateCompilation("""
            class Model {
                GenerateExpression! { ignored }
                func Existing() -> int => 3
            }
            """);
        compilation = compilation.AddMacroReferences(
            new MacroReference(new GenerateExpressionMacro()));

        var model = compilation.GetSemanticModel(tree);
        var expandedText = model.GetExpandedRoot().ToFullString();
        var diagnostic = Assert.Single(
            model.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAVM022"));

        Assert.Contains("expression syntax where member syntax is required", diagnostic.GetMessage());
        Assert.Contains("GenerateExpression!", expandedText, StringComparison.Ordinal);
        Assert.Contains("Existing", expandedText, StringComparison.Ordinal);
    }

    [Fact]
    public void GetExpandedRoot_StacksAttachedDeclarationMacrosBySourceOrder()
    {
        var (compilation, tree) = CreateCompilation("""
            class Sample {
                #[First]
                #[Second]
                var Value: int
            }
            """);

        compilation = compilation.AddMacroReferences(
            new MacroReference(new FirstMacro()),
            new MacroReference(new SecondMacro()));

        var model = compilation.GetSemanticModel(tree);
        var expandedText = model.GetExpandedRoot().ToFullString();

        var firstMarkerIndex = AssertContainsAndGetIndex(expandedText, "func Before_Value() -> int");
        var secondMarkerIndex = AssertContainsAndGetIndex(expandedText, "func BeforeAgain_First_Value() -> int");
        var replacementIndex = AssertContainsAndGetIndex(expandedText, "var Second_First_Value: int");
        var firstPeerIndex = AssertContainsAndGetIndex(expandedText, "func After_Value() -> int");
        var secondPeerIndex = AssertContainsAndGetIndex(expandedText, "func AfterAgain_First_Value() -> int");

        Assert.True(firstMarkerIndex < secondMarkerIndex);
        Assert.True(secondMarkerIndex < replacementIndex);
        Assert.True(replacementIndex < firstPeerIndex);
        Assert.True(firstPeerIndex < secondPeerIndex);

        Assert.DoesNotContain("var Second_Value: int", expandedText, StringComparison.Ordinal);
    }

    [Fact]
    public void GetExpandedRoot_SeparatesIntroducedMemberFromReplacementDeclaration()
    {
        var (compilation, tree) = CreateCompilation("""
            #[ComponentBoundary]
            class TodoList {
            }
            """);

        compilation = compilation.AddMacroReferences(
            new MacroReference(new ComponentBoundaryMacro()));

        var expandedText = compilation.GetSemanticModel(tree)
            .GetExpandedRoot()
            .ToFullString();

        Assert.Matches("}\\r?\\n(?:\\r?\\n)*\\[#ComponentBoundary]", expandedText);
        Assert.DoesNotContain("}[#ComponentBoundary]", expandedText, StringComparison.Ordinal);
    }

    [Fact]
    public void GetExpandedRoot_ReusesCachedExpandedDocument()
    {
        var (compilation, tree) = CreateCompilation("""
            class Harness {
                #[Observable]
                var Title: string
            }
            """);

        compilation = compilation.AddMacroReferences(
            new MacroReference(new MacroCodeGenTests.IntroducedMethodMacro()),
            new MacroReference(new MacroCodeGenTests.ObservablePropertyMacro()));

        var model = compilation.GetSemanticModel(tree);

        var first = model.GetExpandedRoot();
        var second = model.GetExpandedRoot();

        Assert.Same(first, second);
    }

    [Fact]
    public void GetExpandedDeclaration_ReusesCachedExpandedSections()
    {
        var (compilation, tree) = CreateCompilation("""
            class Harness {
                #[Observable]
                var Title: string
            }
            """);

        compilation = compilation.AddMacroReferences(
            new MacroReference(new MacroCodeGenTests.IntroducedMethodMacro()),
            new MacroReference(new MacroCodeGenTests.ObservablePropertyMacro()));

        var model = compilation.GetSemanticModel(tree);
        var attribute = tree.GetRoot()
            .DescendantNodes()
            .OfType<AttributeSyntax>()
            .Single();

        var first = model.GetExpandedDeclaration(attribute);
        var second = model.GetExpandedDeclaration(attribute);

        Assert.Equal(first.Length, second.Length);
        for (var i = 0; i < first.Length; i++)
            Assert.Same(first[i], second[i]);
    }

    [Fact]
    public void MacroInstrumentation_TracksExpansionCountsWithoutDoubleCountingCachedExpansion()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => wrap!(21)
            """);
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var compilation = CreateCompilation(tree, options: options)
            .AddMacroReferences(
                new MacroReference(new ObservableMacro()),
                new MacroReference(new WrapMacro()));

        var model = compilation.GetSemanticModel(tree);
        var expression = tree.GetRoot().DescendantNodes().OfType<InvocableMacroExpressionSyntax>().Single();

        _ = model.GetMacroExpansion(expression);
        _ = model.GetMacroExpansion(expression);

        Assert.Equal(1, instrumentation.Macros.InvocableExpansionInvocations);
        Assert.Equal(0, instrumentation.Macros.AttachedExpansionInvocations);
    }

    private static int AssertContainsAndGetIndex(string text, string value)
    {
        var index = text.IndexOf(value, StringComparison.Ordinal);
        Assert.True(index >= 0, $"Expected to find '{value}' in expanded text.");
        return index;
    }

    private sealed class ObservableMacro : IAttachedDeclarationMacro
    {
        public string Name => "Observable";

        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var tree = SyntaxFactory.ParseSyntaxTree("""
                class __GeneratedContainer {
                    private var _Count: int = 0
                    private val _CountChanged: Subject<int> = Subject<int>()
                    val CountChanged: IObservable<int> => _CountChanged
                    var Count: int {
                        get => _Count
                        set {
                            if value != _Count {
                                let oldValue = _Count
                                _Count = value
                                _CountChanged.OnNext(value)
                            }
                        }
                    }
                }
                """);

            var container = Assert.IsType<ClassDeclarationSyntax>(tree.GetRoot().Members.Single());
            var backingField = Assert.IsType<PropertyDeclarationSyntax>(container.Members[0]);
            var signalStorage = Assert.IsType<PropertyDeclarationSyntax>(container.Members[1]);
            var signalProperty = Assert.IsType<PropertyDeclarationSyntax>(container.Members[2]);
            var replacementProperty = Assert.IsType<PropertyDeclarationSyntax>(container.Members[3]);

            return new MacroExpansionResult
            {
                ReplacementDeclaration = replacementProperty,
                IntroducedMembers = [backingField, signalStorage, signalProperty]
            };
        }
    }

    private sealed class WrapMacro : IInvocableMacro
    {
        public string Name => "wrap";

        public bool AcceptsArguments => true;

        public InvocableMacroExpansionResult Expand(InvocableMacroContext context)
        {
            var callback = context.Arguments.Single().Expression;

            return new InvocableMacroExpansionResult
            {
                Expression = SyntaxFactory.InvocationExpression(
                    SyntaxFactory.IdentifierName("Observe"),
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SeparatedList<ArgumentSyntax>(
                        [
                            new SyntaxNodeOrToken(SyntaxFactory.Argument(callback))
                        ])))
            };
        }
    }

    private sealed class GenerateMembersMacro : ITokenTreeMacro
    {
        public string Name => "GenerateMembers";

        public InvocableMacroExpansionResult Expand(TokenTreeMacroContext context)
            => InvocableMacroExpansionResult.FromMembers(ParseMembers("""
                class __GeneratedContainer {
                    func GeneratedFirst() -> int => 1
                    func GeneratedSecond() -> int => 2
                }
                """));
    }

    private sealed class RemoveMemberMacro : ITokenTreeMacro
    {
        public string Name => "RemoveMember";

        public InvocableMacroExpansionResult Expand(TokenTreeMacroContext context)
            => InvocableMacroExpansionResult.FromMembers(
                ImmutableArray<MemberDeclarationSyntax>.Empty);
    }

    private sealed class GenerateSingleMemberMacro : ITokenTreeMacro
    {
        public string Name => "GenerateSingleMember";

        public InvocableMacroExpansionResult Expand(TokenTreeMacroContext context)
            => InvocableMacroExpansionResult.FromNode(ParseMembers("""
                class __GeneratedContainer {
                    func GeneratedSingle() -> int => 1
                }
                """).Single());
    }

    private sealed class GenerateExpressionMacro : ITokenTreeMacro
    {
        public string Name => "GenerateExpression";

        public InvocableMacroExpansionResult Expand(TokenTreeMacroContext context)
            => InvocableMacroExpansionResult.FromExpression(
                SyntaxFactory.LiteralExpression(
                    SyntaxKind.NumericLiteralExpression,
                    SyntaxFactory.Literal(1)));
    }

    private sealed class FirstMacro : IAttachedDeclarationMacro
    {
        public string Name => "First";

        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var property = Assert.IsType<PropertyDeclarationSyntax>(context.TargetDeclaration);
            var identifier = property.Identifier.ValueText;
            var members = ParseMembers($$"""
                class __GeneratedContainer {
                    func Before_{{identifier}}() -> int { return 1 }
                    var First_{{identifier}}: int { get => 1 }
                    func After_{{identifier}}() -> int { return 10 }
                }
                """);

            return new MacroExpansionResult
            {
                IntroducedMembers = [members[0]],
                ReplacementDeclaration = members[1],
                PeerDeclarations = [members[2]]
            };
        }
    }

    private sealed class SecondMacro : IAttachedDeclarationMacro
    {
        public string Name => "Second";

        public MacroTarget Targets => MacroTarget.Property;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var property = Assert.IsType<PropertyDeclarationSyntax>(context.CurrentDeclaration);
            var identifier = property.Identifier.ValueText;
            var members = ParseMembers($$"""
                class __GeneratedContainer {
                    func BeforeAgain_{{identifier}}() -> int { return 2 }
                    var Second_{{identifier}}: int { get => 2 }
                    func AfterAgain_{{identifier}}() -> int { return 20 }
                }
                """);

            return new MacroExpansionResult
            {
                IntroducedMembers = [members[0]],
                ReplacementDeclaration = members[1],
                PeerDeclarations = [members[2]]
            };
        }
    }

    private sealed class ComponentBoundaryMacro : IAttachedDeclarationMacro
    {
        public string Name => "ComponentBoundary";

        public MacroTarget Targets => MacroTarget.Type;

        public MacroExpansionResult Expand(AttachedMacroContext context)
        {
            var method = ParseMembers("""
                class __GeneratedContainer {
                    protected func BuildRenderTree() {
                        Render()
                    }
                }
                """)[0];

            return new MacroExpansionResult
            {
                IntroducedMembers = [method],
                ReplacementDeclaration = context.CurrentDeclaration
            };
        }
    }

    private static ImmutableArray<MemberDeclarationSyntax> ParseMembers(string source)
    {
        var tree = SyntaxFactory.ParseSyntaxTree(source);
        var container = Assert.IsType<ClassDeclarationSyntax>(tree.GetRoot().Members.Single());
        return [.. container.Members];
    }
}

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
    public void GetExpandedRoot_RewritesAttachedAndFreestandingMacros()
    {
        var (compilation, tree) = CreateCompilation("""
            class Harness {
                #[Observable]
                var Title: string

                func GetAnswer() -> int {
                    return #add(20, Right: 22)
                }
            }
            """);

        compilation = compilation.AddMacroReferences(
            new MacroReference(new MacroCodeGenTests.IntroducedMethodMacro()),
            new MacroReference(new MacroCodeGenTests.ObservablePropertyMacro()),
            new MacroReference(typeof(FreestandingMacroCodeGenTests.AddMacro)));

        var model = compilation.GetSemanticModel(tree);
        var expandedRoot = model.GetExpandedRoot();
        var expandedText = expandedRoot.ToFullString();

        Assert.Contains("private var _Title: string", expandedText, StringComparison.Ordinal);
        Assert.Contains("return 20 + 22", expandedText, StringComparison.Ordinal);
        Assert.DoesNotContain("#add(", expandedText, StringComparison.Ordinal);
        Assert.DoesNotContain("#[Observable]", expandedText, StringComparison.Ordinal);

        var expandedProperty = expandedRoot.DescendantNodes()
            .OfType<PropertyDeclarationSyntax>()
            .Single(static property => property.Identifier.ValueText == "Title");

        Assert.NotNull(expandedProperty.AccessorList);
    }

    [Fact]
    public void GetExpandedRoot_PreservesBlankLinesAndFormatsFreestandingLambdaBodies()
    {
        var (compilation, tree) = CreateCompilation("""
            func Main() {
                use subscription = #wrap((value) => {
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
    public void GetExpandedRoot_RewritesEveryFreestandingMacroInSameMember()
    {
        var (compilation, tree) = CreateCompilation("""
            func Main() {
                let first = #add(1, Right: 2)
                let second = #add(3, Right: 4)
            }
            """);
        compilation = compilation.AddMacroReferences(
            new MacroReference(typeof(FreestandingMacroCodeGenTests.AddMacro)));

        var expandedText = compilation.GetSemanticModel(tree)
            .GetExpandedRoot()
            .ToFullString();

        Assert.Contains("let first = 1 + 2", expandedText, StringComparison.Ordinal);
        Assert.Contains("let second = 3 + 4", expandedText, StringComparison.Ordinal);
        Assert.DoesNotContain("#add(", expandedText, StringComparison.Ordinal);
    }

    [Fact]
    public void GetExpandedRoot_PreservesLineBreakAfterBangMacro()
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
            new MacroReference(typeof(FreestandingMacroSemanticTests.RavenBodyMacro)));

        var expandedText = compilation.GetSemanticModel(tree)
            .GetExpandedRoot()
            .ToFullString();

        Assert.Contains("let answer = 6\n    WriteLine(answer)", expandedText, StringComparison.Ordinal);
        Assert.DoesNotContain("raven!", expandedText, StringComparison.Ordinal);
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
            func Main() -> int => #wrap(21)
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
        var expression = tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>().Single();

        _ = model.GetMacroExpansion(expression);
        _ = model.GetMacroExpansion(expression);

        Assert.Equal(1, instrumentation.Macros.FreestandingExpansionInvocations);
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
                                val oldValue = _Count
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

    private sealed class WrapMacro : IFreestandingExpressionMacro
    {
        public string Name => "wrap";

        public bool AcceptsArguments => true;

        public FreestandingMacroExpansionResult Expand(FreestandingMacroContext context)
        {
            var callback = context.Arguments.Single().Expression;

            return new FreestandingMacroExpansionResult
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

    private static ImmutableArray<MemberDeclarationSyntax> ParseMembers(string source)
    {
        var tree = SyntaxFactory.ParseSyntaxTree(source);
        var container = Assert.IsType<ClassDeclarationSyntax>(tree.GetRoot().Members.Single());
        return [.. container.Members];
    }
}

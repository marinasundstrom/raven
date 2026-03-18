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
            new MacroReference(typeof(MacroCodeGenTests.EmitMacroPlugin)),
            new MacroReference(typeof(FreestandingMacroCodeGenTests.AddMacroPlugin)));

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

        compilation = compilation.AddMacroReferences(new MacroReference(typeof(FormattingMacroPlugin)));

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

    public sealed class FormattingMacroPlugin : IRavenMacroPlugin
    {
        public string Name => nameof(FormattingMacroPlugin);

        public ImmutableArray<IMacroDefinition> GetMacros()
            => [new ObservableMacro(), new WrapMacro()];
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
                    private val _CountChanged: Subject<int> = new Subject<int>()
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

        public MacroTarget Targets => MacroTarget.None;

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
}

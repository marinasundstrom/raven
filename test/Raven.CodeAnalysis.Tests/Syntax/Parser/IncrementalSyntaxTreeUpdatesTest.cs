using System;

using Raven.CodeAnalysis.Text;

using Xunit;
using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class IncrementalSyntaxTreeUpdatesTest(ITestOutputHelper output)
{
    private static readonly PrinterOptions s_treeDumpOptions = new()
    {
        IncludeNames = true,
        IncludeTokens = true,
        IncludeTrivia = true,
        IncludeSpans = true,
        IncludeLocations = false,
        Colorize = false,
        ExpandListsAsProperties = true
    };

    private static void AssertIncrementalParse(SourceText original, SourceText updated)
    {
        var originalTree = SyntaxTree.ParseText(original);
        var incrementalTree = originalTree.WithChangedText(updated);
        var expectedTree = SyntaxTree.ParseText(updated);

        AssertEquivalentSyntaxAndDiagnostics(expectedTree, incrementalTree);
    }

    private void AssertIncrementalStepMatchesFullParse(SyntaxTree previousTree, SourceText updated, string label, out SyntaxTree incrementalTree)
    {
        incrementalTree = previousTree.WithChangedText(updated);
        var expectedTree = SyntaxTree.ParseText(updated, previousTree.Options, previousTree.FilePath);

        output.WriteLine($"==== {label} source ====");
        output.WriteLine(updated.ToString());
        output.WriteLine($"==== {label} incremental tree ====");
        output.WriteLine(incrementalTree.GetRoot().GetSyntaxTreeRepresentation(s_treeDumpOptions));

        Assert.Equal(updated.ToString(), incrementalTree.GetRoot().ToFullString());
        AssertEquivalentSyntaxAndDiagnostics(expectedTree, incrementalTree);
    }

    private static void AssertEquivalentSyntaxAndDiagnostics(SyntaxTree expectedTree, SyntaxTree actualTree)
    {
        Assert.Equal(
            expectedTree.GetRoot().GetSyntaxTreeRepresentation(s_treeDumpOptions),
            actualTree.GetRoot().GetSyntaxTreeRepresentation(s_treeDumpOptions));

        var expectedDiagnostics = expectedTree.GetDiagnostics()
            .Select(static diagnostic => (
                diagnostic.Id,
                diagnostic.Severity,
                diagnostic.Location.SourceSpan,
                Message: diagnostic.GetMessage()))
            .ToArray();
        var actualDiagnostics = actualTree.GetDiagnostics()
            .Select(static diagnostic => (
                diagnostic.Id,
                diagnostic.Severity,
                diagnostic.Location.SourceSpan,
                Message: diagnostic.GetMessage()))
            .ToArray();

        Assert.Equal(expectedDiagnostics, actualDiagnostics);
        Assert.All(actualTree.GetDiagnostics(), diagnostic => Assert.Same(actualTree, diagnostic.Location.SourceTree));
    }

    [Fact]
    public void ChangedTextPolicy_UsesIncrementalParseForSmallSingleChange()
    {
        var original = SourceText.From("func Main() -> unit {}\n");
        var updated = original.Replace(original.Length - 1, 0, "// comment\n");
        var ranges = updated.GetChangeRanges(original);

        Assert.False(SyntaxTree.ShouldFullyReparseChangedText(original, updated, ranges));
    }

    [Fact]
    public void ChangedTextPolicy_UsesFullParseForWholeDocumentChange()
    {
        var original = SourceText.From("func Main() -> unit {}\n");
        var updated = original.Replace(new TextSpan(0, original.Length), "class C {}\n");
        var ranges = updated.GetChangeRanges(original);

        Assert.True(SyntaxTree.ShouldFullyReparseChangedText(original, updated, ranges));
    }

    [Fact]
    public void ChangedTextPolicy_UsesFullParseForLargeInsert()
    {
        var original = SourceText.From("func Main() -> unit {}\n");
        var insertedText = new string(' ', SyntaxTree.IncrementalParseMaxChangeLength + 1);
        var updated = original.Replace(original.Length - 1, 0, insertedText);
        var ranges = updated.GetChangeRanges(original);

        Assert.True(SyntaxTree.ShouldFullyReparseChangedText(original, updated, ranges));
    }

    [Fact]
    public void ApplyTextChangeToSyntaxTree()
    {
        var sourceText = SourceText.From(
            """
            if (foo)  {
                return 0;
            } else if (bar ) {
                return 1;
            }
            """);

        var textChange = new TextChange(
            new TextSpan(4, 3),
            "x"
        );

        var changedSourceText = sourceText.WithChange(textChange);

        AssertIncrementalParse(sourceText, changedSourceText);
    }

    [Fact]
    public void GetChanges_ReturnsChangesFromOldTreeToCurrentTree()
    {
        var sourceText = SourceText.From("let value = 1\n");
        var changedSourceText = sourceText.Replace(new TextSpan("let value = ".Length, 1), "42");
        var originalTree = SyntaxTree.ParseText(sourceText);
        var changedTree = originalTree.WithChangedText(changedSourceText);

        var change = Assert.Single(changedTree.GetChanges(originalTree));

        Assert.Equal(new TextSpan("let value = ".Length, 1), change.Span);
        Assert.Equal("42", change.NewText);
    }

    [Fact]
    public void ApplyChangedTextToSyntaxTree()
    {
        var sourceText = SourceText.From(
            """
            if (foo)  {
                return 0;
            }
            """);

        var changedSourceText = SourceText.From(
            """
            if (foo)  {
                return 0;
            } else if (bar ) {
                return 1;
            }
            """);

        AssertIncrementalParse(sourceText, changedSourceText);
    }

    [Fact]
    public void TypingEmptyMatchIntoMacroBody_MatchesFullParseAtEveryStep()
    {
        var text = SourceText.From(
            """
            import Raven.CodeAnalysis.Syntax.*

            macro AddOffset(offset: int, expression: ExpressionSyntax) {
                let source = expression.ToString() + " + " + offset.ToString()
                expand Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression(source)
            }
            """);
        var tree = SyntaxTree.ParseText(text);
        var insertionPosition = text.ToString().IndexOf(
            "    let source",
            StringComparison.Ordinal);
        const string insertedText = """
                match expression {

                }

            """;

        for (var index = 0; index < insertedText.Length; index++)
        {
            var character = insertedText[index];
            text = text.Replace(insertionPosition, 0, character.ToString());
            tree = tree.WithChangedText(text);
            insertionPosition++;

            var fullParse = SyntaxTree.ParseText(text);
            try
            {
                AssertEquivalentSyntaxAndDiagnostics(fullParse, tree);
            }
            catch
            {
                output.WriteLine($"Mismatch after character {index}: {FormatCharacter(character)}");
                output.WriteLine(text.ToString());
                throw;
            }
        }

        static string FormatCharacter(char character) => character switch
        {
            '\n' => "\\n",
            '\r' => "\\r",
            '\t' => "\\t",
            _ => character.ToString()
        };
    }

    [Fact]
    public void AppendingBlankLinesAfterDeclarationMacro_MatchesFullParseAtEveryStep()
    {
        var text = SourceText.From(
            """
            import System.Console.*

            component! Greeting(Name: string = "") {
                WriteLine("Rendering Greeting for ${Name}")

                markup! {
                    <section class="greeting">
                        <h1>Hello {Name}</h1>
                    </section>
                }
            }
            """ + "\n");
        var tree = SyntaxTree.ParseText(text);

        for (var index = 0; index < 3; index++)
        {
            text = text.Replace(text.Length, 0, "\n");
            AssertIncrementalStepMatchesFullParse(tree, text, $"blank line {index + 1}", out tree);

            Assert.Empty(tree.GetDiagnostics());
            Assert.Single(tree.GetRoot().Members.OfType<FreestandingMacroDeclarationSyntax>());
        }
    }

    [Fact]
    public void TypingIncompleteEnumDeclaration_MatchesFullParseAtEveryStep()
    {
        var text = SourceText.From("func Main() { }\n");
        var tree = SyntaxTree.ParseText(text);

        foreach (var character in "enum Status {\n    Ok")
        {
            text = text.Replace(text.Length, 0, character.ToString());
            AssertIncrementalStepMatchesFullParse(tree, text, $"typed '{character}'", out tree);
        }
    }

    [Fact]
    public void RemovingTypeNameBeforeNewLine_MatchesFullParse()
    {
        var original = SourceText.From("class C {\n    val value: int\n}\n");
        var tree = SyntaxTree.ParseText(original);
        var typePosition = original.ToString().IndexOf("int", StringComparison.Ordinal);
        var updated = original.Replace(typePosition, "int".Length, string.Empty);

        AssertIncrementalStepMatchesFullParse(tree, updated, "removed type name", out _);
    }

    [Fact]
    public void InsertingClassCloseBraceBeforeFollowingDeclarations_DoesNotCreateExecutableGlobalStatement()
    {
        var text = SourceText.From("""
func Main() {
    val message = "Hello from Raven"
    System.Console.WriteLine(message)
}

record ItemId(Value: Guid)

record Item(Id: ItemId, Name: string)

class ItemRepository : IRepository<Item, ItemId> {
    func getById(id: ItemId) -> Result<Item, RepositoryError> {
        return Error(RepositoryError.NotFound)
    }

interface IRepository<T, TId> {
    func getById(id: TId) -> Result<T, RepositoryError> {
        return Error(RepositoryError.NotFound)
    }
}

union RepositoryError {
    case NotFound
}
""");
        var tree = SyntaxTree.ParseText(text);
        var interfacePosition = text.ToString().IndexOf("interface", StringComparison.Ordinal);
        text = text.Replace(interfacePosition, 0, "}\n\n");

        AssertIncrementalStepMatchesFullParse(tree, text, "closed class", out tree);

        var root = tree.GetRoot();
        var globalStatement = Assert.Single(root.Members.OfType<GlobalStatementSyntax>());
        Assert.IsType<FunctionStatementSyntax>(globalStatement.Statement);
        Assert.Equal(text.ToString(), root.ToFullString());
    }

    [Theory]
    [InlineData("declaration", "component! Greeting() {\n}\n", "component")]
    [InlineData("expression", "func Run() {\n    let value = answer!()\n}\n", "answer")]
    [InlineData("statement", "func Run() {\n    trace! { }\n}\n", "trace")]
    [InlineData("top-level-statement", "trace! { }\nclass Existing { }\n", "trace")]
    [InlineData("file-scoped-statement", "namespace App;\n\ntrace! { }\nclass Existing { }\n", "trace")]
    [InlineData("member", "class Model {\n    members! { }\n}\n", "members")]
    public void EditingTriviaAroundMacroCarrier_PreservesInvocationForm(
        string carrierKind,
        string source,
        string alias)
    {
        var originalText = SourceText.From(source);
        var originalTree = SyntaxTree.ParseText(originalText);
        var originalCarrier = AssertMacroCarrier(originalTree, carrierKind, alias);

        foreach (var (label, position, insertion) in new[]
                 {
                     ("before", originalCarrier.Span.Start, "\n// edit before\n"),
                     ("after", originalCarrier.Span.End, "\n// edit after\n")
                 })
        {
            var updatedText = originalText.Replace(position, 0, insertion);
            var updatedTree = originalTree.WithChangedText(updatedText);
            var fullTree = SyntaxTree.ParseText(updatedText);

            output.WriteLine($"==== {carrierKind} {label} source ====");
            output.WriteLine(updatedText.ToString());

            Assert.Equal(updatedText.ToString(), updatedTree.GetRoot().ToFullString());
            Assert.Empty(updatedTree.GetDiagnostics());
            Assert.Empty(fullTree.GetDiagnostics());
            _ = AssertMacroCarrier(updatedTree, carrierKind, alias);
            _ = AssertMacroCarrier(fullTree, carrierKind, alias);
        }
    }

    private static SyntaxNode AssertMacroCarrier(
        SyntaxTree tree,
        string carrierKind,
        string alias)
    {
        var root = tree.GetRoot();
        return carrierKind switch
        {
            "declaration" => Assert.Single(
                root.DescendantNodesAndSelf().OfType<FreestandingMacroDeclarationSyntax>(),
                declaration => declaration.Name.ToString() == alias),
            "member" => Assert.Single(
                root.DescendantNodesAndSelf().OfType<FreestandingMacroMemberDeclarationSyntax>(),
                member => member.Name.ToString() == alias),
            "expression" => AssertExpressionCarrier(
                root,
                alias,
                expectStatement: false,
                expectGlobalStatement: false),
            "statement" => AssertExpressionCarrier(
                root,
                alias,
                expectStatement: true,
                expectGlobalStatement: false),
            "top-level-statement" or "file-scoped-statement" => AssertExpressionCarrier(
                root,
                alias,
                expectStatement: true,
                expectGlobalStatement: true),
            _ => throw new ArgumentOutOfRangeException(nameof(carrierKind))
        };
    }

    private static FreestandingMacroExpressionSyntax AssertExpressionCarrier(
        SyntaxNode root,
        string alias,
        bool expectStatement,
        bool expectGlobalStatement)
    {
        var expression = Assert.Single(
            root.DescendantNodesAndSelf().OfType<FreestandingMacroExpressionSyntax>(),
            candidate => candidate.Name.ToString() == alias);
        var statement = expression.Parent as ExpressionStatementSyntax;

        Assert.Equal(expectStatement, statement is not null);
        Assert.Equal(
            expectGlobalStatement,
            statement?.Parent is GlobalStatementSyntax);
        return expression;
    }

    [Fact]
    public void EditingTypedConditionalBindingThroughMissingType_MatchesFullParse()
    {
        var original = SourceText.From(
            """
            func Length(value: string?) -> int {
                if let text: string = value {
                    return text.Length
                }

                return 0
            }
            """);
        var tree = SyntaxTree.ParseText(original);
        var missingType = SourceText.From(original.ToString().Replace(
            "text: string =",
            "text: =",
            StringComparison.Ordinal));
        AssertIncrementalStepMatchesFullParse(tree, missingType, "missing conditional-binding type", out tree);

        var restored = SourceText.From(missingType.ToString().Replace(
            "text: =",
            "text: string =",
            StringComparison.Ordinal));
        AssertIncrementalStepMatchesFullParse(tree, restored, "restored conditional-binding type", out tree);

        Assert.Empty(tree.GetDiagnostics());
        var binding = tree.GetRoot().DescendantNodes().OfType<IfPatternStatementSyntax>().Single();
        var variablePattern = Assert.IsType<VariablePatternSyntax>(binding.Pattern);
        var typedDesignation = Assert.IsType<TypedVariableDesignationSyntax>(variablePattern.Designation);
        var designation = Assert.IsType<SingleVariableDesignationSyntax>(typedDesignation.Designation);
        Assert.Equal("text", designation.Identifier.ValueText);
        Assert.Equal("string", typedDesignation.TypeAnnotation.Type.ToString());
    }

    [Fact]
    public void EditingNullableMatchArmThroughMissingExpression_MatchesFullParse()
    {
        var original = SourceText.From(
            """
            func Describe(value: string?) -> int {
                return match value {
                    string text => text.Length
                    null => 0
                }
            }
            """);
        var tree = SyntaxTree.ParseText(original);
        var missingExpression = SourceText.From(original.ToString().Replace(
            "null => 0",
            "null =>",
            StringComparison.Ordinal));
        AssertIncrementalStepMatchesFullParse(tree, missingExpression, "missing nullable match arm expression", out tree);

        var restored = SourceText.From(missingExpression.ToString().Replace(
            "null =>",
            "null => 0",
            StringComparison.Ordinal));
        AssertIncrementalStepMatchesFullParse(tree, restored, "restored nullable match arm expression", out tree);

        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void EditingValidTreeToMissingExpression_MatchesFullParseDiagnostics()
    {
        var original = SourceText.From(
            """
            func Main() {
                let value = 1
            }
            """);
        var expressionPosition = original.ToString().IndexOf('1');
        var updated = original.Replace(expressionPosition, 1, string.Empty);

        AssertIncrementalParse(original, updated);
    }

    [Fact]
    public void EditingBeforeUnchangedDiagnostic_ShiftsDiagnosticToMatchFullParse()
    {
        var original = SourceText.From(
            """
            func First() {
                let value = 1
            }

            func Second() {
                let missing =
            }
            """);
        var expressionPosition = original.ToString().IndexOf('1');
        var updated = original.Replace(expressionPosition, 1, "100");
        var originalTree = SyntaxTree.ParseText(original);
        var unchangedDeclaration = originalTree.GetRoot().Members[1].Green;
        var incrementalTree = originalTree.WithChangedText(updated);

        Assert.Same(unchangedDeclaration, incrementalTree.GetRoot().Members[1].Green);
        AssertEquivalentSyntaxAndDiagnostics(SyntaxTree.ParseText(updated), incrementalTree);
    }

    [Fact]
    public void FixingMissingExpression_RemovesDiagnosticToMatchFullParse()
    {
        var original = SourceText.From(
            """
            func Main() {
                let value =
            }
            """);
        var insertionPosition = original.ToString().IndexOf(
            "\n}",
            StringComparison.Ordinal);
        var updated = original.Replace(insertionPosition, 0, " 1");

        Assert.NotEmpty(SyntaxTree.ParseText(original).GetDiagnostics());
        AssertIncrementalParse(original, updated);
    }

    [Fact]
    public void DeletingConstructorBody_MatchesFullParseRecovery()
    {
        var original = SourceText.From(
            """
            class C {
                init(value: int) {}
                func Next() {}
            }
            """);
        var bodyPosition = original.ToString().IndexOf("{}", StringComparison.Ordinal);
        var updated = original.Replace(bodyPosition, 2, string.Empty);

        AssertIncrementalParse(original, updated);
    }

    [Fact]
    public void EditingGenericConstraintBetweenValidMissingAndRestoredMatchesFullParse()
    {
        var original = SourceText.From(
            """
            import System.Collections.Generic.*

            func Convert<T>(value: T) -> T
                where T: IEnumerable<string> {
                value
            }

            func Stable(value: int) -> int {
                value
            }
            """);
        var constraintStart = original.ToString().IndexOf("IEnumerable<string>", StringComparison.Ordinal);
        var missing = original.Replace(constraintStart, "IEnumerable<string>".Length, string.Empty);
        var originalTree = SyntaxTree.ParseText(original);

        AssertIncrementalStepMatchesFullParse(originalTree, missing, "missing constraint", out var missingTree);

        var restored = missing.Replace(constraintStart, 0, "class");
        AssertIncrementalStepMatchesFullParse(missingTree, restored, "restored constraint", out _);
    }

    [Fact]
    public void EditingTreeWithMissingExpression_DoesNotFailIncrementalReplacement()
    {
        var original = SourceText.From(
            """
            func First() {
                let value =
            }

            func Second() {
                let value = 1
            }
            """);
        var tree = SyntaxTree.ParseText(original);
        var literalPosition = original.ToString().LastIndexOf('1');
        var updated = original.Replace(literalPosition, 1, "2");

        var incrementalTree = tree.WithChangedText(updated);
        var fullParse = SyntaxTree.ParseText(updated);

        AssertEquivalentSyntaxAndDiagnostics(fullParse, incrementalTree);
        Assert.Equal(
            IncrementalParseFallbackReason.None,
            incrementalTree.IncrementalParseFallbackReason);
    }

    [Fact]
    public void EditingTreeWithSkippedTokens_DoesNotFailIncrementalReplacement()
    {
        var original = SourceText.From(
            """
            func First() {
                )
            }

            func Second() {
                let value = 1
            }
            """);
        var tree = SyntaxTree.ParseText(original);
        var literalPosition = original.ToString().LastIndexOf('1');
        var updated = original.Replace(literalPosition, 1, "2");

        var incrementalTree = tree.WithChangedText(updated);
        var fullParse = SyntaxTree.ParseText(updated);

        AssertEquivalentSyntaxAndDiagnostics(fullParse, incrementalTree);
        Assert.Equal(
            IncrementalParseFallbackReason.None,
            incrementalTree.IncrementalParseFallbackReason);
    }

    [Fact]
    public void EditProducingSkippedTokens_RecordsNewRecoveryFallback()
    {
        var original = SourceText.From(
            """
            func Compute(value: int) -> int {
                let answer = value
                return answer
            }
            """);
        var insertionPosition = original.ToString().IndexOf("value\n", StringComparison.Ordinal);
        var updated = original.Replace(insertionPosition, 0, "]");

        var incrementalTree = SyntaxTree.ParseText(original).WithChangedText(updated);
        var fullParse = SyntaxTree.ParseText(updated);

        AssertEquivalentSyntaxAndDiagnostics(fullParse, incrementalTree);
        Assert.Equal(
            IncrementalParseFallbackReason.NewRecoverySyntax,
            incrementalTree.IncrementalParseFallbackReason);
    }

    [Fact]
    public void UndoAfterFallback_MatchesFullParse()
    {
        var original = SourceText.From(
            """
            union RepositoryError {
                case NotFound
            }

            func GetError() -> RepositoryError {
                return RepositoryError.NotFound
            }
            """);
        var memberAccessPosition = original.ToString().LastIndexOf("NotFound", StringComparison.Ordinal);
        var insertionPosition = memberAccessPosition + 4;
        var malformed = original.Replace(insertionPosition, 0, "@");

        var malformedTree = SyntaxTree.ParseText(original).WithChangedText(malformed);
        var restoredTree = malformedTree.WithChangedText(original);

        Assert.NotEqual(IncrementalParseFallbackReason.None, malformedTree.IncrementalParseFallbackReason);
        Assert.Equal(IncrementalParseFallbackReason.PreviousFallback, restoredTree.IncrementalParseFallbackReason);
        AssertEquivalentSyntaxAndDiagnostics(SyntaxTree.ParseText(original), restoredTree);
    }

    [Fact]
    public void StrictIncrementalParsing_ThrowsWithFallbackReason()
    {
        var original = SourceText.From(
            """
            func Compute(value: int) -> int {
                return value
            }
            """);
        var insertionPosition = original.ToString().IndexOf("value\n", StringComparison.Ordinal);
        var updated = original.Replace(insertionPosition, 0, "]");
        var options = new ParseOptions
        {
            ThrowOnIncrementalParseFallback = true
        };
        var tree = SyntaxTree.ParseText(original, options);

        var exception = Assert.Throws<IncrementalParseFallbackException>(() => tree.WithChangedText(updated));

        Assert.Equal(IncrementalParseFallbackReason.NewRecoverySyntax, exception.Reason);
        Assert.Contains("NewRecoverySyntax", exception.Message, StringComparison.Ordinal);
    }

    [Fact]
    public void StrictIncrementalParsing_AllowsCommonDeveloperEditsWithoutFallback()
    {
        var options = new ParseOptions
        {
            ThrowOnIncrementalParseFallback = true
        };
        var text = SourceText.From(
            """
            func Compute(value: int) -> int {
                let baseValue = value + 1
                let answer = baseValue * 2
                return answer
            }
            """);
        string[] snapshots =
        [
            text.ToString().Replace("value + 1", "value  + 1", StringComparison.Ordinal),
            text.ToString().Replace("value + 1", "value + 2", StringComparison.Ordinal),
            text.ToString().Replace("baseValue * 2", "baseValue * 3", StringComparison.Ordinal)
        ];

        foreach (var snapshot in snapshots)
        {
            var updated = SourceText.From(snapshot);
            SyntaxTree tree;
            try
            {
                tree = SyntaxTree.ParseText(text, options).WithChangedText(updated);
            }
            catch (IncrementalParseFallbackException exception)
            {
                throw new InvalidOperationException(
                    $"Common edit unexpectedly required {exception.Reason}:\n{snapshot}",
                    exception);
            }

            Assert.Equal(IncrementalParseFallbackReason.None, tree.IncrementalParseFallbackReason);
            AssertEquivalentSyntaxAndDiagnostics(SyntaxTree.ParseText(updated, options), tree);
        }
    }

    [Fact]
    public void ApplyChangedTextToSyntaxTree_Advanced()
    {
        var sourceText = SourceText.From(
            """
            {
                if (foo)  {
                    return 0;
                }
            }
            """);

        var changedSourceText = SourceText.From(
            """
            {
                if (foo)  {
                    return 0;
                } else if (bar ) {
                    return 1;
                }
            }
            """);

        AssertIncrementalParse(sourceText, changedSourceText);
    }

    [Fact]
    public void ApplyChangedTextToSyntaxTree3()
    {
        var sourceText = SourceText.From(
            """
            if (foo)  {
                return 0;
            }
            """);

        var changedSourceText = SourceText.From(
            """
            if (foo)  {
                return bar;
            }
            """);

        AssertIncrementalParse(sourceText, changedSourceText);
    }

    [Fact]
    public void ApplyChangedTextToSyntaxTree4()
    {
        var sourceText = SourceText.From(
            """
            if (foo)  {
                return 0;
            }
            """);

        var changedSourceText = SourceText.From(
            """
            if (foo)  {
                if(x) {}
            }
            """);

        AssertIncrementalParse(sourceText, changedSourceText);
    }

    [Fact]
    public void ApplyChangedTextToSyntaxTree5()
    {
        var sourceText = SourceText.From(
            """
            if (foo)  {
                if(x) {}
            }
            """);

        var changedSourceText = SourceText.From(
            """
            if (foo)  {
                if(x) {

                }
                else {}
            }
            """);

        AssertIncrementalParse(sourceText, changedSourceText);
    }

    [Fact]
    public void ApplyChangedTextToSyntaxTree6()
    {
        var sourceText = SourceText.From(
            """
            if (foo)  {
                return 0;
            } else if (bar ) {
                return 1;
            }
            """);

        var changedSourceText = SourceText.From(
            """
            if (foo)  {
                return 0;
            } else {
                return 1;
            }
            """);

        AssertIncrementalParse(sourceText, changedSourceText);
    }

    [Fact]
    public void ApplyChangedTextToSyntaxTree7()
    {
        var sourceText = SourceText.From(
            """
            try {
                return 0;
            } catch (Foo ex) {
                return 1;
            }
            """);

        var changedSourceText = SourceText.From(
            """
            try {
                return 0;
            } catch (Bar ex) {
                return 1;
            }
            """);

        AssertIncrementalParse(sourceText, changedSourceText);
    }

    [Fact]
    public void ApplyChangedTextToSyntaxTree_AddParameter()
    {
        var sourceText = SourceText.From(
            """
            fn add(a:int) {
                return a;
            }
            """);

        var changedSourceText = SourceText.From(
            """
            fn add(a:int, b:int) {
                return a + b;
            }
            """);

        AssertIncrementalParse(sourceText, changedSourceText);
    }

    [Fact]
    public void ApplyChangedTextToSyntaxTree_InsertAtStatementBoundaryInBlock()
    {
        var source = """
            fn compute() {
                return 1;
            }
            """;

        var sourceText = SourceText.From(source);
        var insertionPosition = source.IndexOf("return 1;", StringComparison.Ordinal) + "return 1;".Length;
        var changedSourceText = sourceText.WithChange(
            new TextChange(new TextSpan(insertionPosition, 0), "\n    return 2;"));

        AssertIncrementalParse(sourceText, changedSourceText);
    }

    [Fact]
    public void SequentialSameDocumentEdits_OutputActualTreeAfterEachIncrementalChange()
    {
        var text = SourceText.From(
            """
            func Main() -> unit {
                let first = 1
                let second = first + 1
                System.Console.WriteLine(second)
            }
            """);
        var tree = SyntaxTree.ParseText(text, path: "/tmp/live.rav");

        var firstEdit = text.Replace(
            text.ToString().IndexOf("first + 1", StringComparison.Ordinal),
            "first + 1".Length,
            "first + 2");
        AssertIncrementalStepMatchesFullParse(tree, firstEdit, "edit 1", out tree);

        var secondEdit = firstEdit.Replace(
            firstEdit.ToString().IndexOf("System.Console.WriteLine(second)", StringComparison.Ordinal),
            "System.Console.WriteLine(second)".Length,
            "System.Console.WriteLine(first)");
        AssertIncrementalStepMatchesFullParse(tree, secondEdit, "edit 2", out tree);

        var thirdEdit = secondEdit.Replace(
            secondEdit.ToString().IndexOf("let second = first + 2", StringComparison.Ordinal),
            "let second = first + 2".Length,
            "let second = first + 3\n    let third = second + first");
        AssertIncrementalStepMatchesFullParse(tree, thirdEdit, "edit 3", out tree);
    }
}

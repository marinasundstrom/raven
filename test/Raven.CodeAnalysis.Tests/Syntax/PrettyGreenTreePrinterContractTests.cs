#if RAVEN_PORT
extern alias Port;
using GreenPrinter = Port::Raven.CodeAnalysis.Syntax.InternalSyntax.PrettyGreenTreePrinter;
using GreenPrinterOptions = Port::Raven.CodeAnalysis.Syntax.InternalSyntax.PrettyGreenTreePrinterOptions;
#else
using GreenPrinter = Raven.CodeAnalysis.Syntax.InternalSyntax.PrettyGreenTreePrinter;
using GreenPrinterOptions = Raven.CodeAnalysis.Syntax.InternalSyntax.PrettyGreenTreePrinterOptions;
#endif

using Raven.CodeAnalysis.Syntax.InternalSyntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Syntax.Tests;

[Trait("Target", "Shared")]
public sealed class PrettyGreenTreePrinterContractTests
{
    [Fact]
    public void PlainTreeUsesStableBranchesAndWriterMatchesString()
    {
        var root = SyntaxFactory.IdentifierName("x").Green;
        var options = new GreenPrinterOptions { Colorize = false, IncludeWidths = false };
        var text = GreenPrinter.PrintToString(root, options);
        Assert.Equal($"IdentifierName{Environment.NewLine}    └── IdentifierToken: x{Environment.NewLine}", text);
        using var writer = new StringWriter();
        GreenPrinter.Print(root, writer, options);
        Assert.Equal(text, writer.ToString());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void TriviaTextAndColorFollowOptions(bool colorize)
    {
        var token = SyntaxFactory.Identifier("x").WithLeadingTrivia(SyntaxFactory.Whitespace(" \t"));
        var text = GreenPrinter.PrintToString(token.Green, new GreenPrinterOptions
        {
            Colorize = colorize,
            IncludeTrivia = true,
            IncludeWidths = false
        });
        Assert.Contains("LeadingTrivia", text);
        Assert.Contains("␣\\t", text);
        Assert.Equal(colorize, text.Contains("\u001b["));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void DiagnosticsAndAnnotationsCanBeInlineOrChildren(bool children)
    {
        var diagnostic = DiagnosticInfo.Create(CompilerDiagnostics.CharacterExpected, new TextSpan(0, 0), ')');
        var root = SyntaxFactory.IdentifierName("x").Green
            .WithAdditionalDiagnostics(diagnostic, diagnostic)
            .WithAdditionalAnnotations(new SyntaxAnnotation("tag", "value"));
        var text = GreenPrinter.PrintToString(root, new GreenPrinterOptions
        {
            Colorize = false,
            IncludeWidths = false,
            IncludeAnnotations = true,
            DiagnosticsAsChildren = children,
            AnnotationsAsChildren = children
        });
        Assert.Contains("#[tag]", text);
        Assert.Equal(children ? 2 : 1, text.Split(diagnostic.Descriptor.Id).Length - 1);
        Assert.Equal(children, text.Contains("#[tag] = value"));
    }

    [Fact]
    public void AddingAnnotationsPreservesDiagnosticsAndEarlierAnnotations()
    {
        var diagnostic = DiagnosticInfo.Create(CompilerDiagnostics.CharacterExpected, new TextSpan(0, 0), ')');
        var first = new SyntaxAnnotation("first");
        var second = new SyntaxAnnotation("second");
        var original = SyntaxFactory.IdentifierName("x").Green.WithAdditionalDiagnostics(diagnostic);
        var annotated = original.WithAdditionalAnnotations(first).WithAdditionalAnnotations(second);

        Assert.Equal(new[] { first, second }, annotated.GetAnnotations(new[] { "first", "second" }));
        Assert.Same(diagnostic, Assert.Single(annotated.GetDiagnosticsRecursive()));
        Assert.Empty(original.GetAnnotations(new[] { "first", "second" }));
        Assert.Same(annotated, annotated.WithAdditionalAnnotations());
    }

    [Fact]
    public void FlattenedListsRetainOriginalSlotIndices()
    {
        var root = SyntaxFactory.Block(SyntaxFactory.List<StatementSyntax>([SyntaxFactory.ExpressionStatement(SyntaxFactory.IdentifierName("x"))])).Green;
        var options = new GreenPrinterOptions
        {
            Colorize = false,
            IncludeWidths = false,
            IncludeSlotIndices = true,
            FlattenSyntaxLists = true,
            HideZeroWidthSlots = true
        };
        var text = GreenPrinter.PrintToString(root, options);
        Assert.Contains("[1.0] ExpressionStatement", text);
        Assert.DoesNotContain(" (Missing)", text);
    }

    [Fact]
    public void NullInputsAreRejected()
    {
        using var writer = new StringWriter();
        Assert.Throws<ArgumentNullException>(() => GreenPrinter.Print(null!, writer));
        Assert.Throws<ArgumentNullException>(() => GreenPrinter.Print(SyntaxFactory.IdentifierName("x").Green, null!));
    }
}

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Implements low-level expansion mechanics used by the standard Raven macro library.
/// </summary>
/// <remarks>
/// This is a transitional compiler-side implementation. As the Raven macro-function
/// API gains the required diagnostic and syntax-construction capabilities, behavior
/// that does not require compiler internals should move wholly or partly into
/// Raven.Macros.
/// </remarks>
public static partial class StandardMacroExpansions
{
    private const string MissingDelegateTypeCode = "COMPILE001";
    private const string CompileExpansionFailedCode = "COMPILE002";

    /// <summary>
    /// Expands a Raven runtime compilation expression.
    /// </summary>
    public static FreestandingMacroExpansionResult ExpandCompile(TokenTreeMacroContext context)
    {
        if (context.Syntax.Name is not GenericNameSyntax genericName ||
            genericName.TypeArgumentList.Arguments.Count != 1)
        {
            return FreestandingMacroExpansionResult.FromDiagnostic(
                context.CreateDiagnostic(
                    "The compile macro requires exactly one delegate type argument, as in compile<Func<int>>! { ... }.",
                    syntax: context.Syntax.Name,
                    code: MissingDelegateTypeCode));
        }

        var quoteResult = ExpandQuote(context);
        if (quoteResult.Expression is null)
            return quoteResult;

        var delegateType = genericName.TypeArgumentList.Arguments[0].Type.ToString();
        var expansionText =
            $"Raven.CodeAnalysis.RavenCompiler.Compile<{delegateType}>({quoteResult.Expression.ToFullString()})";
        var parser = new Syntax.InternalSyntax.Parser.LanguageParser(
            context.Syntax.SyntaxTree?.FilePath,
            context.Syntax.SyntaxTree?.Options ?? new ParseOptions());
        var expansion = parser.ParseSyntaxWithDiagnostics(
            typeof(ExpressionSyntax),
            SourceText.From(expansionText),
            position: 0,
            consumeFullText: true);

        if (expansion is null ||
            expansion.Value.Diagnostics.Count > 0 ||
            expansion.Value.Root.IsMissing ||
            expansion.Value.Root.CreateRed() is not ExpressionSyntax expression)
        {
            return FreestandingMacroExpansionResult.FromDiagnostic(
                context.CreateDiagnostic(
                    "The compiler could not construct the runtime compilation expression.",
                    syntax: context.Syntax.Name,
                    code: CompileExpansionFailedCode));
        }

        return FreestandingMacroExpansionResult.FromExpression(expression);
    }
}

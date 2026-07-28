using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

internal sealed class IntrinsicCompileMacro : ITokenTreeExpressionMacro
{
    private const string MissingDelegateTypeCode = "COMPILE001";
    private const string ExpansionFailedCode = "COMPILE002";

    public static IntrinsicCompileMacro Instance { get; } = new();

    private IntrinsicCompileMacro()
    {
    }

    public string Name => "compile";

    public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
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

        var quoteResult = IntrinsicQuoteMacro.Instance.Expand(context);
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
                    code: ExpansionFailedCode));
        }

        return FreestandingMacroExpansionResult.FromExpression(expression);
    }
}

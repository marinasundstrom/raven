using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

[assembly: RavenCompilerPlugin(typeof(SampleMacros.CustomTokenMacro))]

namespace SampleMacros;

public sealed class CustomTokenMacro : ITokenTreeExpressionMacro, IMacroTokenStreamProvider
{
    private const int AnswerTokenRawKind = 80_101;

    public string Name => "customToken";

    public IMacroTokenStream CreateTokenStream(MacroTokenStreamContext context)
        => new CustomTokenStream(context, AnswerTokenRawKind);

    public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
    {
        var stream = context.CreateTokenStream();
        var token = stream.ReadToken();
        if (token.RawKind != AnswerTokenRawKind ||
            token.Text != "⟨answer⟩" ||
            !stream.IsEndOfFile)
        {
            return FreestandingMacroExpansionResult.FromDiagnostic(
                context.CreateDiagnostic("Expected the custom token '⟨answer⟩'."));
        }

        return FreestandingMacroExpansionResult.FromExpression(
            SyntaxFactory.ParseExpression("42"));
    }

    private sealed class CustomTokenStream : IMacroTokenStream
    {
        private readonly SyntaxToken _token;
        private bool _hasRead;

        public CustomTokenStream(MacroTokenStreamContext context, int rawKind)
        {
            var text = context.BodyText.Trim();
            var position = context.BodyText.IndexOf(text, StringComparison.Ordinal);
            _token = SyntaxFactory.Token(rawKind, text, position);
        }

        public bool IsEndOfFile => _hasRead;

        public SyntaxToken PeekToken(int offset = 0)
        {
            if (offset != 0 || _hasRead)
                throw new ArgumentOutOfRangeException(nameof(offset));

            return _token;
        }

        public SyntaxToken ReadToken()
        {
            if (_hasRead)
                throw new InvalidOperationException("The custom token stream has been consumed.");

            _hasRead = true;
            return _token;
        }
    }
}

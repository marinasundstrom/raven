namespace Raven.CodeAnalysis.Syntax;

public sealed partial class CollectionExpressionSyntax
{
    public bool IsMutableByDefault => ExclamationToken.Kind == SyntaxKind.ExclamationToken;

    public bool IsImmutableByDefault => !IsMutableByDefault;
}

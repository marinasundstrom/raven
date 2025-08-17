using System.Runtime.CompilerServices;

namespace Raven.CodeAnalysis.Syntax;

public static partial class SyntaxFactory
{
    public static BlockSyntax Block() => new BlockSyntax(OpenBraceToken, EmptyList<StatementSyntax>(), CloseBraceToken);
}
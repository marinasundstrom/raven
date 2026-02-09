namespace Raven.CodeAnalysis.Syntax
{
    public static partial class SyntaxFactory
    {
        public static IdentifierNameSyntax IdentifierName(string name) => new(Identifier(name));
    }
}
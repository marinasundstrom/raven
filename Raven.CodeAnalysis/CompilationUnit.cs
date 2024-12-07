using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public class CompilationUnit
{
    //public SyntaxList<ImportDirectiveSyntax> Imports { get; set; } = default!;

    public SyntaxList<MemberDeclarationSyntax> Members { get; set; } = default!;
}
namespace Generator;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

public partial class SyntaxNodePartialGenerator : IIncrementalGenerator
{
    private static ClassDeclarationSyntax GenerateRewriterPartialClass(SourceProductionContext context, INamedTypeSymbol? classSymbol)
    {
        return VisitorPartialGenerator.GenerateVisitMethodForRewriter(context, classSymbol);
    }
}
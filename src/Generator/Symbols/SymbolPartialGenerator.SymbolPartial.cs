namespace Generator;

using System;
using System.Linq;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

public partial class SymbolPartialGenerator : IIncrementalGenerator
{
    private static ClassDeclarationSyntax GeneratePartialClass(SourceProductionContext context, INamedTypeSymbol? classSymbol)
    {
        var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();
        var className = classSymbol.Name;

        var accessModifier = Token(SyntaxKind.InternalKeyword);

        List<MemberDeclarationSyntax> members = [];

        members.AddRange(
            GenerateAcceptMethods(classSymbol));

        // Generate the partial class
        var generatedClass = ClassDeclaration(className)
            .WithModifiers(TokenList(accessModifier, Token(SyntaxKind.PartialKeyword)))
            .AddMembers(members.ToArray());

        return generatedClass;
    }

    private static IEnumerable<MemberDeclarationSyntax> GenerateAcceptMethods(INamedTypeSymbol classSymbol)
    {
        return AcceptMethodGenerator.GenerateAcceptMethods(new AcceptMethodGeneratorOptions(classSymbol.Name, suffix: "Symbol", makeInternal: false, nodeBaseClass: "ISymbol"));
    }
}
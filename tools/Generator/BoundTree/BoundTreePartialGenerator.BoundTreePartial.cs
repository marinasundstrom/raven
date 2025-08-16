namespace Generator;

using System;
using System.Linq;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

public partial class BoundNodePartialGenerator : IIncrementalGenerator
{
    private static ClassDeclarationSyntax GeneratePartialClass(SourceProductionContext context, INamedTypeSymbol? classSymbol)
    {
        var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();
        var className = classSymbol.Name;

        var accessModifier = Token(SyntaxKind.InternalKeyword);

        List<MemberDeclarationSyntax> members = [];

        members.AddRange(
            GenerateAcceptMethods(classSymbol));

        members.AddRange(
            GenerateUpdateMethod(classSymbol));

        // Generate the partial class
        var generatedClass = ClassDeclaration(className)
            .WithModifiers(TokenList(accessModifier, Token(SyntaxKind.PartialKeyword)))
            .AddMembers(members.ToArray());

        return generatedClass;
    }

    private static IEnumerable<MemberDeclarationSyntax> GenerateAcceptMethods(INamedTypeSymbol classSymbol)
    {
        return AcceptMethodGenerator.GenerateAcceptMethods(new AcceptMethodGeneratorOptions(classSymbol.Name, suffix: "Bound", makeInternal: false, visitorClassName: "BoundTree", nodeBaseClass: "BoundNode"));
    }

    private static IEnumerable<MemberDeclarationSyntax> GenerateUpdateMethod(INamedTypeSymbol classSymbol)
    {
        var parameters = classSymbol.Constructors.First().Parameters
            //.Where(x => x.Type.InheritsFromBoundNode() || x.Type.IsImplementingISymbol())
            .Select(x => new PropOrParamType(FormatName(x), x.Type.ToDisplayString()));

        return [UpdateMethodGenerator.GenerateUpdateMethod(classSymbol.Name, parameters)];
    }

    private static string FormatName(IParameterSymbol x)
    {
        return char.ToUpper(x.Name[0]) + x.Name.Substring(1);
    }
}
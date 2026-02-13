// Updated Red Node Generator
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using NodesShared;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Raven.Generators;

public static class VisitorGenerator
{
    private static bool IsDerivedFrom(
        SyntaxNodeModel candidate,
        string rootName,
        IReadOnlyDictionary<string, SyntaxNodeModel> byName)
    {
        var current = candidate;
        while (!string.IsNullOrEmpty(current.Inherits))
        {
            if (string.Equals(current.Inherits, rootName, StringComparison.Ordinal))
                return true;

            if (!byName.TryGetValue(current.Inherits, out current!))
                break;
        }

        return false;
    }

    private static IEnumerable<(SyntaxNodeModel Root, List<SyntaxNodeModel> Cases)> GetDispatchGroups(List<SyntaxNodeModel> allNodes)
    {
        var byName = allNodes.ToDictionary(n => n.Name, StringComparer.Ordinal);
        foreach (var root in allNodes.Where(n => n.IsAbstract))
        {
            var cases = allNodes
                .Where(n => !n.IsAbstract && IsDerivedFrom(n, root.Name, byName))
                .OrderBy(n => n.Name, StringComparer.Ordinal)
                .ToList();

            if (cases.Count > 0)
                yield return (root, cases);
        }
    }

    private static MemberDeclarationSyntax BuildVisitorDispatchMethod(SyntaxNodeModel root, IReadOnlyList<SyntaxNodeModel> cases)
    {
        var sb = new StringBuilder();
        sb.AppendLine($"public virtual void Visit{root.Name}({root.Name}Syntax node)");
        sb.AppendLine("{");
        sb.AppendLine("    switch (node)");
        sb.AppendLine("    {");
        foreach (var c in cases)
            sb.AppendLine($"        case {c.Name}Syntax n: Visit{c.Name}(n); return;");
        sb.AppendLine("        default: DefaultVisit(node); return;");
        sb.AppendLine("    }");
        sb.AppendLine("}");
        return ParseMemberDeclaration(sb.ToString())!;
    }

    private static MemberDeclarationSyntax BuildGenericVisitorDispatchMethod(SyntaxNodeModel root, IReadOnlyList<SyntaxNodeModel> cases)
    {
        var sb = new StringBuilder();
        sb.AppendLine($"public virtual TResult Visit{root.Name}({root.Name}Syntax node)");
        sb.AppendLine("{");
        sb.AppendLine("    return node switch");
        sb.AppendLine("    {");
        foreach (var c in cases)
            sb.AppendLine($"        {c.Name}Syntax n => Visit{c.Name}(n),");
        sb.AppendLine("        _ => DefaultVisit(node),");
        sb.AppendLine("    };");
        sb.AppendLine("}");
        return ParseMemberDeclaration(sb.ToString())!;
    }

    private static MemberDeclarationSyntax BuildRewriterDispatchMethod(SyntaxNodeModel root, IReadOnlyList<SyntaxNodeModel> cases)
    {
        var sb = new StringBuilder();
        sb.AppendLine($"public override SyntaxNode? Visit{root.Name}({root.Name}Syntax node)");
        sb.AppendLine("{");
        sb.AppendLine("    return node switch");
        sb.AppendLine("    {");
        foreach (var c in cases)
            sb.AppendLine($"        {c.Name}Syntax n => Visit{c.Name}(n),");
        sb.AppendLine("        _ => node,");
        sb.AppendLine("    };");
        sb.AppendLine("}");
        return ParseMemberDeclaration(sb.ToString())!;
    }

    public static CompilationUnitSyntax GenerateVisitorClass(List<SyntaxNodeModel> allNodes, bool isPublic = true, string namespaceName = "Raven.CodeAnalysis.Syntax")
    {
        var dispatchMethods = GetDispatchGroups(allNodes)
            .Select(group => BuildVisitorDispatchMethod(group.Root, group.Cases))
            .ToArray();

        var methods = allNodes
            .Where(n => !n.IsAbstract)
            .Select(n =>
                MethodDeclaration(
                    PredefinedType(Token(SyntaxKind.VoidKeyword)),
                    Identifier("Visit" + n.Name))
                .AddModifiers(Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.VirtualKeyword))
                .AddParameterListParameters(
                    Parameter(Identifier("node"))
                        .WithType(IdentifierName(n.Name + "Syntax")))
                .WithBody(Block(
                    ExpressionStatement(
                        InvocationExpression(
                            IdentifierName("DefaultVisit"))
                        .WithArgumentList(ArgumentList(SingletonSeparatedList(
                            Argument(IdentifierName("node")))))))))
            .ToArray();

        var visitorClass =
            ClassDeclaration("SyntaxVisitor")
                .AddModifiers(isPublic ? Token(SyntaxKind.PublicKeyword) : Token(SyntaxKind.InternalKeyword), Token(SyntaxKind.AbstractKeyword), Token(SyntaxKind.PartialKeyword))
                .AddMembers(dispatchMethods.Concat(methods).ToArray());

        return CompilationUnit()
            .AddUsings(UsingDirective(ParseName(namespaceName)))
            .AddMembers(NamespaceDeclaration(ParseName(namespaceName))
                .AddMembers(visitorClass))
            .NormalizeWhitespace();
    }

    public static CompilationUnitSyntax GenerateGenericVisitorClass(List<SyntaxNodeModel> allNodes, bool isPublic = true, string namespaceName = "Raven.CodeAnalysis.Syntax")
    {
        var dispatchMethods = GetDispatchGroups(allNodes)
            .Select(group => BuildGenericVisitorDispatchMethod(group.Root, group.Cases))
            .ToArray();

        var methods = allNodes
            .Where(n => !n.IsAbstract)
            .Select(n =>
                MethodDeclaration(
                    IdentifierName("TResult"),
                    "Visit" + n.Name)
                .AddModifiers(Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.VirtualKeyword))
                .AddParameterListParameters(
                    Parameter(Identifier("node"))
                        .WithType(IdentifierName(n.Name + "Syntax")))
                .WithBody(Block(
                    ReturnStatement(
                        InvocationExpression(
                            IdentifierName("DefaultVisit"))
                        .WithArgumentList(ArgumentList(SingletonSeparatedList(
                            Argument(IdentifierName("node"))))))))
            ).ToArray();

        var visitorClass =
            ClassDeclaration("SyntaxVisitor")
                .AddModifiers(isPublic ? Token(SyntaxKind.PublicKeyword) : Token(SyntaxKind.InternalKeyword), Token(SyntaxKind.AbstractKeyword), Token(SyntaxKind.PartialKeyword))
                .AddTypeParameterListParameters(TypeParameter("TResult"))
                .AddMembers(dispatchMethods.Concat(methods).ToArray());

        return CompilationUnit()
            .AddUsings(UsingDirective(ParseName(namespaceName)))
            .AddMembers(NamespaceDeclaration(ParseName(namespaceName))
                .AddMembers(visitorClass))
            .NormalizeWhitespace();
    }

    public static CompilationUnitSyntax GenerateSyntaxRewriterClass(List<SyntaxNodeModel> allNodes, bool isPublic = true, string namespaceName = "Raven.CodeAnalysis.Syntax")
    {
        var dispatchMethods = GetDispatchGroups(allNodes)
            .Select(group => BuildRewriterDispatchMethod(group.Root, group.Cases))
            .ToArray();

        var methods = allNodes
            .Where(n => !n.IsAbstract)
            .Select(n =>
            {
                var visitArgs = new List<ArgumentSyntax>();

                if (n.HasExplicitKind)
                {
                    // Add: node.Kind
                    visitArgs.Add(Argument(
                        MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            IdentifierName("node"),
                            IdentifierName("Kind"))));
                }

                foreach (var prop in n.Slots)
                {
                    var memberAccess = MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        IdentifierName("node"),
                        IdentifierName(prop.Name));

                    bool b = namespaceName.Contains("InternalSyntax");

                    var visitMethodName = "Visit" + (b ? GetVisitorMethodNameGreen(prop.FullTypeName) : GetVisitorMethodName(prop.FullTypeName));
                    var visitInvocation = InvocationExpression(
                        IdentifierName(visitMethodName))
                        .WithArgumentList(ArgumentList(SingletonSeparatedList(Argument(memberAccess))));

                    // Decide if cast is needed
                    ExpressionSyntax visitedArg = visitMethodName switch
                    {
                        "VisitToken" or "VisitList" => visitInvocation,
                        _ => CastExpression(ParseTypeName((b ? MapGreenType(prop.FullTypeName, prop.IsNullable) : MapRedType(prop.FullTypeName, prop.IsNullable))), visitInvocation)
                    };

                    visitArgs.Add(Argument(visitedArg));
                }

                return MethodDeclaration(NullableType(IdentifierName("SyntaxNode")), "Visit" + n.Name)
                    .AddModifiers(Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.OverrideKeyword))
                    .AddParameterListParameters(
                        Parameter(Identifier("node"))
                            .WithType(IdentifierName(n.Name + "Syntax")))
                    .WithBody(Block(ReturnStatement(
                            InvocationExpression(
                                ConditionalAccessExpression(
                                    IdentifierName("node"),
                                    MemberBindingExpression(IdentifierName("Update"))))
                            .WithArgumentList(ArgumentList(SeparatedList(visitArgs))))));
            }).Cast<MemberDeclarationSyntax>().ToArray();

        var classDecl =
            ClassDeclaration("SyntaxRewriter")
                .AddModifiers(isPublic ? Token(SyntaxKind.PublicKeyword) : Token(SyntaxKind.InternalKeyword), Token(SyntaxKind.AbstractKeyword), Token(SyntaxKind.PartialKeyword))
                .WithBaseList(BaseList(SingletonSeparatedList<BaseTypeSyntax>(
                    SimpleBaseType(GenericName("SyntaxVisitor")
                        .WithTypeArgumentList(TypeArgumentList(SingletonSeparatedList<TypeSyntax>(
                            NullableType(IdentifierName("SyntaxNode")))))))))
                .AddMembers(dispatchMethods.Concat(methods).ToArray());

        return CompilationUnit()
            .AddUsings(UsingDirective(ParseName(namespaceName)))
            .AddMembers(NamespaceDeclaration(ParseName(namespaceName))
                .AddMembers(classDecl))
            .NormalizeWhitespace();
    }

    private static string MapRedType(string rawType, bool nullable) => rawType switch
    {
        "Token" => nullable ? "SyntaxToken?" : "SyntaxToken",
        "TokenList" => "SyntaxTokenList",
        var t when t.StartsWith("List<") => $"SyntaxList<{t[(t.IndexOf('<') + 1)..^1]}Syntax>",
        var t when t.StartsWith("SeparatedList<") => $"SeparatedList<{t[(t.IndexOf('<') + 1)..^1]}Syntax>",
        _ => rawType + "Syntax" + (nullable ? "?" : "")
    };

    private static string MapGreenType(string rawType, bool nullable) => rawType switch
    {
        "Token" => nullable ? "SyntaxToken?" : "SyntaxToken",
        "TokenList" => "SyntaxList",
        var t when t.StartsWith("List<") || t.StartsWith("SeparatedList<") => $"SyntaxList",
        _ => rawType + "Syntax" + (nullable ? "?" : "")
    };

    private static string GetVisitorMethodName(string type) => type switch
    {
        "Token" => type, // Tokens don't use Visit
        "TokenList" => "TokenList", // TokenLists don't either
        var t when t.StartsWith("List<") || t.StartsWith("SeparatedList<") => "List", // Lists skipped
        _ => type // Other nodes use their type
    };

    private static string GetVisitorMethodNameGreen(string type) => type switch
    {
        "Token" => type, // Tokens don't use Visit
        "TokenList" => "List", // TokenLists don't either
        var t when t.StartsWith("List<") || t.StartsWith("SeparatedList<") => "List", // Lists skipped
        _ => type // Other nodes use their type
    };
}

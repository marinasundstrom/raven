using NodesShared;

using Raven.Generators;

/// <summary>Complete generated Raven source set, separate from filesystem orchestration.</summary>
internal static class RavenSyntaxTreeGenerator
{
    public static Dictionary<string, string> Generate(List<SyntaxNodeModel> nodes,
        List<SyntaxHierarchyModel> hierarchies, List<FactoryDefinitionModel> factories,
        List<TokenKindModel> tokens, List<NodeKindModel> kinds)
    {
        var files = new Dictionary<string, string>(StringComparer.Ordinal);
        var definitions = factories.ToDictionary(f => f.Node, StringComparer.Ordinal);
        foreach (var node in nodes)
        {
            var subtypes = nodes.Where(n => n.Inherits == node.Name).Select(n => n.Name + "Syntax")
                .Concat(node.AdditionalPermittedTypes).ToArray();
            files.Add("generated/" + node.Name + "Syntax.g.rvn", RavenRedNodeGenerator.GenerateNode(node, subtypes));
            files.Add("InternalSyntax/generated/" + node.Name + "Syntax.g.rvn", RavenGreenNodeGenerator.GenerateNode(node));
            definitions.TryGetValue(node.Name, out var definition);
            var redFactory = RavenRedNodeGenerator.GenerateFactory(node, definition);
            if (redFactory is not null)
                files.Add("generated/" + node.Name + "Syntax.SyntaxFactory.g.rvn", redFactory);
            var greenFactory = RavenGreenNodeGenerator.GenerateFactory(node);
            if (greenFactory is not null)
                files.Add("InternalSyntax/generated/" + node.Name + "Syntax.SyntaxFactory.g.rvn", greenFactory);
        }
        foreach (var hierarchy in hierarchies)
        {
            var types = nodes.Where(n => n.Inherits == hierarchy.ModelBase).Select(n => n.Name + "Syntax")
                .Concat(hierarchy.AdditionalPermittedTypes).Select(t => "typeof(" + t + ")");
            files.Add("generated/" + hierarchy.Name + ".ClosedHierarchy.g.rvn",
                "namespace Raven.CodeAnalysis.Syntax\n[System.Runtime.CompilerServices.ClosedHierarchy(" +
                string.Join(", ", types) + ")]\npublic abstract partial class " + hierarchy.Name + " { }\n");
        }
        files.Add("generated/SyntaxKind.g.rvn", SyntaxKindGenerator.Generate(nodes, kinds, tokens, true));
        files.Add("generated/SyntaxFacts.Nodes.g.rvn", NodeFactsGenerator.GenerateSyntaxFacts(nodes, kinds, true));
        files.Add("generated/SyntaxFacts.Tokens.g.rvn", TokenGenerator.GenerateRavenSyntaxFacts(tokens));
        foreach (var green in new[] { false, true })
        {
            var directory = green ? "InternalSyntax/generated/" : "generated/";
            var ns = "Raven.CodeAnalysis.Syntax" + (green ? ".InternalSyntax" : "");
            files.Add(directory + "SyntaxFactory.Tokens.g.rvn", TokenGenerator.GenerateRavenFactory(tokens, green));
            files.Add(directory + "SyntaxVisitor.g.rvn", RavenVisitorGenerator.GenerateVisitor(nodes, false, !green, ns));
            files.Add(directory + "SyntaxVisitor`1.g.rvn", RavenVisitorGenerator.GenerateVisitor(nodes, true, !green, ns));
            files.Add(directory + "SyntaxRewriter.g.rvn", RavenRewriterGenerator.GenerateRewriter(nodes, !green, ns));
        }
        return files;
    }
}

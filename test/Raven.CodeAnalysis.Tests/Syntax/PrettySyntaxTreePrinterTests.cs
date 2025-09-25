using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class PrettySyntaxTreePrinterTests
{
    [Fact]
    public void ExpandListsAsPropertiesFalse_GroupsListProperties()
    {
        var tree = SyntaxTree.ParseText(
            """
class C {
    var x: int = 1
}
"""
        );

        var root = tree.GetRoot();

        var grouped = root.GetSyntaxTreeRepresentation(new PrinterOptions
        {
            IncludeTokens = false,
            IncludeNames = true,
            Colorize = false,
            ExpandListsAsProperties = false,
        });

        var groupedLines = grouped.Split(Environment.NewLine);
        Assert.Contains(groupedLines, line => line.TrimEnd().EndsWith("Members:"));

        var expanded = root.GetSyntaxTreeRepresentation(new PrinterOptions
        {
            IncludeTokens = false,
            IncludeNames = true,
            Colorize = false,
            ExpandListsAsProperties = true,
        });

        var expandedLines = expanded.Split(Environment.NewLine);
        Assert.DoesNotContain(expandedLines, line => line.TrimEnd().EndsWith("Members:"));
    }

    [Fact]
    public void ListPropertyLayout_ReturnsPlacementForFirstListItem()
    {
        var tree = SyntaxTree.ParseText(
            """
class C {
    var x: int = 1
    var y: int = 2
}
"""
        );

        var root = tree.GetRoot();
        var children = root.ChildNodesAndTokens().ToArray();

        var layout = ListPropertyLayout.Create(root, children, includeTokens: true);
        Assert.NotNull(layout);

        var classIndex = Array.FindIndex(children, child => child.TryGetNode(out var node) && node is ClassDeclarationSyntax);
        Assert.NotEqual(-1, classIndex);

        var classChild = (SyntaxNodeOrToken)children[classIndex];
        Assert.True(layout!.TryGetGroup(classChild, classIndex, out var group, out var isLast));
        Assert.Equal("Members", group.PropertyName);
        Assert.False(isLast);

        var endOfFileChild = (SyntaxNodeOrToken)children[^1];
        Assert.False(layout.TryGetGroup(endOfFileChild, children.Length - 1, out _, out _));
    }
}

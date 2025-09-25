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
        Assert.NotEqual(expanded, grouped);
    }

    [Fact]
    public void ExpandListsAsPropertiesFalse_DoesNotDuplicateGroupMembers()
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

        var grouped = root.GetSyntaxTreeRepresentation(new PrinterOptions
        {
            IncludeTokens = false,
            IncludeNames = true,
            Colorize = false,
            ExpandListsAsProperties = false,
        });

        var classNode = root.DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var classChildren = classNode.ChildNodesAndTokens().ToArray();
        var classLayout = ListPropertyLayout.Create(classNode, classChildren, includeTokens: false);
        Assert.NotNull(classLayout);

        var members = classNode.Members.ToArray();
        Assert.Equal(2, members.Length);

        int memberIndex0 = Array.FindIndex(classChildren, child => child.TryGetNode(out var node) && node == members[0]);
        int memberIndex1 = Array.FindIndex(classChildren, child => child.TryGetNode(out var node) && node == members[1]);

        Assert.True(memberIndex0 >= 0);
        Assert.True(memberIndex1 >= 0);

        var memberItem0 = (SyntaxNodeOrToken)classChildren[memberIndex0];
        var memberItem1 = (SyntaxNodeOrToken)classChildren[memberIndex1];

        Assert.True(classLayout!.TryStartGroup(memberItem0, memberIndex0, out var group, out var groupIsLast));
        Assert.False(groupIsLast);
        Assert.Equal("Members", group.PropertyName);
        Assert.Equal(2, group.Items.Count);

        Assert.False(classLayout.TryStartGroup(memberItem1, memberIndex1, out _, out _));
        Assert.True(classLayout.IsGroupedMember(memberItem1));

        var groupedLines = grouped.Split(Environment.NewLine);
        var disallowedPrefixes = group.Items
            .Select(item => item.Item.TryGetNode(out var node) ? node.Kind.ToString() : item.Item.AsToken().Kind.ToString())
            .SelectMany(kind => new[] { $"├── {kind}", $"└── {kind}" })
            .ToArray();

        Assert.All(disallowedPrefixes, prefix => Assert.DoesNotContain(groupedLines, line => line.StartsWith(prefix, StringComparison.Ordinal)));
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
        Assert.True(layout!.TryStartGroup(classChild, classIndex, out var group, out var isLast));
        Assert.True(layout.IsGroupedMember(classChild));
        Assert.Equal("Members", group.PropertyName);
        Assert.False(isLast);

        var endOfFileChild = (SyntaxNodeOrToken)children[^1];
        Assert.False(layout.TryStartGroup(endOfFileChild, children.Length - 1, out _, out _));
        Assert.False(layout.IsGroupedMember(endOfFileChild));
    }
}

using System;
using System.Collections.Generic;
using System.Linq;

namespace Raven.CodeAnalysis.Syntax;

internal sealed class ListPropertyLayout
{
    private readonly Dictionary<SyntaxNodeOrToken, SyntaxNodeReflectionExtensions.ChildGroup> _childToGroupMap;
    private readonly Dictionary<string, GroupPlacement> _groupPlacements;

    private ListPropertyLayout(
        Dictionary<SyntaxNodeOrToken, SyntaxNodeReflectionExtensions.ChildGroup> childToGroupMap,
        Dictionary<string, GroupPlacement> groupPlacements)
    {
        _childToGroupMap = childToGroupMap;
        _groupPlacements = groupPlacements;
    }

    public static ListPropertyLayout? Create(SyntaxNode node, ChildSyntaxListItem[] children, bool includeTokens)
    {
        if (children.Length == 0)
            return null;

        var grouped = node.GetChildrenGroupedByProperty(includeTokens);
        var listGroups = grouped.Properties.Where(static p => p.IsList).ToArray();
        if (listGroups.Length == 0)
            return null;

        var childToGroupMap = new Dictionary<SyntaxNodeOrToken, SyntaxNodeReflectionExtensions.ChildGroup>();
        var groupPlacements = new Dictionary<string, GroupPlacement>(StringComparer.Ordinal);

        var orderedChildren = children.Select(static c => (SyntaxNodeOrToken)c).ToArray();

        foreach (var group in listGroups)
        {
            int firstIndex = int.MaxValue;
            int lastIndex = -1;

            foreach (var item in group.Items)
            {
                var childItem = item.Item;
                childToGroupMap[childItem] = group;

                var index = Array.IndexOf(orderedChildren, childItem);
                if (index < 0)
                {
                    continue;
                }

                if (index < firstIndex)
                    firstIndex = index;

                if (index > lastIndex)
                    lastIndex = index;
            }

            if (firstIndex == int.MaxValue)
            {
                continue;
            }

            bool groupIsLast = lastIndex == orderedChildren.Length - 1;
            groupPlacements[group.PropertyName] = new GroupPlacement(firstIndex, groupIsLast);
        }

        if (groupPlacements.Count == 0)
            return null;

        return new ListPropertyLayout(childToGroupMap, groupPlacements);
    }

    public bool TryStartGroup(
        SyntaxNodeOrToken child,
        int index,
        out SyntaxNodeReflectionExtensions.ChildGroup group,
        out bool groupIsLast)
    {
        if (_childToGroupMap.TryGetValue(child, out group)
            && _groupPlacements.TryGetValue(group.PropertyName, out var placement)
            && placement.FirstIndex == index)
        {
            groupIsLast = placement.IsLast;
            return true;
        }

        group = default!;
        groupIsLast = default;
        return false;
    }

    public bool IsGroupedMember(SyntaxNodeOrToken child)
    {
        return _childToGroupMap.ContainsKey(child);
    }

    private readonly record struct GroupPlacement(int FirstIndex, bool IsLast);
}

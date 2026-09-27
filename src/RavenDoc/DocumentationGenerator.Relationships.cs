using System.Text;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

public static partial class DocumentationGenerator
{
    private static readonly Dictionary<string, INamedTypeSymbol> DocumentedTypes = new(StringComparer.Ordinal);
    private static readonly Dictionary<string, List<(INamedTypeSymbol Type, bool Direct)>> ReverseTypeRelationships = new(StringComparer.Ordinal);

    private static string TypeDefinitionId(ITypeSymbol type)
        => GetXrefId(type is INamedTypeSymbol named ? named.OriginalDefinition : type);

    private static void BuildReverseTypeRelationships()
    {
        foreach (var type in DocumentedTypes.Values)
        {
            var related = new Dictionary<string, bool>(StringComparer.Ordinal);
            for (var ancestor = type.BaseType; ancestor is not null; ancestor = ancestor.BaseType)
                related[TypeDefinitionId(ancestor)] = SymbolEqualityComparer.Default.Equals(ancestor, type.BaseType);
            foreach (var contract in type.AllInterfaces)
            {
                var id = TypeDefinitionId(contract);
                // Imported CLI symbols can expose the transitive interface closure in
                // Interfaces. Keep inherited contracts distinct from effective direct edges.
                var inheritedFromBase = type.BaseType?.AllInterfaces.Any(inherited =>
                    SymbolEqualityComparer.Default.Equals(inherited, contract)) == true;
                var inheritedFromInterface = type.Interfaces.Any(other =>
                    !SymbolEqualityComparer.Default.Equals(other, contract) &&
                    other.AllInterfaces.Any(inherited => SymbolEqualityComparer.Default.Equals(inherited, contract)));
                var direct = !inheritedFromBase && !inheritedFromInterface;
                related[id] = related.GetValueOrDefault(id) || direct;
            }
            foreach (var (id, direct) in related)
            {
                if (!DocumentedTypes.ContainsKey(id) || id == TypeDefinitionId(type))
                    continue;
                if (!ReverseTypeRelationships.TryGetValue(id, out var descendants))
                    ReverseTypeRelationships[id] = descendants = [];
                descendants.Add((type, direct));
            }
        }
    }

    private static IEnumerable<string> RenderReverseTypeRelationships(string directory, ITypeSymbol type)
    {
        if (!ReverseTypeRelationships.TryGetValue(TypeDefinitionId(type), out var descendants))
            yield break;
        foreach (var group in descendants.GroupBy(item => type.TypeKind == TypeKind.Interface
            ? item.Type.TypeKind == TypeKind.Interface ? "Derived interfaces" : "Implementing types"
            : "Derived types").OrderBy(group => group.Key, StringComparer.Ordinal))
        {
            var lines = new StringBuilder();
            lines.AppendLine($"## {group.Key}").AppendLine();
            lines.AppendLine("Documented types in this API reference. Indirect relationships are marked.").AppendLine();
            foreach (var item in group.OrderBy(item => GetTypeDocName(item.Type), StringComparer.Ordinal))
            {
                lines.Append("- ").Append(FormatTypeLink(directory, item.Type, ContainingTypeDisplayFormat));
                if (!item.Direct)
                    lines.Append(" (indirect)");
                lines.AppendLine();
            }
            yield return lines.ToString();
        }
    }

    private static IEnumerable<ISymbol> PublicRelatedMembers(ITypeSymbol type)
        => type.GetMembers().Where(member => member.DeclaredAccessibility == Accessibility.Public && !member.IsStatic &&
            member is not ITypeSymbol && member is not IMethodSymbol { IsConstructor: true } &&
            member is not IMethodSymbol { AssociatedSymbol: not null } &&
            !ExcludedMembers.Contains(GetXrefId(member).Replace('+', '.').Replace("..ctor", ".#ctor")) &&
            !IsCompilerGeneratedExtensionArtifact(member) && CanRenderSymbol(member));

    private static string MemberIdentity(ISymbol member)
    {
        string Parameters(IEnumerable<IParameterSymbol> parameters) => string.Join(",", parameters.Select(parameter =>
            parameter.RefKind + ":" + (parameter.Type is ITypeParameterSymbol argument ? "!" + argument.Ordinal : GetTypeName(parameter.Type))));
        return member.Kind + ":" + member.Name + (member switch
        {
            IMethodSymbol method => "`" + method.Arity + "(" + Parameters(method.Parameters) + ")",
            IPropertySymbol property => "(" + Parameters(property.Parameters) + ")",
            _ => ""
        });
    }

    private static IEnumerable<ISymbol> VisibleTypeMembers(ITypeSymbol type, IEnumerable<ISymbol> declared)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        foreach (var member in declared)
        {
            seen.Add(MemberIdentity(member));
            yield return member;
        }
        for (var ancestor = type.BaseType; ancestor is not null; ancestor = ancestor.BaseType)
            foreach (var member in PublicRelatedMembers(ancestor))
                if (seen.Add(MemberIdentity(member))) yield return member;
        foreach (var contract in type.AllInterfaces.OrderByDescending(contract => contract.AllInterfaces.Length))
            foreach (var member in PublicRelatedMembers(contract))
                if (seen.Add(MemberIdentity(member))) yield return member;
    }

    private static IEnumerable<string> ClosedHierarchyLines(string directory, ITypeSymbol type)
    {
        if (type is INamedTypeSymbol { IsSealedHierarchy: true } root)
        {
            yield return "**Closed hierarchy**: only the permitted direct subtypes can extend this type.<br />";
            if (root.PermittedDirectSubtypes.Length > 0)
                yield return "**Permitted direct subtypes**: " + string.Join(", ", root.PermittedDirectSubtypes
                    .Select(child => FormatTypeLink(directory, child, ContainingTypeDisplayFormat))) + "<br />";
        }
        var ancestors = GetInheritanceChain(type).Where(ancestor => !SymbolEqualityComparer.Default.Equals(ancestor, type))
            .Concat(type.AllInterfaces).OfType<INamedTypeSymbol>().Where(ancestor => ancestor.IsSealedHierarchy)
            .Distinct(SymbolEqualityComparer.Default).OfType<INamedTypeSymbol>();
        foreach (var ancestor in ancestors)
            yield return "**Part of closed hierarchy**: " + FormatTypeLink(directory, ancestor, ContainingTypeDisplayFormat) + "<br />";
    }

    private static string? InterfaceImplementationStatus(ISymbol member)
    {
        if (member.ContainingType?.TypeKind != TypeKind.Interface) return null;
        var accessors = member switch
        {
            IMethodSymbol method => new[] { (Name: "member", Method: (IMethodSymbol?)method) },
            IPropertySymbol property => new[] { (Name: "getter", Method: property.GetMethod), (Name: "setter", Method: property.SetMethod) },
            IEventSymbol @event => new[] { (Name: "add accessor", Method: @event.AddMethod), (Name: "remove accessor", Method: @event.RemoveMethod) },
            _ => []
        };
        var declared = accessors.Where(accessor => accessor.Method is not null).ToArray();
        if (declared.Length == 0) return null;
        var implemented = declared.Where(accessor => !accessor.Method!.IsAbstract).ToArray();
        if (implemented.Length == 0) return "Required implementation (no default).";
        if (implemented.Length == declared.Length) return "Default implementation provided.";
        return "Default implementation for " + string.Join(", ", implemented.Select(accessor => accessor.Name)) +
            "; implementation required for " + string.Join(", ", declared.Where(accessor => accessor.Method!.IsAbstract).Select(accessor => accessor.Name)) + ".";
    }

    private static string RenderMemberOrigins(string directory, ISymbol member, ITypeSymbol? context = null)
    {
        if (LogicalMemberOwner(member) is not { } owner || member is ITypeSymbol)
            return "";
        context ??= owner;
        var origins = new List<string>();
        if (member.GetExtensionReceiverType() is not null)
            return "<span class=\"member-origin\">Extension from " + MemberDefinitionLink(directory, member) + "</span>";
        if (!SymbolEqualityComparer.Default.Equals(owner, context))
        {
            origins.Add(owner.TypeKind == TypeKind.Interface && context.TypeKind != TypeKind.Interface
                ? InterfaceDispatchDescription(directory, context, member)
                : "Inherited from " + MemberDefinitionLink(directory, member));
        }
        else if (IsOverride(member))
        {
            for (var ancestor = owner.BaseType; ancestor is not null; ancestor = ancestor.BaseType)
            {
                var overridden = ancestor.GetMembers().FirstOrDefault(candidate => MemberIdentity(candidate) == MemberIdentity(member));
                if (overridden is null) continue;
                origins.Add("Overrides " + MemberDefinitionLink(directory, overridden));
                break;
            }
        }
        IEnumerable<ISymbol> definitions = owner.TypeKind == TypeKind.Interface ? [member] :
            context.AllInterfaces.SelectMany(contract => contract.GetMembers()).Where(contract => MatchesInterfaceMember(member, contract));
        var links = definitions.Distinct(SymbolEqualityComparer.Default)
            .Select(definition => MemberDefinitionLink(directory, definition)).ToArray();
        if (links.Length > 0)
            origins.Add((owner.TypeKind == TypeKind.Interface ? "Defined by " : IsOverride(member) ? "Overrides implementation of " : "Implements ") + string.Join(", ", links));
        if (InterfaceImplementationStatus(member) is { } status && owner.TypeKind == TypeKind.Interface && context.TypeKind == TypeKind.Interface)
            origins.Add(status);
        return origins.Count == 0 ? "" : "<span class=\"member-origin\">" + string.Join(" · ", origins) + "</span>";
    }

    private static string MemberDefinitionLink(string directory, ISymbol member)
    {
        var label = RavenDocSiteTemplate.Escape(GetTypeName(LogicalMemberOwner(member)!) + "." + member.Name);
        var id = NormalizeXrefIdForIndex(GetXrefId(member));
        return XrefToTargetPath.TryGetValue(id, out var target)
            ? $"<a href=\"{RavenDocSiteTemplate.Escape(RelLink(directory, target))}\">{label}</a>" : label;
    }

    private static bool IsOverride(ISymbol member) => member switch
    {
        IMethodSymbol method => !method.IsConstructor && method.IsOverride,
        IPropertySymbol property => property.GetMethod?.IsOverride == true || property.SetMethod?.IsOverride == true,
        IEventSymbol @event => @event.AddMethod?.IsOverride == true || @event.RemoveMethod?.IsOverride == true,
        _ => false
    };

    private static string InterfaceDispatchDescription(string directory, ITypeSymbol type, ISymbol contract)
    {
        for (ITypeSymbol? current = type; current is not null && current.TypeKind != TypeKind.Interface; current = current.BaseType)
        {
            var implementation = current.GetMembers().FirstOrDefault(candidate =>
                candidate is not IMethodSymbol { AssociatedSymbol: not null } && MatchesInterfaceMember(candidate, contract));
            if (implementation is null) continue;
            var state = IsOverride(implementation) ? "Overridden by " :
                SymbolEqualityComparer.Default.Equals(current, type) ? "Implemented by " : "Inherited implementation from ";
            return state + MemberDefinitionLink(directory, implementation);
        }
        var candidates = type.AllInterfaces.Concat(type.TypeKind == TypeKind.Interface && type is INamedTypeSymbol named ? [named] : [])
            .Where(candidate => SymbolEqualityComparer.Default.Equals(candidate, contract.ContainingType) ||
                candidate.AllInterfaces.Contains(contract.ContainingType!, SymbolEqualityComparer.Default))
            .SelectMany(candidate => candidate.GetMembers().Where(member => MatchesInterfaceMember(member, contract)))
            .ToArray();
        var mostSpecific = candidates.Where(candidate => !candidates.Any(other =>
            !SymbolEqualityComparer.Default.Equals(candidate.ContainingType, other.ContainingType) &&
            other.ContainingType!.AllInterfaces.Contains(candidate.ContainingType!, SymbolEqualityComparer.Default))).ToArray();
        if (mostSpecific.Length > 1) return "Multiple interface declarations; no unique default implementation.";
        var selected = mostSpecific.SingleOrDefault() ?? contract;
        var status = InterfaceImplementationStatus(selected);
        return status == "Default implementation provided."
            ? "Uses default implementation from " + MemberDefinitionLink(directory, selected)
            : (status ?? "Required implementation.") + " Defined by " + MemberDefinitionLink(directory, selected);
    }

    private static bool MatchesInterfaceMember(ISymbol member, ISymbol contract)
    {
        IEnumerable<ISymbol> explicitMembers = member switch
        {
            IMethodSymbol method => method.ExplicitInterfaceImplementations,
            IPropertySymbol property => property.ExplicitInterfaceImplementations,
            IEventSymbol @event => @event.ExplicitInterfaceImplementations,
            _ => []
        };
        if (explicitMembers.Contains(contract, SymbolEqualityComparer.Default)) return true;
        if (member.DeclaredAccessibility != Accessibility.Public || member.Name != contract.Name || member.IsStatic != contract.IsStatic) return false;
        bool TypeMatches(ITypeSymbol left, ITypeSymbol right) => SymbolEqualityComparer.Default.Equals(left, right) ||
            left is ITypeParameterSymbol a && right is ITypeParameterSymbol b && a.OwnerKind == b.OwnerKind && a.Ordinal == b.Ordinal;
        bool ParametersMatch(IEnumerable<IParameterSymbol> left, IEnumerable<IParameterSymbol> right)
            => left.Count() == right.Count() && left.Zip(right).All(pair => pair.First.RefKind == pair.Second.RefKind && TypeMatches(pair.First.Type, pair.Second.Type));
        return (member, contract) switch
        {
            (IMethodSymbol a, IMethodSymbol b) => a.Arity == b.Arity && TypeMatches(a.ReturnType, b.ReturnType) && ParametersMatch(a.Parameters, b.Parameters),
            (IPropertySymbol a, IPropertySymbol b) => TypeMatches(a.Type, b.Type) && ParametersMatch(a.Parameters, b.Parameters),
            (IEventSymbol a, IEventSymbol b) => TypeMatches(a.Type, b.Type),
            _ => false
        };
    }

    private static SemanticModel? ExtensionModel;

    private static void PrepareExtensionLookup(Compilation compilation)
    {
        ExtensionModel = null;
        if (CurrentSiteOptions.ExtensionNamespaces is not { Count: > 0 } namespaces)
            return;
        foreach (var name in namespaces)
            if (!System.Text.RegularExpressions.Regex.IsMatch(name, @"^[\p{L}_][\p{L}\p{N}_]*(\.[\p{L}_][\p{L}\p{N}_]*)*$"))
                throw new InvalidOperationException($"Invalid extension namespace: {name}");
        var tree = SyntaxTree.ParseText(string.Join("\n", namespaces.Select(name => $"import {name}.*")));
        var lookupCompilation = compilation.AddSyntaxTrees(tree);
        ExtensionModel = lookupCompilation.GetSemanticModel(tree);
    }

    private static IEnumerable<ISymbol> ApplicableExtensionMembers(ITypeSymbol type)
    {
        if (ExtensionModel is null) yield break;
        var result = ExtensionModel.LookupApplicableExtensionMembers(type);
        var members = result.InstanceMethods.Cast<ISymbol>().Concat(result.StaticMethods)
            .Concat(result.InstanceProperties).Concat(result.StaticProperties)
            .Where(member => member.DeclaredAccessibility == Accessibility.Public &&
                CurrentSiteOptions.ExtensionNamespaces!.Contains(GetNamespaceFullName(member.ContainingNamespace)) &&
                (CurrentSiteOptions.ExtensionMembers is not { Count: > 0 } selected || selected.Contains(GetXrefId(member))))
            .Distinct(SymbolEqualityComparer.Default);
        foreach (var member in members.Where(IsFromDocumentedAssembly))
            yield return member;
    }
}

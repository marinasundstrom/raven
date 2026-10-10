using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

// Shared source/PE accessibility policy. Metadata providers must retain assembly
// grants before native inputs can participate; missing grants never authorize access.
internal static class FriendAssemblyAccess
{
    internal const string AttributeName = "System.Runtime.CompilerServices.InternalsVisibleToAttribute";

    // Symbols are immutable compilation snapshots. Weak keys avoid retaining either
    // compilation through a process-wide cache; Lazy publishes one decision per pair.
    private static readonly ConditionalWeakTable<IAssemblySymbol, ConditionalWeakTable<IAssemblySymbol, Lazy<bool>>> Decisions = new();

    internal static bool IsGranted(IAssemblySymbol declaring, IAssemblySymbol requesting)
    {
        var decisions = Decisions.GetValue(declaring, static _ => new());
        return decisions.TryGetValue(requesting, out var decision)
            ? decision.Value
            : GetOrCreateDecision(decisions, declaring, requesting).Value;
    }

    private static Lazy<bool> GetOrCreateDecision(
        ConditionalWeakTable<IAssemblySymbol, Lazy<bool>> decisions,
        IAssemblySymbol declaring, IAssemblySymbol requesting)
        => decisions.GetValue(requesting, requester => new Lazy<bool>(() => ComputeGrant(declaring, requester)));

    private static bool ComputeGrant(IAssemblySymbol declaring, IAssemblySymbol requesting)
    {
        var declaringIdentity = declaring is PEAssemblySymbol pe ? pe.AccessIdentity : new AssemblyName { Name = declaring.Name };
        var requestingIdentity = requesting is PEAssemblySymbol requester ? requester.AccessIdentity : new AssemblyName { Name = requesting.Name };
        IEnumerable<string> grants = declaring is PEAssemblySymbol imported
            ? imported.FriendAssemblyNames
            : declaring.GetAttributes()
                .Where(attribute => attribute.AttributeClass.ToFullyQualifiedMetadataName() == AttributeName
                    && attribute.ConstructorArguments.Length == 1
                    && attribute.AttributeConstructor.Parameters.Length == 1
                    && attribute.AttributeConstructor.Parameters[0].Type.SpecialType == SpecialType.System_String)
                .Select(attribute => attribute.ConstructorArguments[0].Value)
                .OfType<string>();
        return grants.Any(grant => Matches(declaringIdentity, requestingIdentity, grant));
    }

    internal static bool Matches(AssemblyName declaring, AssemblyName requesting, string grant)
    {
        try
        {
            // Version, culture and tokens are not valid friend qualifiers. In
            // particular, never discard an unsupported qualifier and grant by name.
            if (!HasOnlyPublicKeyQualifier(grant))
                return false;
            var friend = new AssemblyName(grant);
            if (string.IsNullOrWhiteSpace(friend.Name)
                || !StringComparer.OrdinalIgnoreCase.Equals(friend.Name, requesting.Name))
                return false;
            var key = friend.GetPublicKey() ?? [];
            var declaringKey = declaring.GetPublicKey() ?? [];
            var requestingKey = requesting.GetPublicKey() ?? [];
            if (declaringKey.Length > 0 && key.Length == 0)
                return false;
            return key.AsSpan().SequenceEqual(requestingKey);
        }
        catch (Exception error) when (error is ArgumentException or System.IO.FileLoadException)
        {
            return false;
        }
    }
    private static bool HasOnlyPublicKeyQualifier(string grant)
    {
        var quote = '\0';
        var separator = -1;
        for (var index = 0; index < grant.Length; index++)
        {
            var character = grant[index];
            if (character == '\\')
            {
                index++;
                continue;
            }
            if (quote != '\0')
            {
                if (character == quote)
                    quote = '\0';
            }
            else if (character is '\'' or '"')
                quote = character;
            else if (character == ',')
            {
                if (separator >= 0)
                    return false;
                separator = index;
            }
        }
        if (separator < 0)
            return true;
        var qualifier = grant.AsSpan(separator + 1);
        var equals = qualifier.IndexOf('=');
        return equals >= 0 && qualifier[..equals].Trim().Equals("PublicKey", StringComparison.OrdinalIgnoreCase);
    }

}

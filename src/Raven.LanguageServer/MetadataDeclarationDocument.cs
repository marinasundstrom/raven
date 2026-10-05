using System.Collections.Concurrent;
using System.Security.Cryptography;
using System.Text;

using MediatR;

using OmniSharp.Extensions.JsonRpc;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;

namespace Raven.LanguageServer;

// Read-only signature presentation from compiler symbols. This is not recovered source.
internal static class MetadataDeclarationDocument
{
    private static readonly ConcurrentDictionary<string, string> Documents = new();
    internal static LocationOrLocationLink? Create(ISymbol symbol)
    {
        var owner = symbol as INamedTypeSymbol ?? symbol.ContainingType;
        if (owner is null || symbol.ContainingAssembly is null) return null;
        var lines = new List<string> { "// Metadata declarations; implementation bodies are unavailable.",
            "// Assembly: " + symbol.ContainingAssembly.Name };
        lines.AddRange(owner.ToDisplayString(SymbolDisplayFormat.RavenTooltipFormat).Split('\n'));
        lines.Add("");
        var selected = 2;
        foreach (var member in owner.GetMembers())
        {
            if (SymbolEqualityComparer.Default.Equals(member, symbol)) selected = lines.Count;
            lines.AddRange(member.ToDisplayString(SymbolDisplayFormat.RavenTooltipFormat).Split('\n'));
        }
        var text = string.Join("\n", lines) + "\n";
        var hash = Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(text)));
        var uri = "raven-metadata:/" + hash + "/" + Uri.EscapeDataString(owner.Name) + ".txt";
        // Bound presentation memory. Documents can always be reconstructed by another definition request.
        if (Documents.Count >= 256) Documents.Clear();
        Documents[uri] = text;
        return new OmniSharp.Extensions.LanguageServer.Protocol.Models.Location { Uri = DocumentUri.Parse(uri), Range = new(new(selected, 0), new(selected, lines[selected].Length)) };
    }
    internal static string? Get(DocumentUri uri) => Documents.GetValueOrDefault(uri.ToString());
}

[Method("raven/metadataDeclaration", Direction.ClientToServer)]
internal sealed record MetadataDeclarationParams : IRequest<string?>
{
    public required DocumentUri Uri { get; init; }
}
internal sealed class MetadataDeclarationHandler : IJsonRpcRequestHandler<MetadataDeclarationParams, string?>
{
    public Task<string?> Handle(MetadataDeclarationParams request, CancellationToken cancellationToken)
        => Task.FromResult(MetadataDeclarationDocument.Get(request.Uri));
}

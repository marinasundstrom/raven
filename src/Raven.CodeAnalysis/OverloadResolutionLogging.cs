using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Text;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public interface IOverloadResolutionLogger
{
    void Log(OverloadResolutionLogEntry entry);
}

public enum OverloadCandidateStatus
{
    Applicable,
    TypeInferenceFailed,
    InsufficientArguments,
    ArgumentMismatch,
}

public readonly record struct OverloadCandidateLog(
    IMethodSymbol OriginalMethod,
    IMethodSymbol? ConstructedMethod,
    OverloadCandidateStatus Status,
    int? Score,
    bool IsExtensionMethod,
    bool IsBest,
    bool IsAmbiguous);

public readonly record struct OverloadArgumentLog(
    string? Name,
    RefKind RefKind,
    ITypeSymbol? Type,
    SyntaxNode? Syntax);

public sealed record OverloadResolutionLogEntry(
    SyntaxNode? CallSyntax,
    ITypeSymbol? ReceiverType,
    ImmutableArray<OverloadArgumentLog> Arguments,
    ImmutableArray<OverloadCandidateLog> Candidates,
    IMethodSymbol? SelectedMethod,
    ImmutableArray<IMethodSymbol> AmbiguousCandidates);

public sealed class OverloadResolutionLog : IOverloadResolutionLogger, IDisposable
{
    private static readonly SymbolDisplayFormat Format = SymbolDisplayFormat.MinimallyQualifiedFormat
        .WithGenericsOptions(SymbolDisplayGenericsOptions.IncludeTypeParameters)
        .WithMemberOptions(SymbolDisplayMemberOptions.IncludeParameters | SymbolDisplayMemberOptions.IncludeContainingType)
        .WithParameterOptions(SymbolDisplayParameterOptions.IncludeType | SymbolDisplayParameterOptions.IncludeName);

    private readonly TextWriter _writer;
    private readonly bool _ownsWriter;

    public OverloadResolutionLog(TextWriter writer, bool ownsWriter)
    {
        _writer = TextWriter.Synchronized(writer);
        _ownsWriter = ownsWriter;
    }

    public void Log(OverloadResolutionLogEntry entry)
    {
        var builder = new StringBuilder();

        builder.AppendLine("# Overload resolution");

        if (entry.CallSyntax is { } syntax)
        {
            var span = syntax.GetLocation().GetLineSpan();
            builder.AppendLine($"Call: {syntax.Kind} @ {span.Path}({span.StartLinePosition.Line + 1},{span.StartLinePosition.Character + 1})");
        }

        if (entry.ReceiverType is { } receiverType)
            builder.AppendLine($"Receiver: {receiverType.ToDisplayString(Format)}");

        if (!entry.Arguments.IsDefaultOrEmpty)
        {
            builder.AppendLine("Arguments:");
            foreach (var argument in entry.Arguments)
            {
                var refPrefix = argument.RefKind == RefKind.None ? string.Empty : $"{argument.RefKind.ToString().ToLowerInvariant()} ";
                var name = string.IsNullOrEmpty(argument.Name) ? string.Empty : $"{argument.Name}: ";
                var type = argument.Type is null ? "<null>" : argument.Type.ToDisplayString(Format);
                builder.AppendLine($"  - {name}{refPrefix}{type}");
            }
        }

        if (!entry.Candidates.IsDefaultOrEmpty)
        {
            builder.AppendLine("Candidates:");
            foreach (var candidate in entry.Candidates)
            {
                var status = candidate.Status.ToString();
                if (candidate.IsAmbiguous)
                    status = $"Ambiguous/{status}";
                else if (candidate.IsBest)
                    status = $"Best/{status}";

                var method = candidate.ConstructedMethod ?? candidate.OriginalMethod;
                var signature = method.ToDisplayString(Format);
                var score = candidate.Score is null ? string.Empty : $", Score={candidate.Score}";
                var extensionTag = candidate.IsExtensionMethod ? " [extension]" : string.Empty;

                builder.AppendLine($"  - {signature}{extensionTag} [{status}{score}]");
            }
        }

        if (entry.SelectedMethod is not null)
            builder.AppendLine($"Selected: {entry.SelectedMethod.ToDisplayString(Format)}");

        if (!entry.AmbiguousCandidates.IsDefaultOrEmpty)
        {
            builder.AppendLine("Ambiguous set:");
            foreach (var method in entry.AmbiguousCandidates)
                builder.AppendLine($"  * {method.ToDisplayString(Format)}");
        }

        builder.AppendLine();

        lock (_writer)
        {
            _writer.Write(builder.ToString());
            _writer.Flush();
        }
    }

    public void Dispose()
    {
        if (_ownsWriter)
            _writer.Dispose();
    }
}

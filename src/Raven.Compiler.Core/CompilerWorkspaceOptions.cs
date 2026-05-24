using Raven.CodeAnalysis;

namespace Raven.Compiler.Core;

public sealed record CompilerWorkspaceOptions
{
    public required IReadOnlyList<string> InputPaths { get; init; }

    public string? TargetFramework { get; init; }

    public IReadOnlyList<string> AdditionalReferences { get; init; } = [];

    public OutputKind OutputKind { get; init; } = OutputKind.ConsoleApplication;

    public bool AllowUnsafe { get; init; }

    public bool AllowGlobalStatements { get; init; } = true;

    public bool AllowNamespaceMembers { get; init; } = true;

    public bool AllowNamespaceMemberImports { get; init; } = true;

    public bool? MembersPublicByDefault { get; init; } = true;

    public bool UseRuntimeAsync { get; init; }

    public bool EmbedCoreTypes { get; init; } = true;

    public bool EnableSuggestions { get; init; }

    public bool RestoreProjectReferences { get; init; } = true;
}


namespace Raven.CodeAnalysis;

/// <summary>
/// Mirrors Roslyn's <see cref="Microsoft.CodeAnalysis.WorkspaceChangeKind"/>.
/// </summary>
public enum WorkspaceChangeKind
{
    SolutionAdded,
    SolutionRemoved,
    SolutionChanged,
    SolutionCleared,
    SolutionReloaded,

    ProjectAdded,
    ProjectRemoved,
    ProjectChanged,
    ProjectReloaded,

    DocumentAdded,
    DocumentRemoved,
    DocumentChanged,
    DocumentReloaded,

    AdditionalDocumentAdded,
    AdditionalDocumentRemoved,
    AdditionalDocumentChanged,
    AdditionalDocumentReloaded
}

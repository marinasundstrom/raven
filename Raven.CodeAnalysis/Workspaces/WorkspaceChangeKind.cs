namespace Raven.CodeAnalysis;

public enum WorkspaceChangeKind
{
    SolutionChanged,
    SolutionAdded,
    SolutionRemoved,
    SolutionCleared,
    SolutionReloaded,
    ProjectAdded,
    ProjectRemoved,
    ProjectChanged,
    ProjectReloaded,
    DocumentAdded,
    DocumentRemoved,
    DocumentReloaded,
    DocumentChanged,
    AdditionalDocumentAdded,
    AdditionalDocumentRemoved,
    AdditionalDocumentReloaded,
    AdditionalDocumentChanged,

    // Add more

}

namespace Raven.CodeAnalysis;

public class SolutionInfo
{
    internal SolutionAttributes Attributes { get; }

    /// <summary>
    /// The unique Id of the solution.
    /// </summary>
    public SolutionId Id => Attributes.Id;

    /// <summary>
    /// The version of the solution.
    /// </summary>
    public VersionStamp Version => Attributes.Version;

    /// <summary>
    /// The path to the solution file, or null if there is no solution file.
    /// </summary>
    public string? FilePath => Attributes.FilePath;

    /// <summary>
    /// A list of projects initially associated with the solution.
    /// </summary>
    public IReadOnlyList<ProjectInfo> Projects { get; }

    public SolutionInfo(
        SolutionAttributes attributes,
        IReadOnlyList<ProjectInfo> projects)
    {
        Attributes = attributes;
        Projects = projects;
    }
}

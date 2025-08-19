
using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

/// <summary>Immutable root metadata object describing a Solution (set of projects).</summary>
public sealed class SolutionInfo
{
    public SolutionInfo(SolutionAttributes attributes,
                        IEnumerable<ProjectInfo> projects)
    {
        Attributes = attributes;
        Projects = projects.ToImmutableArray();
    }

    public SolutionAttributes Attributes { get; }

    public SolutionId Id => Attributes.Id;
    public string FilePath => Attributes.FilePath;
    public VersionStamp Version => Attributes.Version;

    public ImmutableArray<ProjectInfo> Projects { get; }

    public SolutionInfo WithProjects(IEnumerable<ProjectInfo> projects) =>
        new(Attributes, projects);

    public SolutionInfo WithVersion(VersionStamp version) =>
        new(new SolutionAttributes(Id, FilePath, version), Projects);

    public sealed record SolutionAttributes(SolutionId Id,
                                            string FilePath,
                                            VersionStamp Version);
}

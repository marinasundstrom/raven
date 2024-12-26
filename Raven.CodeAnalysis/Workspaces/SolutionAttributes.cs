namespace Raven.CodeAnalysis;

public class SolutionAttributes
{
    public SolutionAttributes(
        SolutionId id,
        VersionStamp version,
        string? filePath)
    {
        Id = id;
        Version = version;
        FilePath = filePath;
    }

    public SolutionId Id { get; internal set; }
    public VersionStamp Version { get; internal set; }
    public string? FilePath { get; internal set; }
}

namespace Raven.CodeAnalysis;

public class ProjectAttributes
{
    internal ProjectAttributes(
        ProjectId id,
        VersionStamp version,
        string name,
        string assemblyName,
        string language,
        /*CompilationOutputInfo compilationOutputInfo,
        SourceHashAlgorithm checksumAlgorithm,
        string? defaultNamespace = null, */
        string? filePath = null
        /* string? outputFilePath = null,
        string? outputRefFilePath = null,
        Guid telemetryId = default,
        bool isSubmission = false,
        bool hasAllInformation = true,
        bool runAnalyzers = true,
        bool hasSdkCodeStyleAnalyzers = false */)
    {
        Id = id;
        Version = version;
        Name = name;
        AssemblyName = assemblyName;
        Language = language;
        FilePath = filePath;
    }

    public ProjectId Id { get; }
    public VersionStamp Version { get; }
    public string Name { get; }
    public string AssemblyName { get; }
    public string Language { get; }
    public string? FilePath { get; }

    internal ProjectAttributes With(
           ProjectId id,
           VersionStamp version,
           string name,
           string assemblyName,
           string language,
           string? filePath = null
       )
    {
        return new ProjectAttributes(
            id,
            version,
            name,
            assemblyName,
            language,
            filePath);
    }
}

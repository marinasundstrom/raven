namespace Raven.CodeAnalysis;

/// <summary>Selects a coherent semantic-data loader, runtime contract and code generator.</summary>
/// <remarks>Reference frameworks and core assemblies remain separate, explicit inputs.</remarks>
public enum TargetPlatform
{
    /// <summary>The currently supported CLI metadata and .NET emission pipeline.</summary>
    DotNet = 0,

    /// <summary>The experimental neoCLR CLI metadata/emission bridge, requiring NeoCLR.CoreProbe references.</summary>
    NeoCLR = 1,
}

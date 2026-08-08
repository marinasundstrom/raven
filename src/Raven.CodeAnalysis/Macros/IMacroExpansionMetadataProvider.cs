namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Marks a generated macro adapter whose expansion result carries editor metadata.
/// </summary>
/// <remarks>
/// Macro authors use source contribution statements rather than implementing this
/// adapter-facing marker directly.
/// </remarks>
public interface IMacroExpansionMetadataProvider
{
}

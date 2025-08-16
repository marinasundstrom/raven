using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

/// <summary>
/// Holds a <see cref="SourceText"/> together with its <see cref="VersionStamp"/>.
/// </summary>
internal sealed record TextAndVersion(SourceText Text, VersionStamp Version);


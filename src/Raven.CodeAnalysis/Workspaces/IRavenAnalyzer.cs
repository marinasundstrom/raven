using System.Collections.Generic;
using System.Threading;

namespace Raven.CodeAnalysis;

/// <summary>Defines an analyzer that can produce diagnostics for a compilation.</summary>
public interface IRavenAnalyzer
{
    IEnumerable<Diagnostic> Analyze(Compilation compilation, CancellationToken cancellationToken = default);
}

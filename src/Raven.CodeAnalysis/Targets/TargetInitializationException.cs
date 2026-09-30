using System;

namespace Raven.CodeAnalysis.Targets;

// Marks an expected failure to establish the target universe. Diagnostic/emit
// entry points translate this failure; unrelated compiler exceptions propagate.
internal sealed class TargetInitializationException(string message, Exception? innerException = null)
    : Exception(message, innerException);

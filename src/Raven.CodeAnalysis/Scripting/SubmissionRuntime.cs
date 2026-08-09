using System;
using System.ComponentModel;
using System.Threading;

namespace Raven.CodeAnalysis.Scripting;

/// <summary>
/// Provides the ambient value storage used by compiled Raven script submissions.
/// </summary>
/// <remarks>
/// This type is public because emitted submissions call it directly. Hosts should
/// use the higher-level scripting APIs instead.
/// </remarks>
[EditorBrowsable(EditorBrowsableState.Never)]
public static class SubmissionRuntime
{
    private static readonly AsyncLocal<object?[]?> s_currentVariables = new();

    /// <summary>
    /// Enters an execution scope backed by the specified submission variables.
    /// </summary>
    [EditorBrowsable(EditorBrowsableState.Never)]
    public static IDisposable Enter(object?[] variables)
    {
        ArgumentNullException.ThrowIfNull(variables);
        var previous = s_currentVariables.Value;
        s_currentVariables.Value = variables;
        return new Scope(previous);
    }

    /// <summary>
    /// Gets a typed submission variable from the current execution scope.
    /// </summary>
    [EditorBrowsable(EditorBrowsableState.Never)]
    public static T Get<T>(int slot)
        => (T)GetVariables()[slot]!;

    /// <summary>
    /// Stores a typed submission variable in the current execution scope.
    /// </summary>
    [EditorBrowsable(EditorBrowsableState.Never)]
    public static void Set<T>(int slot, T value)
        => GetVariables()[slot] = value;

    private static object?[] GetVariables()
        => s_currentVariables.Value
            ?? throw new InvalidOperationException("No Raven script submission is currently executing.");

    private sealed class Scope(object?[]? previous) : IDisposable
    {
        private object?[]? _previous = previous;
        private bool _disposed;

        public void Dispose()
        {
            if (_disposed)
                return;

            s_currentVariables.Value = _previous;
            _previous = null;
            _disposed = true;
        }
    }
}

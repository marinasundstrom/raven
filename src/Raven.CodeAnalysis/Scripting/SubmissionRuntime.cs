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
    private static readonly AsyncLocal<SubmissionContext?> s_currentContext = new();

    /// <summary>
    /// Enters an execution scope backed by the specified submission variables.
    /// </summary>
    [EditorBrowsable(EditorBrowsableState.Never)]
    public static IDisposable Enter(object?[] variables)
    {
        ArgumentNullException.ThrowIfNull(variables);
        var previous = s_currentContext.Value;
        s_currentContext.Value = new SubmissionContext(variables);
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

    /// <summary>
    /// Stores the value produced by the trailing expression of a submission.
    /// </summary>
    [EditorBrowsable(EditorBrowsableState.Never)]
    public static void SetResult<T>(T value)
    {
        var context = GetContext();
        context.Result = value;
        context.HasResult = true;
    }

    internal static bool TryGetResult(out object? result)
    {
        var context = GetContext();
        result = context.Result;
        return context.HasResult;
    }

    private static object?[] GetVariables()
        => GetContext().Variables;

    private static SubmissionContext GetContext()
        => s_currentContext.Value
            ?? throw new InvalidOperationException("No Raven script submission is currently executing.");

    private sealed class SubmissionContext(object?[] variables)
    {
        internal object?[] Variables { get; } = variables;
        internal bool HasResult { get; set; }
        internal object? Result { get; set; }
    }

    private sealed class Scope(SubmissionContext? previous) : IDisposable
    {
        private SubmissionContext? _previous = previous;
        private bool _disposed;

        public void Dispose()
        {
            if (_disposed)
                return;

            s_currentContext.Value = _previous;
            _previous = null;
            _disposed = true;
        }
    }
}

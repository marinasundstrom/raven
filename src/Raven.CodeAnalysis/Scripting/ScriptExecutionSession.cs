using System.Reflection;
using System.Runtime.ExceptionServices;
using System.Runtime.Loader;

namespace Raven.CodeAnalysis.Scripting;

internal sealed class ScriptExecutionSession : IDisposable
{
    private readonly SemaphoreSlim _executionGate = new(1, 1);
    private readonly SubmissionLoadContext _loadContext = new();
    private readonly string _temporaryDirectory = Path.Combine(
        Path.GetTempPath(),
        $"raven-script-{Guid.NewGuid():N}");
    private object?[] _variables = [];
    private ScriptState? _latestState;
    private bool _disposed;

    internal ScriptExecutionSession(ScriptOptions options)
    {
        Options = options;
        Directory.CreateDirectory(_temporaryDirectory);
    }

    internal ScriptOptions Options { get; }

    internal async Task<ScriptState> ExecuteAsync(
        Script script,
        ScriptState? previousState,
        CancellationToken cancellationToken)
    {
        ObjectDisposedException.ThrowIf(_disposed, this);
        await _executionGate.WaitAsync(cancellationToken).ConfigureAwait(false);
        try
        {
            ObjectDisposedException.ThrowIf(_disposed, this);
            if (!ReferenceEquals(previousState, _latestState))
            {
                throw new InvalidOperationException(
                    "A Raven script session can only continue from its latest state.");
            }

            var compilation = script.CreateCompilation(
                previousState?.Compilation,
                previousState?.EmittedReference);
            using var peStream = new MemoryStream();
            cancellationToken.ThrowIfCancellationRequested();
            var emitResult = compilation.Emit(peStream);
            if (!emitResult.Success)
                throw new RavenCompilationException(emitResult.Diagnostics);

            var image = peStream.ToArray();
            var imagePath = Path.Combine(_temporaryDirectory, $"{compilation.AssemblyName}.dll");
            var emittedReference = MetadataReference.CreateFromImage(image, imagePath);
            var assembly = _loadContext.Load(image);

            if (_variables.Length < compilation.SubmissionVariableCount)
                Array.Resize(ref _variables, compilation.SubmissionVariableCount);

            object? returnValue;
            bool hasReturnValue;
            using (SubmissionRuntime.Enter(_variables))
            {
                await InvokeEntryPointAsync(assembly).ConfigureAwait(false);
                hasReturnValue = SubmissionRuntime.TryGetResult(out returnValue);
            }

            var state = new ScriptState(
                this,
                script,
                compilation,
                emittedReference,
                hasReturnValue,
                returnValue);
            _latestState = state;
            return state;
        }
        finally
        {
            _executionGate.Release();
        }
    }

    public void Dispose()
    {
        if (_disposed)
            return;

        _disposed = true;
        _loadContext.Unload();
        try
        {
            Directory.Delete(_temporaryDirectory, recursive: true);
        }
        catch (IOException)
        {
        }
        catch (UnauthorizedAccessException)
        {
        }
    }

    private static async Task InvokeEntryPointAsync(Assembly assembly)
    {
        var entryPoint = assembly.EntryPoint;
        if (entryPoint is null)
            return;

        object? result;
        try
        {
            var arguments = entryPoint.GetParameters().Length == 0
                ? null
                : new object?[] { Array.Empty<string>() };
            result = entryPoint.Invoke(null, arguments);
        }
        catch (TargetInvocationException exception) when (exception.InnerException is not null)
        {
            ExceptionDispatchInfo.Capture(exception.InnerException).Throw();
            throw;
        }

        if (result is Task task)
            await task.ConfigureAwait(false);
    }

    private sealed class SubmissionLoadContext : AssemblyLoadContext
    {
        internal SubmissionLoadContext()
            : base($"RavenScript-{Guid.NewGuid():N}", isCollectible: true)
        {
        }

        protected override Assembly? Load(AssemblyName assemblyName)
        {
            if (assemblyName.Name == typeof(SubmissionRuntime).Assembly.GetName().Name)
                return typeof(SubmissionRuntime).Assembly;

            var submissionAssembly = Assemblies.FirstOrDefault(assembly =>
                AssemblyName.ReferenceMatchesDefinition(assembly.GetName(), assemblyName));
            if (submissionAssembly is not null)
                return submissionAssembly;

            return AppDomain.CurrentDomain.GetAssemblies().FirstOrDefault(assembly =>
                !assembly.IsDynamic &&
                AssemblyName.ReferenceMatchesDefinition(assembly.GetName(), assemblyName));
        }

        internal Assembly Load(byte[] image)
        {
            using var stream = new MemoryStream(image);
            return LoadFromStream(stream);
        }
    }
}

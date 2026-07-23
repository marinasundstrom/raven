using System.Reflection;

namespace Raven.Playground.Services;

public sealed class PlaygroundProgramRunner
{
    private static readonly SemaphoreSlim s_consoleGate = new(1, 1);

    public async Task<PlaygroundExecutionResult> RunAsync(byte[] assemblyImage)
    {
        await s_consoleGate.WaitAsync();
        try
        {
            var assembly = Assembly.Load(assemblyImage);
            var entryPoint = assembly.EntryPoint
                ?? throw new InvalidOperationException("The emitted Raven assembly does not have an entry point.");

            var originalOut = Console.Out;
            var originalError = Console.Error;
            using var output = new StringWriter();
            Console.SetOut(output);
            Console.SetError(output);

            try
            {
                var arguments = entryPoint.GetParameters().Length switch
                {
                    0 => null,
                    1 => new object?[] { Array.Empty<string>() },
                    _ => throw new InvalidOperationException("The emitted Raven entry point has an unsupported signature."),
                };

                var invocationResult = entryPoint.Invoke(null, arguments);
                var exitCode = invocationResult switch
                {
                    Task<int> exitCodeTask => await exitCodeTask,
                    Task task => await AwaitTask(task),
                    int value => value,
                    _ => 0,
                };

                return new PlaygroundExecutionResult(exitCode, output.ToString());
            }
            catch (TargetInvocationException exception) when (exception.InnerException is not null)
            {
                throw exception.InnerException;
            }
            finally
            {
                Console.SetOut(originalOut);
                Console.SetError(originalError);
            }
        }
        finally
        {
            s_consoleGate.Release();
        }
    }

    private static async Task<int> AwaitTask(Task task)
    {
        await task;
        return 0;
    }
}

public sealed record PlaygroundExecutionResult(int ExitCode, string Output);

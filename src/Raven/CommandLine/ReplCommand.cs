using System.Text;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Scripting;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CommandLine;

internal static class ReplCommand
{
    internal static async Task<int> ExecuteAsync(string[] args)
    {
        if (args.Length != 1)
        {
            Console.Error.WriteLine("Usage: rvn repl");
            return 1;
        }

        Console.WriteLine("Raven interactive mode. Type :help for commands.");
        ScriptState? state = null;
        var buffer = new StringBuilder();

        try
        {
            while (true)
            {
                Console.Write(buffer.Length == 0 ? "> " : ". ");
                var line = Console.ReadLine();
                if (line is null)
                    return 0;

                if (buffer.Length == 0 && line.StartsWith(':'))
                {
                    var commandResult = await ExecuteCommandAsync(line, state).ConfigureAwait(false);
                    state = commandResult.State;
                    if (commandResult.ShouldQuit)
                        return 0;

                    continue;
                }

                if (buffer.Length > 0)
                    buffer.AppendLine();
                buffer.Append(line);

                var code = buffer.ToString();
                var tree = SyntaxTree.ParseText(
                    code,
                    new ParseOptions { Kind = SourceCodeKind.Interactive },
                    path: "<interactive>");
                if (tree.GetSubmissionCompleteness() == SubmissionCompleteness.Incomplete)
                    continue;

                buffer.Clear();
                state = await ExecuteSubmissionAsync(code, state).ConfigureAwait(false);
            }
        }
        finally
        {
            state?.Dispose();
        }
    }

    private static async Task<CommandResult> ExecuteCommandAsync(
        string input,
        ScriptState? state)
    {
        var command = input.Trim();
        if (command is ":quit" or ":q")
            return new CommandResult(state, ShouldQuit: true);

        if (command == ":reset")
        {
            state?.Dispose();
            Console.WriteLine("State reset.");
            return new CommandResult(null, ShouldQuit: false);
        }

        if (command is ":help" or ":h")
        {
            PrintHelp();
            return new CommandResult(state, ShouldQuit: false);
        }

        if (command == ":references")
        {
            foreach (var reference in ScriptOptions.Default.MetadataReferences.OfType<PortableExecutableReference>())
                Console.WriteLine(reference.FilePath);
            return new CommandResult(state, ShouldQuit: false);
        }

        const string loadPrefix = ":load ";
        if (command.StartsWith(loadPrefix, StringComparison.Ordinal))
        {
            var path = command[loadPrefix.Length..].Trim();
            if (path.Length == 0 || !File.Exists(path))
            {
                Console.Error.WriteLine($"Raven source file '{path}' does not exist.");
                return new CommandResult(state, ShouldQuit: false);
            }

            var code = await File.ReadAllTextAsync(path).ConfigureAwait(false);
            var nextState = await ExecuteSubmissionAsync(code, state).ConfigureAwait(false);
            return new CommandResult(nextState, ShouldQuit: false);
        }

        Console.Error.WriteLine($"Unknown interactive command '{command}'. Type :help for commands.");
        return new CommandResult(state, ShouldQuit: false);
    }

    private static async Task<ScriptState?> ExecuteSubmissionAsync(
        string code,
        ScriptState? state)
    {
        if (string.IsNullOrWhiteSpace(code))
            return state;

        try
        {
            var nextState = state is null
                ? await RavenScript.RunAsync(code).ConfigureAwait(false)
                : await state.ContinueWithAsync(code).ConfigureAwait(false);
            ScriptCommand.WriteResult(nextState);
            return nextState;
        }
        catch (RavenCompilationException exception)
        {
            ScriptCommand.WriteDiagnostics(exception.Diagnostics);
            return state;
        }
        catch (Exception exception)
        {
            Console.Error.WriteLine(exception.Message);
            return state;
        }
    }

    private static void PrintHelp()
    {
        Console.WriteLine(":load <file>  Execute a Raven source file in the current state.");
        Console.WriteLine(":reset        Clear variables and declarations.");
        Console.WriteLine(":references   List active metadata references.");
        Console.WriteLine(":quit         Exit interactive mode.");
    }

    private sealed record CommandResult(ScriptState? State, bool ShouldQuit);
}

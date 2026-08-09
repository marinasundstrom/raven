using System.Diagnostics;

using Raven.CodeAnalysis;

namespace Raven.CommandLine;

internal static class FileApplicationCommand
{
    public static bool TryFindSourceArgument(string[] args, int startIndex, out int sourceArgumentIndex)
    {
        var separatorIndex = Array.IndexOf(args, "--", startIndex);
        var endIndex = separatorIndex >= 0 ? separatorIndex : args.Length;

        for (var i = startIndex; i < endIndex; i++)
        {
            if (RavenFileExtensions.HasRavenExtension(args[i]))
            {
                sourceArgumentIndex = i;
                return true;
            }
        }

        sourceArgumentIndex = -1;
        return false;
    }

    public static int Execute(string[] args, int sourceArgumentIndex)
    {
        var sourcePath = Path.GetFullPath(args[sourceArgumentIndex]);
        if (!File.Exists(sourcePath))
        {
            Console.Error.WriteLine($"Raven source file '{sourcePath}' does not exist.");
            return 1;
        }

        var compilerPath = SdkLocator.TryFindCompilerDriverPath();
        if (compilerPath is null)
        {
            Console.Error.WriteLine("Unable to locate the Raven compiler driver (rvnc). Reinstall the Raven SDK or build src/Raven.Compiler.");
            return 1;
        }

        if (!TryParseArguments(args, sourceArgumentIndex, out var compilerArguments, out var applicationArguments))
            return 1;

        var outputDirectory = Path.Combine(Path.GetTempPath(), "raven", "run", Guid.NewGuid().ToString("N"));
        var outputPath = Path.Combine(outputDirectory, $"{Path.GetFileNameWithoutExtension(sourcePath)}.dll");
        Directory.CreateDirectory(outputDirectory);

        try
        {
            var startInfo = new ProcessStartInfo("dotnet")
            {
                UseShellExecute = false
            };
            startInfo.Environment["RAVEN_FRONTEND_INVOCATION"] = "1";
            startInfo.ArgumentList.Add(compilerPath);
            foreach (var argument in compilerArguments)
                startInfo.ArgumentList.Add(argument);
            startInfo.ArgumentList.Add("--run");
            startInfo.ArgumentList.Add("--output");
            startInfo.ArgumentList.Add(outputPath);
            startInfo.ArgumentList.Add(sourcePath);

            if (applicationArguments.Count > 0)
            {
                startInfo.ArgumentList.Add("--");
                foreach (var argument in applicationArguments)
                    startInfo.ArgumentList.Add(argument);
            }

            using var process = Process.Start(startInfo);
            if (process is null)
            {
                Console.Error.WriteLine("Failed to start the Raven compiler.");
                return 1;
            }

            process.WaitForExit();
            return process.ExitCode;
        }
        catch (Exception ex) when (ex is InvalidOperationException or System.ComponentModel.Win32Exception)
        {
            Console.Error.WriteLine($"Failed to start the Raven compiler: {ex.Message}");
            return 1;
        }
        finally
        {
            TryDeleteDirectory(outputDirectory);
        }
    }

    private static bool TryParseArguments(
        string[] args,
        int sourceArgumentIndex,
        out IReadOnlyList<string> compilerArguments,
        out IReadOnlyList<string> applicationArguments)
    {
        var separatorIndex = Array.IndexOf(args, "--");
        var toolArguments = new List<string>();
        const int toolStartIndex = 1;
        var toolEndIndex = sourceArgumentIndex == 0
            ? 1
            : separatorIndex >= 0 ? separatorIndex : args.Length;

        for (var i = toolStartIndex; i < toolEndIndex; i++)
        {
            if (i == sourceArgumentIndex)
                continue;

            if (RavenFileExtensions.HasRavenExtension(args[i]))
            {
                Console.Error.WriteLine("File-based applications accept exactly one Raven source file.");
                compilerArguments = [];
                applicationArguments = [];
                return false;
            }

            toolArguments.Add(args[i]);
        }

        compilerArguments = toolArguments;
        applicationArguments = sourceArgumentIndex == 0
            ? args.Skip(args.Length > 1 && args[1] == "--" ? 2 : 1).ToArray()
            : separatorIndex >= 0
                ? args.Skip(separatorIndex + 1).ToArray()
                : [];
        return true;
    }

    private static void TryDeleteDirectory(string path)
    {
        try
        {
            Directory.Delete(path, recursive: true);
        }
        catch (IOException)
        {
        }
        catch (UnauthorizedAccessException)
        {
        }
    }
}

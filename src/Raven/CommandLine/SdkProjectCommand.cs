using System.Diagnostics;
using System.Text;
using System.Text.Json;

using Raven.CodeAnalysis;

namespace Raven.CommandLine;

internal static class SdkProjectCommand
{
    public static int Execute(string commandName, string[] args)
    {
        if (args.Length > 1 && IsHelp(args[1]))
        {
            PrintHelp(commandName);
            return 0;
        }

        if (!TryParse(commandName, args, out var projectFilePath, out var forwardedArgs))
            return 1;

        var dotnetArgs = new List<string> { commandName };
        if (commandName == "run")
        {
            dotnetArgs.Add("--project");
            dotnetArgs.Add(projectFilePath);
        }
        else
        {
            dotnetArgs.Add(projectFilePath);
        }

        if (SdkLocator.TryFindRoot() is { } sdkRoot)
        {
            var languageTargets = Path.Combine(sdkRoot, "sdk", "build", "Raven.Language.targets");
            if (File.Exists(languageTargets))
                dotnetArgs.Add($"-p:LanguageTargets={languageTargets}");
        }

        dotnetArgs.AddRange(forwardedArgs);
        return RunDotnet(dotnetArgs);
    }

    private static bool TryParse(
        string commandName,
        string[] args,
        out string projectFilePath,
        out IReadOnlyList<string> forwardedArgs)
    {
        string? project = null;
        var rest = new List<string>();
        var externalConstants = new Dictionary<string, string>(StringComparer.Ordinal);

        for (var i = 1; i < args.Length; i++)
        {
            var arg = args[i];
            if (arg == "--")
            {
                rest.AddRange(args.Skip(i));
                break;
            }

            if (commandName is "build" or "run" && arg == "--constant")
            {
                if (i + 1 >= args.Length || !TryParseExternalConstant(args[++i], externalConstants))
                {
                    projectFilePath = string.Empty;
                    forwardedArgs = [];
                    return false;
                }

                continue;
            }

            if (project is null && !arg.StartsWith('-') && IsRavenProjectPath(arg))
            {
                project = arg;
                continue;
            }

            rest.Add(arg);
        }

        project ??= TryFindDefaultProjectFile();
        if (project is null)
        {
            Console.Error.WriteLine("No Raven project file was specified and the current directory does not contain exactly one .rvnproj file.");
            Console.Error.WriteLine("Usage: rvn build [project.rvnproj] [dotnet-build-options]");
            Console.Error.WriteLine("       rvn run [project.rvnproj] [dotnet-run-options] [-- application-args]");
            Console.Error.WriteLine("       rvn clean [project.rvnproj] [dotnet-clean-options]");
            projectFilePath = string.Empty;
            forwardedArgs = [];
            return false;
        }

        projectFilePath = Path.GetFullPath(project);
        if (externalConstants.Count > 0)
        {
            var json = JsonSerializer.Serialize(externalConstants);
            var payload = Convert.ToBase64String(Encoding.UTF8.GetBytes(json));
            rest.Add($"-p:RavenExternalConstantOverrides={payload}");
        }

        forwardedArgs = rest;
        return true;
    }

    private static bool TryParseExternalConstant(string specification, IDictionary<string, string> values)
    {
        var separator = specification.IndexOf('=');
        if (separator <= 0)
        {
            Console.Error.WriteLine($"Invalid external constant '{specification}'. Expected NAME=VALUE.");
            return false;
        }

        var name = specification[..separator].Trim();
        if (name.Length == 0)
        {
            Console.Error.WriteLine("External constant names cannot be empty.");
            return false;
        }

        values[name] = specification[(separator + 1)..];
        return true;
    }

    private static bool IsRavenProjectPath(string path)
    {
        var extension = Path.GetExtension(path);
        return string.Equals(extension, RavenFileExtensions.Project, StringComparison.OrdinalIgnoreCase);
    }

    private static string? TryFindDefaultProjectFile()
    {
        var currentDirectory = Directory.GetCurrentDirectory();
        var projects = Directory.GetFiles(currentDirectory, $"*{RavenFileExtensions.Project}");
        if (projects.Length == 1)
            return projects[0];

        if (projects.Length > 1)
        {
            Console.Error.WriteLine("Multiple .rvnproj files were found. Specify the project explicitly.");
            foreach (var project in projects.OrderBy(static p => p, StringComparer.OrdinalIgnoreCase))
                Console.Error.WriteLine($"- {Path.GetFileName(project)}");
        }

        return null;
    }

    private static int RunDotnet(IReadOnlyList<string> dotnetArgs)
    {
        var startInfo = new ProcessStartInfo("dotnet")
        {
            UseShellExecute = false
        };

        foreach (var arg in dotnetArgs)
            startInfo.ArgumentList.Add(arg);

        try
        {
            using var process = Process.Start(startInfo);
            if (process is null)
            {
                Console.Error.WriteLine("Failed to start dotnet.");
                return 1;
            }

            process.WaitForExit();
            return process.ExitCode;
        }
        catch (Exception ex) when (ex is InvalidOperationException or System.ComponentModel.Win32Exception)
        {
            Console.Error.WriteLine($"Failed to start dotnet: {ex.Message}");
            return 1;
        }
    }

    private static bool IsHelp(string arg)
        => arg is "-h" or "--help" or "/?" or "help";

    private static void PrintHelp(string commandName)
    {
        switch (commandName)
        {
            case "build":
                Console.WriteLine("Usage: rvn build [project.rvnproj] [--constant NAME=VALUE] [dotnet-build-options]");
                Console.WriteLine("Runs: dotnet build <project.rvnproj> [dotnet-build-options]");
                break;
            case "run":
                Console.WriteLine("Usage: rvn run <file.rvn> [compiler-options] [-- application-args]");
                Console.WriteLine("       rvn run [project.rvnproj] [--constant NAME=VALUE] [dotnet-run-options] [-- application-args]");
                Console.WriteLine("Runs a source file as an isolated file-based application, or runs a project through dotnet run.");
                Console.WriteLine("Runs: dotnet run --project <project.rvnproj> [dotnet-run-options] [-- application-args]");
                break;
            case "clean":
                Console.WriteLine("Usage: rvn clean [project.rvnproj] [dotnet-clean-options]");
                Console.WriteLine("Runs: dotnet clean <project.rvnproj> [dotnet-clean-options]");
                break;
        }

        Console.WriteLine();
        if (commandName is "build" or "run")
            Console.WriteLine("--constant NAME=VALUE supplies or overrides a typed extern const (repeatable).");
        Console.WriteLine("When no input is specified, rvn uses the single .rvnproj in the current directory.");
    }
}

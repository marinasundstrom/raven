using System.Runtime.InteropServices.JavaScript;
using System.Runtime.Versioning;
using System.Text.Json;

using Raven.Playground.Services;

namespace Raven.Playground.Worker;

[SupportedOSPlatform("browser")]
public static partial class PlaygroundWorkerMethods
{
    private static readonly Lazy<PlaygroundLanguageService> s_languageService = new(() =>
        new PlaygroundLanguageService(new PlaygroundFrameworkReferences()));
    private static readonly Lazy<PlaygroundProgramRunner> s_runner = new(() => new PlaygroundProgramRunner());

    [JSExport]
    public static string GetCompletions(string source, int position)
    {
        try
        {
            return JsonSerializer.Serialize(s_languageService.Value.GetCompletions(source, position));
        }
        catch (Exception exception)
        {
            throw new InvalidOperationException(exception.ToString());
        }
    }

    [JSExport]
    public static string GetHover(string source, int position)
    {
        try
        {
            return JsonSerializer.Serialize(s_languageService.Value.GetHover(source, position));
        }
        catch (Exception exception)
        {
            throw new InvalidOperationException(exception.ToString());
        }
    }

    [JSExport]
    public static async Task<string> Compile(string source, bool run)
    {
        try
        {
            var compilation = s_languageService.Value.Compile(source);
            if (!compilation.Success || compilation.AssemblyImage is null)
            {
                return JsonSerializer.Serialize(new PlaygroundWorkerResult(
                    false,
                    0,
                    compilation.Diagnostics,
                    null,
                    null));
            }

            if (!run)
            {
                return JsonSerializer.Serialize(new PlaygroundWorkerResult(
                    true,
                    compilation.AssemblyImage.Length,
                    compilation.Diagnostics,
                    null,
                    null));
            }

            var execution = await s_runner.Value.RunAsync(
                compilation.AssemblyImage,
                compilation.AsyncEntryPointImplementationName);
            return JsonSerializer.Serialize(new PlaygroundWorkerResult(
                true,
                compilation.AssemblyImage.Length,
                compilation.Diagnostics,
                execution.ExitCode,
                execution.Output));
        }
        catch (Exception exception)
        {
            throw new InvalidOperationException(exception.ToString());
        }
    }
}

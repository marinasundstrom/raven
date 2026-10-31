using System.Diagnostics;

namespace NeoClrMetadataProbe;

internal static class DriverChecks
{
    internal static async Task RunRuntime(string driver, string runtime, string core, string directory)
    {
        if (Directory.Exists(directory)) throw new IOException("output must be fresh");
        Directory.CreateDirectory(directory);
        await Run(Path.GetFullPath(driver), Path.GetFullPath(core), Path.GetFullPath(directory), async (expected, arguments) =>
        {
            var start = new ProcessStartInfo(Path.GetFullPath(runtime)) { RedirectStandardOutput = true, RedirectStandardError = true };
            foreach (var argument in arguments) start.ArgumentList.Add(argument);
            using var process = Process.Start(start)!;
            var stdout = process.StandardOutput.ReadToEndAsync(); var stderr = process.StandardError.ReadToEndAsync();
            using var timeout = new CancellationTokenSource(TimeSpan.FromMinutes(2));
            try { await process.WaitForExitAsync(timeout.Token); }
            catch (OperationCanceledException) { process.Kill(true); throw new TimeoutException("driver runtime check timed out"); }
            var text = await stdout + await stderr;
            Check(process.ExitCode == expected, $"runtime exit {process.ExitCode}, expected {expected}: {text}");
            return text;
        });
    }
    internal static async Task Run(string driver, string core, string directory, Func<int, string[], Task<string>> runtime)
    {
        var librarySource = Path.Combine(directory, "DriverLibrary.rvn");
        var mainSource = Path.Combine(directory, "DriverMain.rvn");
        var helperSource = Path.Combine(directory, "DriverHelper.rvn");
        File.WriteAllText(librarySource, """
            namespace Example
            public static class Math {
                public static func Echo(value: string) -> string => value
                public static func Twice(value: int) -> int => value * 2
            }
            """);
        File.WriteAllText(mainSource, """
            func Main() -> int {
                Greet()
                System.Console.WriteLine(Example.Math.Echo("Library 🌍"))
                return Example.Math.Twice(21)
            }
            """);
        File.WriteAllText(helperSource, """
            func Echo(value: string) -> string => value
            func Greet() {
                let message = Echo("Hello World")
                System.Console.WriteLine(message)
            }
            """);
        var library = Path.Combine(directory, "DriverLibrary.dll");
        var application = Path.Combine(directory, "DriverApplication.dll");
        await Compile(0, "--library", "-o", library, librarySource);
        await Compile(0, "--reference", library, "-o", application, mainSource, helperSource);
        await runtime(0, ["verify", application, "--module", library]);
        var result = await runtime(42, ["run", application, "--module", library]);
        Check(result.Replace("\r\n", "\n") == "Hello World\nLibrary 🌍\n", "driver execution output");
        var unitSource = Path.Combine(directory, "DriverUnitMain.rvn");
        var unitApplication = Path.Combine(directory, "DriverUnitApplication.dll");
        File.WriteAllText(unitSource, "func Main() => Greet()");
        await Compile(0, "-o", unitApplication, unitSource, helperSource);
        await runtime(0, ["verify", unitApplication]);
        Check((await runtime(0, ["run", unitApplication])).Replace("\r\n", "\n") == "Hello World\n", "Unit entry process exit and stdout");
        var original = File.ReadAllBytes(application);
        await Compile(1, "--reference", library, "-o", application, mainSource, helperSource);
        Check(File.ReadAllBytes(application).SequenceEqual(original), "existing output was modified");
        var invalidSource = Path.Combine(directory, "RejectedDriver.rvn");
        var rejectedOutput = Path.Combine(directory, "RejectedDriver.dll");
        File.WriteAllText(invalidSource, "func Main() -> int { return Convert(42) }\nfunc Convert(value: int) -> int { return (int)(double)value }");
        Check((await Compile(1, "-o", rejectedOutput, invalidSource)).Contains("NEOMETA001"), "unsupported source diagnostic");
        Check(!File.Exists(rejectedOutput), "source error created output");
        await Compile(1, "--reference", typeof(object).Assembly.Location, "-o", rejectedOutput, invalidSource);
        await Compile(1, "--reference", typeof(Console).Assembly.Location, "-o", rejectedOutput, invalidSource);
        Check(!File.Exists(rejectedOutput), "ordinary CLI reference created output");
        await Compile(1, "--unknown");
        await Compile(1, "-o");
        await Compile(0, "--help");
        Console.WriteLine("PASS rvnc native command: source/library emission, reference binding, assembly function call, runtime execution and failures");

        async Task<string> Compile(int expected, params string[] args)
        {
            var start = new ProcessStartInfo("dotnet") { RedirectStandardOutput = true, RedirectStandardError = true };
            start.ArgumentList.Add(driver);
            start.ArgumentList.Add("neoclr");
            if (!args.Contains("--help"))
            {
                start.ArgumentList.Add("--core-reference");
                start.ArgumentList.Add(core);
            }
            foreach (var argument in args) start.ArgumentList.Add(argument);
            using var process = Process.Start(start)!;
            var stdout = process.StandardOutput.ReadToEndAsync();
            var stderr = process.StandardError.ReadToEndAsync();
            await process.WaitForExitAsync();
            var text = await stdout + await stderr;
            Check(process.ExitCode == expected, $"rvnc exit {process.ExitCode}, expected {expected}: {text}");
            return text;
        }
    }

    private static void Check(bool condition, string message)
    {
        if (!condition) throw new Exception(message);
    }
}

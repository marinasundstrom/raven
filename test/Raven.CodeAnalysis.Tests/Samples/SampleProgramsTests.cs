using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using Raven.CodeAnalysis;
using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class SampleProgramsTests
{
    public static IEnumerable<object[]> SamplePrograms => new[]
    {
        new object[] {"arrays.rav", Array.Empty<string>()},
        new object[] {"enums.rav", Array.Empty<string>()},
        new object[] {"general.rav", Array.Empty<string>()},
        new object[] {"generics.rav", Array.Empty<string>()},
        new object[] {"io.rav", new[] {"."}},
        new object[] {"test2.rav", Array.Empty<string>()},
        new object[] {"type-unions.rav", Array.Empty<string>()},
        new object[] {"tuples.rav", Array.Empty<string>()},
        new object[] {"main.rav", Array.Empty<string>()},
        new object[] {"classes.rav", Array.Empty<string>()},
    };

    [Theory]
    [MemberData(nameof(SamplePrograms))]
    public void Sample_should_compile_and_run(string fileName, string[] args)
    {
        var projectDir = Path.GetFullPath(Path.Combine("..", "..", "..", "..", "..", "src", "Raven.Compiler"));
        var samplePath = Path.Combine(projectDir, "samples", fileName);

        var outputDir = Path.Combine(Path.GetTempPath(), "raven_samples", Guid.NewGuid().ToString());
        Directory.CreateDirectory(outputDir);
        var outputDll = Path.Combine(outputDir, Path.ChangeExtension(fileName, ".dll"));

        var build = Process.Start(new ProcessStartInfo
        {
            FileName = "dotnet",
            Arguments = $"run --project \"{projectDir}\" -- \"{samplePath}\" -o \"{outputDll}\"",
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            WorkingDirectory = projectDir,
        })!;
        build.WaitForExit();
        Assert.Equal(0, build.ExitCode);

        var testDep = Path.Combine(projectDir, "TestDep.dll");
        if (File.Exists(testDep))
            File.Copy(testDep, Path.Combine(outputDir, "TestDep.dll"), overwrite: true);

        var refDir = ReferenceAssemblyPaths.GetReferenceAssemblyDir();
        var version = new DirectoryInfo(Path.GetDirectoryName(Path.GetDirectoryName(refDir)!)!).Name;
        File.WriteAllText(
            Path.Combine(outputDir, Path.ChangeExtension(fileName, ".runtimeconfig.json")),
            $@"{{
  ""runtimeOptions"": {{
    ""tfm"": ""net9.0"",
    ""framework"": {{
      ""name"": ""Microsoft.NETCore.App"",
      ""version"": ""{version}""
    }}
  }}
}}" );

        var run = Process.Start(new ProcessStartInfo
        {
            FileName = "dotnet",
            Arguments = $"\"{outputDll}\" {string.Join(' ', args)}",
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            WorkingDirectory = outputDir,
        })!;
        run.WaitForExit();
        Assert.Equal(0, run.ExitCode);
    }
}

using System;
using System.Diagnostics;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests.Metadata;

public sealed class CSharpUnionInteropTests
{
    [Fact]
    public void CSharpUnionFromLatestSdk_ImportsNullableContent()
    {
        if (!TryGetLatestDotNet11Sdk(out var sdkVersion))
            return;

        var directory = Path.Combine(Path.GetTempPath(), $"raven-csharp-union-{Guid.NewGuid():N}");
        Directory.CreateDirectory(directory);

        try
        {
            File.WriteAllText(
                Path.Combine(directory, "global.json"),
                $$"""
                {
                  "sdk": {
                    "version": "{{sdkVersion}}",
                    "rollForward": "disable"
                  }
                }
                """);

            File.WriteAllText(
                Path.Combine(directory, "CSharpUnionFixture.csproj"),
                """
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net11.0</TargetFramework>
                    <LangVersion>preview</LangVersion>
                    <Nullable>enable</Nullable>
                    <ImplicitUsings>disable</ImplicitUsings>
                  </PropertyGroup>
                </Project>
                """);

            File.WriteAllText(
                Path.Combine(directory, "UnionFixture.cs"),
                """
                namespace CSharpUnionFixture;

                public union Foo(int, double?);
                """);

            var build = RunDotnet(["build", "/property:WarningLevel=0", "-v:minimal"], directory);
            Assert.True(build.ExitCode == 0, build.Output);

            var referencePath = Path.Combine(directory, "bin", "Debug", "net11.0", "CSharpUnionFixture.dll");
            Assert.True(File.Exists(referencePath), referencePath);

            var net11Version = TargetFrameworkResolver.ResolveVersion("net11.0");
            var net11References = TargetFrameworkResolver.GetReferenceAssemblies(net11Version)
                .Where(File.Exists)
                .Select(MetadataReference.CreateFromFile)
                .ToArray();

            var compilation = Compilation.Create(
                "csharp-union-interop",
                [],
                [.. net11References, MetadataReference.CreateFromFile(referencePath)]);
            var fixtureNamespace = compilation.GlobalNamespace.GetMembers("CSharpUnionFixture").OfType<INamespaceSymbol>().Single();
            var foo = fixtureNamespace.GetMembers("Foo").OfType<IUnionSymbol>().Single();

            Assert.True(foo.ContentMayBeNull);
            Assert.Contains(foo.MemberTypes, static member => member.SpecialType == SpecialType.System_Int32);
            Assert.Contains(foo.MemberTypes, static member =>
                member is INamedTypeSymbol { SpecialType: SpecialType.System_Nullable_T } namedMember &&
                namedMember.TypeArguments.Length == 1 &&
                namedMember.TypeArguments[0].SpecialType == SpecialType.System_Double);

            var valueProperty = Assert.Single(foo.GetMembers("Value").OfType<IPropertySymbol>());
            Assert.True(valueProperty.Type.IsNullable);
        }
        finally
        {
            if (Directory.Exists(directory))
                Directory.Delete(directory, recursive: true);
        }
    }

    private static bool TryGetLatestDotNet11Sdk(out string sdkVersion)
    {
        var result = RunDotnet(["--list-sdks"], Directory.GetCurrentDirectory());
        if (result.ExitCode != 0)
        {
            sdkVersion = string.Empty;
            return false;
        }

        sdkVersion = result.Output
            .Split([Environment.NewLine], StringSplitOptions.RemoveEmptyEntries)
            .Select(static line => line.Split(' ', StringSplitOptions.RemoveEmptyEntries).FirstOrDefault())
            .Where(static version => version is not null && version.StartsWith("11.", StringComparison.Ordinal))
            .LastOrDefault() ?? string.Empty;

        return sdkVersion.Length > 0;
    }

    private static (int ExitCode, string Output) RunDotnet(string[] arguments, string workingDirectory)
    {
        using var process = new Process();
        process.StartInfo = new ProcessStartInfo("dotnet")
        {
            WorkingDirectory = workingDirectory,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false
        };

        foreach (var argument in arguments)
            process.StartInfo.ArgumentList.Add(argument);

        process.Start();
        var output = process.StandardOutput.ReadToEnd();
        output += process.StandardError.ReadToEnd();
        process.WaitForExit();

        return (process.ExitCode, output);
    }
}

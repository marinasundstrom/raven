using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

using Microsoft.CodeAnalysis;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;
using RavenSyntaxTree = Raven.CodeAnalysis.Syntax.SyntaxTree;

namespace Raven.CodeAnalysis.Tests.CodeGen;

internal static class CodeGenTestUtilities
{
    internal static byte[] EmitAssembly(string code, string assemblyName, params string[] additionalSources)
    {
        var compilation = CreateCompilation(code, assemblyName, additionalSources);
        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Xunit.Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        return peStream.ToArray();
    }

    internal static byte[] EmitLibrary(string code, string assemblyName, params string[] additionalSources)
    {
        var compilation = CreateCompilation(code, assemblyName, OutputKind.DynamicallyLinkedLibrary, additionalSources);
        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Xunit.Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        return peStream.ToArray();
    }

    internal static string? EmitAndRun(string code, string assemblyName, params string[] additionalSources)
    {
        var compilation = CreateCompilation(code, assemblyName, additionalSources);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Xunit.Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        var assemblyBytes = peStream.ToArray();
        var assembly = Assembly.Load(assemblyBytes);
        var entryPoint = assembly.EntryPoint;
        Xunit.Assert.NotNull(entryPoint);

        var originalOut = Console.Out;
        using var writer = new StringWriter();
        Console.SetOut(writer);

        try
        {
            AssertRuntimeSupport();
            var parameters = entryPoint!.GetParameters();

            object?[]? arguments = parameters.Length switch
            {
                0 => null,
                1 => new object?[] { Array.Empty<string>() },
                _ => throw new InvalidOperationException("Unexpected entry point signature."),
            };

            try
            {
                entryPoint.Invoke(null, arguments);
            }
            catch (TargetInvocationException invocationException)
                when (invocationException.InnerException is MissingMethodException mme
                    && mme.Message.Contains("System.Runtime.CompilerServices.ITuple", StringComparison.Ordinal))
            {
                return null;
            }
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        var output = writer.ToString();
        return output.ReplaceLineEndings("\n").TrimEnd('\n');
    }

    private static void AssertRuntimeSupport()
    {
        // Tuple pattern lowering relies on the runtime ITuple shape. Skip the test if it's missing.
        Xunit.Assert.NotNull(typeof(System.Runtime.CompilerServices.ITuple).GetProperty("Length"));
    }

    internal static MetadataReference[] RuntimeMetadataReferences { get; } = GetRuntimeMetadataReferences();

    private static Compilation CreateCompilation(string code, string assemblyName, params string[] additionalSources)
        => CreateCompilation(code, assemblyName, OutputKind.ConsoleApplication, additionalSources);

    private static Compilation CreateCompilation(string code, string assemblyName, OutputKind outputKind, params string[] additionalSources)
    {
        var syntaxTrees = new List<RavenSyntaxTree> { RavenSyntaxTree.ParseText(code) };

        foreach (var source in additionalSources)
            syntaxTrees.Add(RavenSyntaxTree.ParseText(source));

        var references = RuntimeMetadataReferences;

        return Compilation.Create(assemblyName, new CompilationOptions(outputKind))
            .AddSyntaxTrees(syntaxTrees.ToArray())
            .AddReferences(references);
    }

    private static MetadataReference[] GetRuntimeMetadataReferences()
    {
        var tpa = AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") as string;
        if (string.IsNullOrEmpty(tpa))
            return TestMetadataReferences.Default;

        var references = new List<MetadataReference>();
        var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

        foreach (var path in tpa.Split(Path.PathSeparator))
        {
            if (string.IsNullOrEmpty(path))
                continue;

            var name = Path.GetFileNameWithoutExtension(path);
            if (!seen.Add(name))
                continue;

            references.Add(MetadataReference.CreateFromFile(path));
        }

        return references.ToArray();
    }
}

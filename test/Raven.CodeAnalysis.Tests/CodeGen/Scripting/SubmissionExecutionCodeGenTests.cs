using System.Reflection;
using System.Runtime.Loader;

using Raven.CodeAnalysis.Scripting;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.CodeGen.Scripting;

public sealed class SubmissionExecutionCodeGenTests
{
    [Fact]
    public void PreviousSubmissionVariable_LoadsPersistedValue()
    {
        var first = CreateSubmission("let value = 40", "variable_load_submission0");
        var second = CreateSubmission("System.Console.WriteLine(value + 2)", "variable_load_submission1", first);

        var firstImage = Emit(first);
        var secondImage = Emit(second);
        var variables = new object?[second.SubmissionVariableCount];

        using var loadContext = new SubmissionLoadContext();
        var firstAssembly = loadContext.Load(firstImage);
        var secondAssembly = loadContext.Load(secondImage);
        using var executionScope = SubmissionRuntime.Enter(variables);

        InvokeEntryPoint(firstAssembly);

        var originalOut = Console.Out;
        using var writer = new StringWriter();
        try
        {
            Console.SetOut(writer);
            InvokeEntryPoint(secondAssembly);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        Assert.Equal("42", writer.ToString().Trim());
        Assert.Equal(40, variables[0]);
    }

    [Fact]
    public void PreviousSubmissionVariable_AssignmentPersistsAcrossChain()
    {
        var first = CreateSubmission("var value = 40", "variable_assignment_submission0");
        var second = CreateSubmission("value = value + 1", "variable_assignment_submission1", first);
        var third = CreateSubmission("System.Console.WriteLine(value)", "variable_assignment_submission2", second);

        var variables = new object?[third.SubmissionVariableCount];
        using var loadContext = new SubmissionLoadContext();
        var assemblies = new[] { first, second, third }
            .Select(compilation => loadContext.Load(Emit(compilation)))
            .ToArray();
        using var executionScope = SubmissionRuntime.Enter(variables);

        var originalOut = Console.Out;
        using var writer = new StringWriter();
        try
        {
            Console.SetOut(writer);
            foreach (var assembly in assemblies)
                InvokeEntryPoint(assembly);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        Assert.Equal("41", writer.ToString().Trim());
        Assert.Equal(41, variables[0]);
    }

    [Fact]
    public void PreviousSubmissionFunction_ExecutesThroughEmittedReference()
    {
        var first = CreateSubmission(
            "func twice(value: int) -> int => value * 2",
            "function_submission0");
        var firstImage = Emit(first);
        var firstReference = MetadataReference.CreateFromImage(firstImage);
        var second = CreateSubmission(
            "System.Console.WriteLine(twice(21))",
            "function_submission1",
            first,
            firstReference);

        using var loadContext = new SubmissionLoadContext();
        _ = loadContext.Load(firstImage);
        var secondAssembly = loadContext.Load(Emit(second));
        using var executionScope = SubmissionRuntime.Enter(new object?[second.SubmissionVariableCount]);

        var originalOut = Console.Out;
        using var writer = new StringWriter();
        try
        {
            Console.SetOut(writer);
            InvokeEntryPoint(secondAssembly);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        Assert.Equal("42", writer.ToString().Trim());
    }

    [Fact]
    public void PreviousSubmissionType_ExecutesThroughEmittedReference()
    {
        var first = CreateSubmission("class Widget {}", "type_submission0");
        var firstImage = Emit(first);
        var firstReference = MetadataReference.CreateFromImage(firstImage);
        var second = CreateSubmission(
            "let widget = Widget()\nSystem.Console.WriteLine(widget.GetType().Name)",
            "type_submission1",
            first,
            firstReference);

        using var loadContext = new SubmissionLoadContext();
        _ = loadContext.Load(firstImage);
        var secondAssembly = loadContext.Load(Emit(second));
        using var executionScope = SubmissionRuntime.Enter(new object?[second.SubmissionVariableCount]);

        var originalOut = Console.Out;
        using var writer = new StringWriter();
        try
        {
            Console.SetOut(writer);
            InvokeEntryPoint(secondAssembly);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        Assert.Equal("Widget", writer.ToString().Trim());
    }

    private static Compilation CreateSubmission(
        string source,
        string assemblyName,
        Compilation? previous = null,
        MetadataReference? previousReference = null)
    {
        var tree = SyntaxTree.ParseText(source, new ParseOptions { Kind = SourceCodeKind.Script });
        return Compilation.CreateScriptCompilation(
            assemblyName,
            tree,
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication),
            previous,
            previousReference);
    }

    private static byte[] Emit(Compilation compilation)
    {
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        return stream.ToArray();
    }

    private static void InvokeEntryPoint(Assembly assembly)
    {
        var entryPoint = assembly.EntryPoint;
        Assert.NotNull(entryPoint);
        _ = entryPoint!.Invoke(null, [Array.Empty<string>()]);
    }

    private sealed class SubmissionLoadContext : AssemblyLoadContext, IDisposable
    {
        public SubmissionLoadContext()
            : base("RavenSubmissionTests", isCollectible: true)
        {
        }

        protected override Assembly? Load(AssemblyName assemblyName)
        {
            if (assemblyName.Name == typeof(SubmissionRuntime).Assembly.GetName().Name)
                return typeof(SubmissionRuntime).Assembly;

            return Assemblies.FirstOrDefault(assembly =>
                AssemblyName.ReferenceMatchesDefinition(assembly.GetName(), assemblyName));
        }

        public Assembly Load(byte[] image)
        {
            using var stream = new MemoryStream(image);
            return LoadFromStream(stream);
        }

        public void Dispose()
            => Unload();
    }
}

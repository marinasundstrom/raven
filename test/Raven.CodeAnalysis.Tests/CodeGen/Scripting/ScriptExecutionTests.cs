using Raven.CodeAnalysis.Scripting;

namespace Raven.CodeAnalysis.Tests.CodeGen.Scripting;

public sealed class ScriptExecutionTests
{
    [Fact]
    public async Task RunAsync_ExecutesSubmission()
    {
        var originalOut = Console.Out;
        using var writer = new StringWriter();
        try
        {
            Console.SetOut(writer);
            using var state = await RavenScript.RunAsync("System.Console.WriteLine(42)");
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        Assert.Equal("42", writer.ToString().Trim());
    }

    [Fact]
    public async Task RunAsync_ReturnsTrailingExpressionValue()
    {
        using var state = await RavenScript.RunAsync("1 + 2");

        Assert.True(state.HasReturnValue);
        Assert.Equal(3, state.ReturnValue);
    }

    [Fact]
    public async Task ContinueWithAsync_ReturnsValueFromPreviousFunction()
    {
        using var first = await RavenScript.RunAsync(
            "func twice(value: int) -> int => value * 2");
        using var second = await first.ContinueWithAsync("twice(21)");

        Assert.True(second.HasReturnValue);
        Assert.Equal(42, second.ReturnValue);
    }

    [Fact]
    public async Task RunAsync_ReportsNoReturnValueForUnitSubmission()
    {
        using var state = await RavenScript.RunAsync("let value = 42");

        Assert.False(state.HasReturnValue);
        Assert.Null(state.ReturnValue);
    }

    [Fact]
    public async Task ContinueWithAsync_PreservesAndMutatesVariables()
    {
        using var first = await RavenScript.RunAsync("var value = 40");
        var second = await first.ContinueWithAsync("value = value + 2");

        var originalOut = Console.Out;
        using var writer = new StringWriter();
        try
        {
            Console.SetOut(writer);
            using var third = await second.ContinueWithAsync("System.Console.WriteLine(value)");
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        Assert.Equal("42", writer.ToString().Trim());
    }

    [Fact]
    public async Task ContinueWithAsync_UsesPreviousFunctionsAndTypes()
    {
        using var first = await RavenScript.RunAsync(
            "func twice(value: int) -> int => value * 2\nclass Widget {}");

        var originalOut = Console.Out;
        using var writer = new StringWriter();
        try
        {
            Console.SetOut(writer);
            using var second = await first.ContinueWithAsync(
                "let widget = Widget()\nSystem.Console.WriteLine(widget.GetType().Name)\nSystem.Console.WriteLine(twice(21))");
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        Assert.Equal(["Widget", "42"], writer.ToString().Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries));
    }

    [Fact]
    public async Task RunAsync_ThrowsCompilationExceptionForErrors()
    {
        var exception = await Assert.ThrowsAsync<RavenCompilationException>(
            () => RavenScript.RunAsync("missingName + 1"));

        Assert.Contains(
            exception.Diagnostics,
            diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }
}

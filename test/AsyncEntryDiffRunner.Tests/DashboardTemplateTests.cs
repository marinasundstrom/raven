using System;
using Raven.Tools.AsyncEntryDiffRunner;
using Xunit;

namespace AsyncEntryDiffRunner.Tests;

public class DashboardTemplateTests
{
    [Fact]
    public void Build_IncludesPermutationAndIlStatus()
    {
        var generatedAt = new DateTime(2024, 1, 2, 3, 4, 5, DateTimeKind.Utc);
        var permutations = new[]
        {
            new AsyncEntryPermutationStatus(
                "Generic entry (single await)",
                "docs/investigations/assets/async_entry_generic.rav",
                "docs/investigations/snippets/async-entry-step21-generic.log",
                "Step21",
                CompilationSucceeded: true,
                CompilationExitCode: 0,
                ExecutionSucceeded: true,
                ExecutionExitCode: 42,
                RuntimeMatchesBaseline: true,
                IlMatchesBaseline: true),
            new AsyncEntryPermutationStatus(
                "Multi-await entry",
                "docs/investigations/assets/async_entry_multi.rav",
                "docs/investigations/snippets/async-entry-step15.log",
                "Step15",
                CompilationSucceeded: false,
                CompilationExitCode: 1,
                ExecutionSucceeded: false,
                ExecutionExitCode: -1,
                RuntimeMatchesBaseline: false,
                IlMatchesBaseline: false),
            new AsyncEntryPermutationStatus(
                "Async lambda (single await)",
                "docs/investigations/assets/async_lambda.rav",
                "docs/investigations/snippets/async-entry-step23-lambda.log",
                "Step23/lambda",
                CompilationSucceeded: true,
                CompilationExitCode: 0,
                ExecutionSucceeded: true,
                ExecutionExitCode: 42,
                RuntimeMatchesBaseline: true,
                IlMatchesBaseline: false)
        };

        var ilStatus = new AsyncEntryIlDiffStatus(
            RavenCompilationSucceeded: true,
            RavenExitCode: 0,
            RoslynCompilationSucceeded: false,
            RoslynExitCode: 1,
            RavenInstructionCount: 120,
            RoslynInstructionCount: 121,
            InstructionDifferenceCount: 3);

        var markdown = AsyncEntryDashboardTemplate.Build(generatedAt, permutations, ilStatus);

        Assert.Contains("_Last generated: 2024-01-02 03:04:05 UTC_", markdown);
        Assert.Contains("| Generic entry (single await) | `docs/investigations/assets/async_entry_generic.rav` | `docs/investigations/snippets/async-entry-step21-generic.log` | Step21 | ✅ (exit 0) | ✅ (exit 42) | ✅ | ✅ |", markdown);
        Assert.Contains("| Multi-await entry | `docs/investigations/assets/async_entry_multi.rav` | `docs/investigations/snippets/async-entry-step15.log` | Step15 | ❌ (exit 1) | ❌ (exit -1) | ❌ | ❌ |", markdown);
        Assert.Contains("| Async lambda (single await) | `docs/investigations/assets/async_lambda.rav` | `docs/investigations/snippets/async-entry-step23-lambda.log` | Step23/lambda | ✅ (exit 0) | ✅ (exit 42) | ✅ | ❌ |", markdown);
        Assert.Contains("- Raven compilation: ✅ (exit 0)", markdown);
        Assert.Contains("- Roslyn compilation: ❌ (exit 1)", markdown);
        Assert.Contains("- Instruction comparison: ❌ (120 vs. 121)", markdown);
        Assert.Contains("_Source: `docs/investigations/reports/async-entry-nightly.md`_", markdown);
    }
}

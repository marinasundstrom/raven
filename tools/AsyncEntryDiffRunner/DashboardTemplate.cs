using System;
using System.Collections.Generic;
using System.Text;

namespace Raven.Tools.AsyncEntryDiffRunner;

public sealed record class AsyncEntryPermutationStatus(
    string Name,
    string SourcePath,
    string BaselinePath,
    string StepLabel,
    bool CompilationSucceeded,
    int CompilationExitCode,
    bool ExecutionSucceeded,
    int ExecutionExitCode,
    bool RuntimeMatchesBaseline,
    bool IlMatchesBaseline);

public sealed record class AsyncEntryIlDiffStatus(
    bool RavenCompilationSucceeded,
    int RavenExitCode,
    bool RoslynCompilationSucceeded,
    int RoslynExitCode,
    int RavenInstructionCount,
    int RoslynInstructionCount,
    int InstructionDifferenceCount);

public static class AsyncEntryDashboardTemplate
{
    public static string Build(
        DateTime generatedAtUtc,
        IReadOnlyList<AsyncEntryPermutationStatus> permutations,
        AsyncEntryIlDiffStatus ilStatus)
    {
        var builder = new StringBuilder();

        builder.AppendLine("# Roslyn diff dashboard");
        builder.AppendLine();
        builder.AppendLine($"_Last generated: {generatedAtUtc:yyyy-MM-dd HH:mm:ss} UTC_");
        builder.AppendLine();
        builder.AppendLine("## Async entry pointer / IL summary");
        builder.AppendLine();
        builder.AppendLine("| Permutation | Source asset | Baseline log | Step | Compilation | Execution | Runtime timeline | IL timeline |");
        builder.AppendLine("| --- | --- | --- | --- | --- | --- | --- | --- |");

        foreach (var permutation in permutations)
        {
            builder.AppendLine(
                $"| {permutation.Name} | `{permutation.SourcePath}` | `{permutation.BaselinePath}` | {permutation.StepLabel} | " +
                $"{FormatStatus(permutation.CompilationSucceeded)} (exit {permutation.CompilationExitCode}) | " +
                $"{FormatExecution(permutation.ExecutionSucceeded, permutation.ExecutionExitCode)} | " +
                $"{FormatStatus(permutation.RuntimeMatchesBaseline)} | {FormatStatus(permutation.IlMatchesBaseline)} |");
        }

        builder.AppendLine();
        builder.AppendLine("### MoveNext IL diff");
        builder.AppendLine();
        builder.AppendLine($"- Raven compilation: {FormatStatus(ilStatus.RavenCompilationSucceeded)} (exit {ilStatus.RavenExitCode})");
        builder.AppendLine($"- Roslyn compilation: {FormatStatus(ilStatus.RoslynCompilationSucceeded)} (exit {ilStatus.RoslynExitCode})");
        builder.AppendLine($"- Instruction comparison: {FormatStatus(ilStatus.InstructionDifferenceCount == 0)} ({ilStatus.RavenInstructionCount} vs. {ilStatus.RoslynInstructionCount})");
        builder.AppendLine();
        builder.AppendLine("_Source: `docs/investigations/reports/async-entry-nightly.md`_");

        return builder.ToString();
    }

    private static string FormatStatus(bool succeeded) => succeeded ? "✅" : "❌";

    private static string FormatExecution(bool succeeded, int exitCode)
    {
        var icon = FormatStatus(succeeded);
        return $"{icon} (exit {exitCode})";
    }
}

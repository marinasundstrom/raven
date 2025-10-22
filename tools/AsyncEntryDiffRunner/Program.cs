using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.CompilerServices;
using System.Runtime.Loader;
using System.Text;

namespace Raven.Tools.AsyncEntryDiffRunner;

internal static class Program
{
    private static readonly OpCode[] s_singleByteOpCodes;
    private static readonly OpCode[] s_multiByteOpCodes;

    static Program()
    {
        s_singleByteOpCodes = new OpCode[0x100];
        s_multiByteOpCodes = new OpCode[0x100];

        foreach (var field in typeof(OpCodes).GetFields(BindingFlags.Public | BindingFlags.Static))
        {
            if (field.GetValue(null) is not OpCode opcode)
                continue;

            var value = (ushort)opcode.Value;
            if (value < 0x100)
            {
                s_singleByteOpCodes[value] = opcode;
            }
            else if ((value & 0xFF00) == 0xFE00)
            {
                s_multiByteOpCodes[value & 0xFF] = opcode;
            }
        }
    }

    public static int Main(string[] args)
    {
        try
        {
            var repoRoot = FindRepositoryRoot();
            if (repoRoot is null)
            {
                Console.Error.WriteLine("Unable to locate the Raven repository root.");
                return 1;
            }

            var outputPath = args.Length > 0
                ? Path.GetFullPath(args[0])
                : Path.Combine(repoRoot, "docs", "investigations", "reports", "async-entry-nightly.md");

            Directory.CreateDirectory(Path.GetDirectoryName(outputPath)!);

            const string stepLabel = "Step15";
            var pointerBaseline = LoadPointerBaseline(repoRoot, stepLabel);
            var pointerResult = RunPointerRegression(repoRoot, pointerBaseline, stepLabel);
            var ilResult = RunIlDiff(repoRoot);

            WriteReport(outputPath, pointerResult, ilResult, pointerBaseline, stepLabel);

            Console.WriteLine($"Async entry nightly diff written to {outputPath}");
            return pointerResult.Passed && ilResult.Passed ? 0 : 1;
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine(ex.ToString());
            return 1;
        }
    }

    private static PointerRegressionResult RunPointerRegression(
        string repoRoot,
        IReadOnlyList<(string Field, string Operation)> baseline,
        string stepLabel)
    {
        var sourcePath = Path.Combine(repoRoot, "docs", "investigations", "assets", "async_entry_multi.rav");
        var assemblyPath = Path.Combine(Path.GetTempPath(), $"async-pointer-{Guid.NewGuid():N}.dll");

        CompilationArtifacts? artifacts = null;
        try
        {
            var compileResult = CompileRavenSample(repoRoot, sourcePath, assemblyPath, stepLabel);
            var runtimeResult = compileResult.Process.ExitCode == 0
                ? RunAssembly(assemblyPath)
                : new ProcessResult(-1, string.Empty, string.Empty);

            var runtimeLines = runtimeResult.StdOut
                .Replace("\r\n", "\n", StringComparison.Ordinal)
                .Split('\n', StringSplitOptions.RemoveEmptyEntries);

            var pointerRecords = runtimeLines
                .Where(line => line.StartsWith($"{stepLabel}:", StringComparison.Ordinal))
                .Select(ParsePointerRecord)
                .ToArray();

            var runtimeTimeline = pointerRecords
                .Select(record => (record.Field, record.Operation))
                .ToArray();

            var ilTimeline = compileResult.Process.ExitCode == 0
                ? ReadPointerTimelineFromAssembly(assemblyPath, stepLabel)
                : Array.Empty<(string Field, string Operation)>();

            var runtimeMatches = runtimeTimeline.SequenceEqual(baseline);
            var ilMatches = ilTimeline.SequenceEqual(baseline);
            var executionSucceeded = runtimeResult.ExitCode == 42 && string.IsNullOrWhiteSpace(runtimeResult.StdErr);

            artifacts = compileResult.Artifacts;

            return new PointerRegressionResult(
                CompilationSucceeded: compileResult.Process.ExitCode == 0,
                ExecutionSucceeded: executionSucceeded,
                RuntimeMatchesBaseline: runtimeMatches,
                IlMatchesBaseline: ilMatches,
                RuntimeRecords: pointerRecords,
                RuntimeTimeline: runtimeTimeline,
                IlTimeline: ilTimeline,
                Compilation: compileResult.Process,
                Execution: runtimeResult);
        }
        finally
        {
            if (artifacts is not null)
                DeleteArtifacts(artifacts.Value);
            if (File.Exists(assemblyPath))
                File.Delete(assemblyPath);
        }
    }

    private static IlDiffResult RunIlDiff(string repoRoot)
    {
        var ravenSourcePath = Path.Combine(repoRoot, "docs", "investigations", "assets", "async_entry.rav");
        var ravenAssemblyPath = Path.Combine(Path.GetTempPath(), $"async-il-{Guid.NewGuid():N}.dll");

        CompilationArtifacts? ravenArtifacts = null;
        try
        {
            var ravenCompile = CompileRavenSample(repoRoot, ravenSourcePath, ravenAssemblyPath, stepLabel: null);
            ravenArtifacts = ravenCompile.Artifacts;

            var roslynCompile = BuildRoslynBaseline(repoRoot);

            var ravenInstructions = ravenCompile.Process.ExitCode == 0
                ? DisassembleMoveNext(ravenAssemblyPath)
                : Array.Empty<string>();

            var roslynInstructions = roslynCompile.Process.ExitCode == 0
                ? DisassembleMoveNext(roslynCompile.AssemblyPath)
                : Array.Empty<string>();

            var differences = ComputeInstructionDifferences(ravenInstructions, roslynInstructions);

            return new IlDiffResult(
                RavenCompilationSucceeded: ravenCompile.Process.ExitCode == 0,
                RoslynCompilationSucceeded: roslynCompile.Process.ExitCode == 0,
                RavenInstructions: ravenInstructions,
                RoslynInstructions: roslynInstructions,
                Differences: differences,
                RavenCompilation: ravenCompile.Process,
                RoslynCompilation: roslynCompile.Process);
        }
        finally
        {
            if (ravenArtifacts is not null)
                DeleteArtifacts(ravenArtifacts.Value);
            if (File.Exists(ravenAssemblyPath))
                File.Delete(ravenAssemblyPath);
        }
    }

    private static IReadOnlyList<(string Field, string Operation)> LoadPointerBaseline(string repoRoot, string stepLabel)
    {
        var timelinePath = Path.Combine(repoRoot, "docs", "investigations", "snippets", "async-entry-step15.log");
        if (!File.Exists(timelinePath))
            throw new FileNotFoundException($"Pointer timeline asset not found: {timelinePath}");

        return File.ReadLines(timelinePath)
            .Select(line => line.Trim())
            .Where(line => line.Length > 0 && !line.StartsWith("#", StringComparison.Ordinal))
            .Where(line => line.StartsWith($"{stepLabel}:", StringComparison.Ordinal))
            .Select(ParsePointerFormat)
            .ToArray();
    }

    private static CompilationResult CompileRavenSample(
        string repoRoot,
        string sourcePath,
        string assemblyPath,
        string? stepLabel)
    {
        var projectPath = Path.Combine(repoRoot, "src", "Raven.Compiler", "Raven.Compiler.csproj");
        var args = stepLabel is null
            ? $"run --project \"{projectPath}\" -- \"{sourcePath}\" -o \"{assemblyPath}\""
            : $"run --project \"{projectPath}\" -- --async-investigation {stepLabel} \"{sourcePath}\" -o \"{assemblyPath}\"";

        var process = RunProcess(
            fileName: "dotnet",
            arguments: args,
            workingDirectory: repoRoot);

        var runtimeConfig = Path.ChangeExtension(assemblyPath, ".runtimeconfig.json");
        var deps = Path.ChangeExtension(assemblyPath, ".deps.json");
        var pdb = Path.ChangeExtension(assemblyPath, ".pdb");

        var artifacts = new CompilationArtifacts(
            AssemblyPath: assemblyPath,
            RuntimeConfigPath: File.Exists(runtimeConfig) ? runtimeConfig : null,
            DepsPath: File.Exists(deps) ? deps : null,
            PdbPath: File.Exists(pdb) ? pdb : null);

        return new CompilationResult(process, artifacts);
    }

    private static RoslynCompilationResult BuildRoslynBaseline(string repoRoot)
    {
        var projectPath = Path.Combine(repoRoot, "docs", "investigations", "assets", "RoslynAsyncEntry", "RoslynAsyncEntry.csproj");
        var process = RunProcess(
            fileName: "dotnet",
            arguments: $"build \"{projectPath}\" -c Release",
            workingDirectory: repoRoot);

        var assemblyPath = Path.Combine(
            Path.GetDirectoryName(projectPath)!,
            "bin",
            "Release",
            "net8.0",
            "RoslynAsyncEntry.dll");

        return new RoslynCompilationResult(process, assemblyPath);
    }

    private static ProcessResult RunAssembly(string assemblyPath)
    {
        return RunProcess(
            fileName: "dotnet",
            arguments: $"\"{assemblyPath}\"",
            workingDirectory: Path.GetDirectoryName(assemblyPath)!);
    }

    private static ProcessResult RunProcess(string fileName, string arguments, string workingDirectory)
    {
        var info = new ProcessStartInfo(fileName, arguments)
        {
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            CreateNoWindow = true,
            WorkingDirectory = workingDirectory
        };

        using var process = Process.Start(info) ?? throw new InvalidOperationException($"Failed to start '{fileName}'.");
        var stdOut = process.StandardOutput.ReadToEnd();
        var stdErr = process.StandardError.ReadToEnd();
        process.WaitForExit();

        return new ProcessResult(process.ExitCode, stdOut, stdErr);
    }

    private static IReadOnlyList<(string Field, string Operation)> ReadPointerTimelineFromAssembly(
        string assemblyPath,
        string stepLabel)
    {
        var loadContext = new AssemblyLoadContext($"async-pointer-{Guid.NewGuid():N}", isCollectible: true);
        try
        {
            using var peStream = new FileStream(assemblyPath, FileMode.Open, FileAccess.Read, FileShare.Read);
            var assembly = loadContext.LoadFromStream(peStream);

            var stateMachineType = assembly
                .GetTypes()
                .First(type => typeof(IAsyncStateMachine).IsAssignableFrom(type) && type.Name.Contains("MainAsync", StringComparison.Ordinal));

            var moveNext = stateMachineType.GetMethod("MoveNext", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)
                ?? throw new InvalidOperationException("MoveNext method not found on state machine.");

            return ReadLdstrOperands(moveNext)
                .Where(value => value.StartsWith($"{stepLabel}:", StringComparison.Ordinal))
                .Select(ParsePointerFormat)
                .ToArray();
        }
        finally
        {
            loadContext.Unload();
            GC.Collect();
            GC.WaitForPendingFinalizers();
        }
    }

    private static IReadOnlyList<string> DisassembleMoveNext(string assemblyPath)
    {
        var loadContext = new AssemblyLoadContext($"async-il-{Guid.NewGuid():N}", isCollectible: true);
        try
        {
            using var peStream = new FileStream(assemblyPath, FileMode.Open, FileAccess.Read, FileShare.Read);
            var assembly = loadContext.LoadFromStream(peStream);

            var stateMachineType = assembly
                .GetTypes()
                .First(type => typeof(IAsyncStateMachine).IsAssignableFrom(type) && type.Name.Contains("MainAsync", StringComparison.Ordinal));

            var moveNext = stateMachineType.GetMethod("MoveNext", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)
                ?? throw new InvalidOperationException("MoveNext method not found on state machine.");

            var body = moveNext.GetMethodBody() ?? throw new InvalidOperationException("MoveNext has no method body.");
            var il = body.GetILAsByteArray() ?? throw new InvalidOperationException("MoveNext has no IL body.");
            var module = moveNext.Module;

            var instructions = new List<string>();
            var offset = 0;
            while (offset < il.Length)
            {
                var instructionOffset = offset;
                var opcode = ReadOpCode(il, ref offset);
                var operand = ReadOperand(opcode, il, ref offset, module);
                instructions.Add($"IL_{instructionOffset:X4}: {opcode.Name}{operand}");
            }

            return instructions;
        }
        finally
        {
            loadContext.Unload();
            GC.Collect();
            GC.WaitForPendingFinalizers();
        }
    }

    private static OpCode ReadOpCode(byte[] il, ref int offset)
    {
        var code = il[offset++];
        if (code == 0xFE)
        {
            if (offset >= il.Length)
                throw new InvalidOperationException("Unexpected end of IL stream when decoding multi-byte opcode.");

            var second = il[offset++];
            var opcode = s_multiByteOpCodes[second];
            if (opcode.Value == 0 && opcode != OpCodes.Nop)
                throw new InvalidOperationException($"Unknown opcode: 0xFE 0x{second:X2}");
            return opcode;
        }
        else
        {
            var opcode = s_singleByteOpCodes[code];
            if (opcode.Value == 0 && opcode != OpCodes.Nop)
                throw new InvalidOperationException($"Unknown opcode: 0x{code:X2}");
            return opcode;
        }
    }

    private static string ReadOperand(OpCode opcode, byte[] il, ref int offset, Module module)
    {
        switch (opcode.OperandType)
        {
            case OperandType.InlineNone:
                return string.Empty;
            case OperandType.ShortInlineBrTarget:
            {
                var delta = (sbyte)il[offset++];
                var target = offset + delta;
                return $" IL_{target:X4}";
            }
            case OperandType.InlineBrTarget:
            {
                var delta = BitConverter.ToInt32(il, offset);
                offset += 4;
                var target = offset + delta;
                return $" IL_{target:X4}";
            }
            case OperandType.ShortInlineI:
            {
                var value = (sbyte)il[offset++];
                return $" {value}";
            }
            case OperandType.InlineI:
            {
                var value = BitConverter.ToInt32(il, offset);
                offset += 4;
                return $" {value}";
            }
            case OperandType.InlineI8:
            {
                var value = BitConverter.ToInt64(il, offset);
                offset += 8;
                return $" {value}";
            }
            case OperandType.ShortInlineR:
            {
                var value = BitConverter.ToSingle(il, offset);
                offset += 4;
                return $" {value.ToString("R", CultureInfo.InvariantCulture)}";
            }
            case OperandType.InlineR:
            {
                var value = BitConverter.ToDouble(il, offset);
                offset += 8;
                return $" {value.ToString("R", CultureInfo.InvariantCulture)}";
            }
            case OperandType.InlineString:
            {
                var token = BitConverter.ToInt32(il, offset);
                offset += 4;
                var value = module.ResolveString(token);
                return $" \"{value.Replace("\\", "\\\\", StringComparison.Ordinal)}\"";
            }
            case OperandType.InlineVar:
            {
                var value = BitConverter.ToUInt16(il, offset);
                offset += 2;
                return $" {value}";
            }
            case OperandType.ShortInlineVar:
            {
                var value = il[offset++];
                return $" {value}";
            }
            case OperandType.InlineSwitch:
            {
                var count = BitConverter.ToInt32(il, offset);
                offset += 4;
                var targets = new string[count];
                for (var i = 0; i < count; i++)
                {
                    var delta = BitConverter.ToInt32(il, offset);
                    offset += 4;
                    var target = offset + delta;
                    targets[i] = $"IL_{target:X4}";
                }

                return $" {string.Join(", ", targets)}";
            }
            case OperandType.InlineField:
            case OperandType.InlineMethod:
            case OperandType.InlineType:
            case OperandType.InlineTok:
            case OperandType.InlineSig:
            {
                var token = BitConverter.ToInt32(il, offset);
                offset += 4;
                return $" {ResolveMetadataToken(module, opcode.OperandType, token)}";
            }
            default:
                throw new NotSupportedException($"Unsupported operand type: {opcode.OperandType}");
        }
    }

    private static string ResolveMetadataToken(Module module, OperandType operandType, int metadataToken)
    {
        try
        {
            return operandType switch
            {
                OperandType.InlineField => FormatField(module.ResolveField(metadataToken)),
                OperandType.InlineMethod => FormatMethod(module.ResolveMethod(metadataToken)),
                OperandType.InlineType => FormatType(module.ResolveType(metadataToken)),
                OperandType.InlineTok => FormatMember(module.ResolveMember(metadataToken)),
                OperandType.InlineSig => $"sig:0x{metadataToken:X8}",
                _ => $"token:0x{metadataToken:X8}"
            };
        }
        catch (ArgumentException)
        {
            return $"token:0x{metadataToken:X8}";
        }
    }

    private static string FormatField(FieldInfo field)
        => $"{field.DeclaringType?.FullName ?? field.DeclaringType?.Name}.{field.Name}";

    private static string FormatMethod(MethodBase method)
    {
        var declaringType = method.DeclaringType?.FullName ?? method.DeclaringType?.Name ?? string.Empty;
        return string.IsNullOrEmpty(declaringType)
            ? method.Name
            : $"{declaringType}.{method.Name}";
    }

    private static string FormatType(Type type)
        => type.FullName ?? type.Name;

    private static string FormatMember(MemberInfo member)
        => member switch
        {
            FieldInfo field => FormatField(field),
            MethodBase method => FormatMethod(method),
            Type type => FormatType(type),
            _ => member.ToString() ?? $"token:0x{member.MetadataToken:X8}"
        };

    private static IEnumerable<string> ReadLdstrOperands(MethodInfo method)
    {
        var body = method.GetMethodBody() ?? throw new InvalidOperationException("Method has no body.");
        var il = body.GetILAsByteArray() ?? throw new InvalidOperationException("Method body has no IL.");
        var module = method.Module;

        for (var i = 0; i < il.Length;)
        {
            var opcode = ReadOpCode(il, ref i);

            switch (opcode.OperandType)
            {
                case OperandType.InlineNone:
                    break;
                case OperandType.InlineString:
                {
                    var token = BitConverter.ToInt32(il, i);
                    i += 4;
                    yield return module.ResolveString(token);
                    break;
                }
                case OperandType.ShortInlineBrTarget:
                case OperandType.ShortInlineI:
                case OperandType.ShortInlineVar:
                    i += 1;
                    break;
                case OperandType.InlineVar:
                    i += 2;
                    break;
                case OperandType.InlineI:
                case OperandType.InlineBrTarget:
                case OperandType.InlineField:
                case OperandType.InlineMethod:
                case OperandType.InlineSig:
                case OperandType.InlineTok:
                case OperandType.InlineType:
                    i += 4;
                    break;
                case OperandType.InlineI8:
                case OperandType.InlineR:
                    i += 8;
                    break;
                case OperandType.ShortInlineR:
                    i += 4;
                    break;
                case OperandType.InlineSwitch:
                {
                    var count = BitConverter.ToInt32(il, i);
                    i += 4 + (count * 4);
                    break;
                }
                default:
                    throw new NotSupportedException($"Unsupported operand type while reading ldstr operands: {opcode.OperandType}");
            }
        }
    }

    private static IReadOnlyList<(int Index, string Raven, string Roslyn)> ComputeInstructionDifferences(
        IReadOnlyList<string> raven,
        IReadOnlyList<string> roslyn)
    {
        var count = Math.Max(raven.Count, roslyn.Count);
        var differences = new List<(int, string, string)>();

        for (var i = 0; i < count; i++)
        {
            var ravenInstruction = i < raven.Count ? raven[i] : "(missing)";
            var roslynInstruction = i < roslyn.Count ? roslyn[i] : "(missing)";

            if (!string.Equals(ravenInstruction, roslynInstruction, StringComparison.Ordinal))
            {
                differences.Add((i, ravenInstruction, roslynInstruction));
                if (differences.Count >= 25)
                    break;
            }
        }

        return differences;
    }

    private static void DeleteArtifacts(CompilationArtifacts artifacts)
    {
        if (artifacts.RuntimeConfigPath is not null && File.Exists(artifacts.RuntimeConfigPath))
            File.Delete(artifacts.RuntimeConfigPath);
        if (artifacts.DepsPath is not null && File.Exists(artifacts.DepsPath))
            File.Delete(artifacts.DepsPath);
        if (artifacts.PdbPath is not null && File.Exists(artifacts.PdbPath))
            File.Delete(artifacts.PdbPath);
    }

    private static string? FindRepositoryRoot()
    {
        var directory = new DirectoryInfo(AppContext.BaseDirectory);
        while (directory is not null)
        {
            var candidate = Path.Combine(directory.FullName, "Raven.sln");
            if (File.Exists(candidate))
                return directory.FullName;

            directory = directory.Parent;
        }

        return null;
    }

    private static void WriteReport(
        string outputPath,
        PointerRegressionResult pointerResult,
        IlDiffResult ilResult,
        IReadOnlyList<(string Field, string Operation)> baseline,
        string stepLabel)
    {
        var builder = new StringBuilder();
        builder.AppendLine("# Async entry nightly diff");
        builder.AppendLine();
        builder.AppendLine($"_Last generated: {DateTime.UtcNow:yyyy-MM-dd HH:mm:ss} UTC_");
        builder.AppendLine();

        builder.AppendLine("## Pointer trace regression");
        builder.AppendLine();
        builder.AppendLine($"- Compilation: {FormatStatus(pointerResult.CompilationSucceeded)} (exit {pointerResult.Compilation.ExitCode})");
        builder.AppendLine($"- Execution: {FormatStatus(pointerResult.ExecutionSucceeded)} (exit {pointerResult.Execution.ExitCode})");
        builder.AppendLine($"- Runtime pointer timeline: {FormatStatus(pointerResult.RuntimeMatchesBaseline)} ({pointerResult.RuntimeTimeline.Count} records)");
        builder.AppendLine($"- Emitted IL pointer timeline: {FormatStatus(pointerResult.IlMatchesBaseline)} ({pointerResult.IlTimeline.Count} records)");
        builder.AppendLine();

        builder.AppendLine("Baseline timeline:");
        builder.AppendLine("```");
        foreach (var entry in baseline)
            builder.AppendLine($"{stepLabel}:{entry.Field}:{entry.Operation}");
        builder.AppendLine("```");
        builder.AppendLine();

        builder.AppendLine("Runtime pointer timeline:");
        builder.AppendLine("```");
        foreach (var record in pointerResult.RuntimeRecords)
            builder.AppendLine($"{stepLabel}:{record.Field}:{record.Operation} -> 0x{record.Address:X}");
        builder.AppendLine("```");
        builder.AppendLine();

        builder.AppendLine("Emitted IL pointer timeline:");
        builder.AppendLine("```");
        foreach (var entry in pointerResult.IlTimeline)
            builder.AppendLine($"{stepLabel}:{entry.Field}:{entry.Operation}");
        builder.AppendLine("```");
        builder.AppendLine();

        if (!string.IsNullOrWhiteSpace(pointerResult.Compilation.StdErr))
        {
            builder.AppendLine("Compiler stderr:");
            builder.AppendLine("```");
            builder.AppendLine(pointerResult.Compilation.StdErr.Trim());
            builder.AppendLine("```");
            builder.AppendLine();
        }

        if (!string.IsNullOrWhiteSpace(pointerResult.Execution.StdErr))
        {
            builder.AppendLine("Execution stderr:");
            builder.AppendLine("```");
            builder.AppendLine(pointerResult.Execution.StdErr.Trim());
            builder.AppendLine("```");
            builder.AppendLine();
        }

        builder.AppendLine("## Raven vs. Roslyn MoveNext IL");
        builder.AppendLine();
        builder.AppendLine($"- Raven compilation: {FormatStatus(ilResult.RavenCompilationSucceeded)} (exit {ilResult.RavenCompilation.ExitCode})");
        builder.AppendLine($"- Roslyn compilation: {FormatStatus(ilResult.RoslynCompilationSucceeded)} (exit {ilResult.RoslynCompilation.ExitCode})");
        builder.AppendLine($"- Instruction comparison: {FormatStatus(ilResult.Differences.Count == 0)} ({ilResult.RavenInstructions.Count} vs. {ilResult.RoslynInstructions.Count})");
        builder.AppendLine();

        if (ilResult.Differences.Count > 0)
        {
            builder.AppendLine("First differences:");
            builder.AppendLine();
            builder.AppendLine("| Index | Raven | Roslyn |");
            builder.AppendLine("| --- | --- | --- |");
            foreach (var diff in ilResult.Differences)
                builder.AppendLine($"| {diff.Index} | `{EscapeMarkdown(diff.Raven)}` | `{EscapeMarkdown(diff.Roslyn)}` |");
            builder.AppendLine();
        }

        builder.AppendLine("Raven MoveNext IL:");
        builder.AppendLine("```");
        foreach (var instruction in ilResult.RavenInstructions)
            builder.AppendLine(instruction);
        builder.AppendLine("```");
        builder.AppendLine();

        builder.AppendLine("Roslyn MoveNext IL:");
        builder.AppendLine("```");
        foreach (var instruction in ilResult.RoslynInstructions)
            builder.AppendLine(instruction);
        builder.AppendLine("```");
        builder.AppendLine();

        if (!string.IsNullOrWhiteSpace(ilResult.RavenCompilation.StdErr))
        {
            builder.AppendLine("Raven compiler stderr:");
            builder.AppendLine("```");
            builder.AppendLine(ilResult.RavenCompilation.StdErr.Trim());
            builder.AppendLine("```");
            builder.AppendLine();
        }

        if (!string.IsNullOrWhiteSpace(ilResult.RoslynCompilation.StdErr))
        {
            builder.AppendLine("Roslyn compiler stderr:");
            builder.AppendLine("```");
            builder.AppendLine(ilResult.RoslynCompilation.StdErr.Trim());
            builder.AppendLine("```");
            builder.AppendLine();
        }

        File.WriteAllText(outputPath, builder.ToString());
    }

    private static string FormatStatus(bool succeeded) => succeeded ? "✅" : "❌";

    private static string EscapeMarkdown(string value)
        => value.Replace("|", "\\|", StringComparison.Ordinal)
            .Replace("`", "\\`", StringComparison.Ordinal);

    private static (string Field, string Operation, ulong Address) ParsePointerRecord(string line)
    {
        var firstColon = line.IndexOf(':');
        var secondColon = line.IndexOf(':', firstColon + 1);
        var arrowIndex = line.IndexOf(" -> ", StringComparison.Ordinal);

        if (firstColon < 0 || secondColon < 0 || arrowIndex < 0 || secondColon <= firstColon)
            throw new FormatException($"Unexpected pointer trace format: '{line}'.");

        var field = line[(firstColon + 1)..secondColon];
        var operation = line[(secondColon + 1)..arrowIndex].Trim();
        var addressText = line[(arrowIndex + 4)..];

        if (addressText.StartsWith("0x", StringComparison.OrdinalIgnoreCase))
            addressText = addressText[2..];

        var address = Convert.ToUInt64(addressText, 16);
        return (field, operation, address);
    }

    private static (string Field, string Operation) ParsePointerFormat(string value)
    {
        var firstColon = value.IndexOf(':');
        var secondColon = value.IndexOf(':', firstColon + 1);
        var arrowIndex = value.IndexOf(" -> ", StringComparison.Ordinal);

        if (firstColon < 0 || secondColon < 0 || arrowIndex < 0 || secondColon <= firstColon)
            throw new FormatException($"Unexpected pointer format: '{value}'.");

        var field = value[(firstColon + 1)..secondColon];
        var operation = value[(secondColon + 1)..arrowIndex].Trim();
        return (field, operation);
    }

    private readonly record struct CompilationArtifacts(
        string AssemblyPath,
        string? RuntimeConfigPath,
        string? DepsPath,
        string? PdbPath);

    private readonly record struct CompilationResult(ProcessResult Process, CompilationArtifacts Artifacts);

    private readonly record struct RoslynCompilationResult(ProcessResult Process, string AssemblyPath);

    private readonly record struct ProcessResult(int ExitCode, string StdOut, string StdErr);

    private readonly record struct PointerRegressionResult(
        bool CompilationSucceeded,
        bool ExecutionSucceeded,
        bool RuntimeMatchesBaseline,
        bool IlMatchesBaseline,
        IReadOnlyList<(string Field, string Operation, ulong Address)> RuntimeRecords,
        IReadOnlyList<(string Field, string Operation)> RuntimeTimeline,
        IReadOnlyList<(string Field, string Operation)> IlTimeline,
        ProcessResult Compilation,
        ProcessResult Execution)
    {
        public bool Passed =>
            CompilationSucceeded &&
            ExecutionSucceeded &&
            RuntimeMatchesBaseline &&
            IlMatchesBaseline;

        public int CompilationExitCode => Compilation.ExitCode;

        public int ExecutionExitCode => Execution.ExitCode;
    }

    private readonly record struct IlDiffResult(
        bool RavenCompilationSucceeded,
        bool RoslynCompilationSucceeded,
        IReadOnlyList<string> RavenInstructions,
        IReadOnlyList<string> RoslynInstructions,
        IReadOnlyList<(int Index, string Raven, string Roslyn)> Differences,
        ProcessResult RavenCompilation,
        ProcessResult RoslynCompilation)
    {
        public bool Passed => RavenCompilationSucceeded && RoslynCompilationSucceeded && Differences.Count == 0;
    }
}

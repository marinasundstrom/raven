using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class ParserTokenInsertionFuzzTests
{
    private static readonly string[] BaseSamples = LoadBaseSamples();

    private static readonly string[] Insertions =
    [
        "}",
        "{",
        "self",
        "public",
        "=>",
        "->",
        "<",
        ">",
        ".",
        ",",
        ";",
        "(",
        ")",
        "[",
        "]",
        "trait",
        "class",
        "namespace",
        "import",
        "@"
    ];

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void RandomSingleTokenMutation_DoesNotHangOrThrow(bool insertMode)
    {
        const int seedCount = 12;
        var timeout = TimeSpan.FromSeconds(2);

        for (var sampleIndex = 0; sampleIndex < BaseSamples.Length; sampleIndex++)
        {
            var baseSample = BaseSamples[sampleIndex];

            for (var seed = 0; seed < seedCount; seed++)
            {
                var mutated = Mutate(baseSample, seed, insertMode);

                var parseTask = Task.Run(() => SyntaxTree.ParseText(mutated));
                var completed = parseTask.Wait(timeout);
                Assert.True(
                    completed,
                    $"Parser timed out for sample {sampleIndex}, seed {seed}, mode {(insertMode ? "insert" : "delete")}. Mutated source:{Environment.NewLine}{mutated}");

                var tree = parseTask.Result;
                var root = tree.GetRoot();
                var eof = root.GetLastToken(includeZeroWidth: true);

                Assert.Equal(
                    SyntaxKind.EndOfFileToken,
                    eof.Kind);
            }
        }
    }

    private static string Mutate(string source, int seed, bool insertMode)
    {
        var random = new Random(seed);

        if (insertMode)
        {
            var insertAt = random.Next(source.Length + 1);
            var token = Insertions[random.Next(Insertions.Length)];

            var tokenText = NeedsPadding(token)
                ? $" {token} "
                : token;

            return source.Insert(insertAt, tokenText);
        }

        if (source.Length == 0)
            return source;

        var deleteAt = random.Next(source.Length);
        return source.Remove(deleteAt, 1);
    }

    private static bool NeedsPadding(string token)
    {
        return token.Length > 1 && char.IsLetter(token[0]);
    }

    private static string[] LoadBaseSamples()
    {
        var repoRoot = FindRepoRoot();
        var samplePaths = new[]
        {
            Path.Combine(repoRoot, "samples", "lambda.rav"),
            Path.Combine(repoRoot, "samples", "json2.rav"),
            Path.Combine(repoRoot, "samples", "result.rav"),
        };

        return samplePaths
            .Where(File.Exists)
            .Select(File.ReadAllText)
            .Where(content => !string.IsNullOrWhiteSpace(content))
            .ToArray();
    }

    private static string FindRepoRoot()
    {
        var current = new DirectoryInfo(AppContext.BaseDirectory);

        while (current is not null)
        {
            if (Directory.Exists(Path.Combine(current.FullName, "samples")) &&
                Directory.Exists(Path.Combine(current.FullName, "src")) &&
                Directory.Exists(Path.Combine(current.FullName, "test")))
            {
                return current.FullName;
            }

            current = current.Parent;
        }

        throw new InvalidOperationException("Unable to locate repository root for sample-based parser fuzz tests.");
    }
}

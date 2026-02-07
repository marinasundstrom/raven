using System;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class ParserTokenInsertionFuzzTests
{
    private static readonly string[] BaseSamples =
    [
        """
        import System.*

        let x = 1
        class Foo {
            public Value: int => x
            public self(flag: bool) -> int {
                if flag { return Value }
                return 0
            }
        }
        """,
        """
        namespace Demo;

        let answer = 42
        class Bar {
            public Hello() -> string {
                return "hi"
            }
        }
        """,
        """
        class C {
            public M(x: int) -> int {
                let y = x + 1
                return y
            }
        }
        """
    ];

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

    [Fact]
    public void RandomTokenInsertion_DoesNotHangOrThrow()
    {
        const int seedCount = 40;
        var timeout = TimeSpan.FromSeconds(2);

        for (var sampleIndex = 0; sampleIndex < BaseSamples.Length; sampleIndex++)
        {
            var baseSample = BaseSamples[sampleIndex];

            for (var seed = 0; seed < seedCount; seed++)
            {
                var mutated = Mutate(baseSample, seed);

                var parseTask = Task.Run(() => SyntaxTree.ParseText(mutated));
                var completed = parseTask.Wait(timeout);
                Assert.True(
                    completed,
                    $"Parser timed out for sample {sampleIndex}, seed {seed}. Mutated source:{Environment.NewLine}{mutated}");

                var tree = parseTask.Result;
                var root = tree.GetRoot();
                var eof = root.GetLastToken(includeZeroWidth: true);

                Assert.Equal(
                    SyntaxKind.EndOfFileToken,
                    eof.Kind);
            }
        }
    }

    private static string Mutate(string source, int seed)
    {
        var random = new Random(seed);
        var insertAt = random.Next(source.Length + 1);
        var token = Insertions[random.Next(Insertions.Length)];

        var tokenText = NeedsPadding(token)
            ? $" {token} "
            : token;

        return source.Insert(insertAt, tokenText);
    }

    private static bool NeedsPadding(string token)
    {
        return token.Length > 1 && char.IsLetter(token[0]);
    }
}

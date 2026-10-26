using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

var source = SourceText.From(string.Concat(Enumerable.Repeat("let identifier = 123456789\n", 2048)));
var change = new TextChange(new TextSpan(source.Length / 2, 1), "x");
var identifiers = string.Concat(Enumerable.Range(0, 256).Select(i => $"let identifier{i} = true\n"));
var multiline = "let text = \"\"\"\n" + string.Concat(Enumerable.Repeat("    some multiline content\n", 256)) + "    \"\"\"\n";
Measure($"source edit ({source.Length:N0} chars)", () => source.WithChange(change));
Measure("parse 256 identifier declarations", () => SyntaxTree.ParseText(identifiers));
Measure("parse 256 indented string lines", () => SyntaxTree.ParseText(multiline));

static void Measure(string name, Func<object> operation)
{
    for (var i = 0; i < 100; i++) GC.KeepAlive(operation());
    const int iterations = 500;
    for (var round = 0; round < 3; round++)
    {
        GC.Collect();
        GC.WaitForPendingFinalizers();
        GC.Collect();
        var before = GC.GetAllocatedBytesForCurrentThread();
        for (var i = 0; i < iterations; i++) GC.KeepAlive(operation());
        var allocated = GC.GetAllocatedBytesForCurrentThread() - before;
        Console.WriteLine($"{name}: {allocated / iterations} bytes/op (round {round + 1})");
    }
}

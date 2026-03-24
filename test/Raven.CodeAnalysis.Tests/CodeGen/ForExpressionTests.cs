using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class ForExpressionTests
{
    [Fact]
    public void For_OverArray_UsesTypedElement()
    {
        var code = """
class Foo {
    func Run() -> int {
        var items = [1, 2, 3]
        var total: int = 0
        for item in items {
            total = total + item
        }
        return total
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Foo", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
        Assert.NotNull(method);
        var value = (int)method!.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal(6, value);
    }

    [Fact]
    public void ForRange_IsInclusive_ForLiteralAndVariableBounds()
    {
        var code = """
import System.Console.*

for x in 2..6 {
    WriteLine(x)
}

val bottom = -3
val top = 7

for x in bottom..top {
    WriteLine(x)
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(
            ["2", "3", "4", "5", "6", "-3", "-2", "-1", "0", "1", "2", "3", "4", "5", "6", "7"],
            output);
    }

    [Fact]
    public void For_WithExtensionGetEnumerator_EmitsAndRuns()
    {
        var code = """
import System.Console.*
import System.Collections.Generic.*

class Box {
    val Items: List<int> = [42]
}

extension BoxEnumerable for Box {
    func GetEnumerator() -> IEnumerator<int> {
        val values: IEnumerable<int> = self.Items
        return values.GetEnumerator()
    }
}

class Program {
    static func Main() {
        val box = Box()
        for value in box {
            WriteLine(value)
        }
    }
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(["42"], output);
    }

    [Fact]
    public void ForRange_SupportsCharAndDecimalBounds()
    {
        var code = """
import System.Console.*

for c in 'a'..'c' {
    WriteLine(c)
}

val start: decimal = 1
val end: decimal = 3

for n in start..end {
    WriteLine(n)
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(["a", "b", "c", "1", "2", "3"], output);
    }

    [Fact]
    public void ForRange_ByClause_SupportsPositiveAndNegativeIntegerSteps()
    {
        var code = """
import System.Console.*

for x in 0..10 by 2 {
    WriteLine(x)
}

for x in 10..0 by -3 {
    WriteLine(x)
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(["0", "2", "4", "6", "8", "10", "10", "7", "4", "1"], output);
    }

    [Fact]
    public void ForRange_ByClause_SupportsFractionalStep()
    {
        var code = """
import System.Console.*

for x in 0..1.0 by 0.5 {
    WriteLine(x)
}
""";

        var output = CompileAndRun(code);
        var normalized = output.Select(static value => value.Replace(',', '.')).ToArray();
        Assert.Equal(["0", "0.5", "1"], normalized);
    }

    [Fact]
    public void ForRange_WithExclusiveUpperBound_IsHalfOpen()
    {
        var code = """
import System.Console.*

for x in 2..<6 {
    WriteLine(x)
}

for x in 6..<2 by -2 {
    WriteLine(x)
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(["2", "3", "4", "5", "6", "4"], output);
    }

    [Fact]
    public void For_WithSequencePattern_BareFixedSegment_CanSkipWithoutDiscardToken()
    {
        var code = """
import System.Console.*

for val [..2, x, ...] in [[2, 1..2, 5]] {
    WriteLine(x)
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(["2"], output);
    }

    [Fact]
    public void For_WithShadowedLoopVariableAndPatternSegmentCapture_EmitsValidProgram()
    {
        var code = """
import System.Console.*

for x in [2, 1..2] {
    WriteLine(x)
}

for val [..2, ..2 x, ...] in [[2, 1..3]] {
    WriteLine(x.Count)
    WriteLine(x[0])
    WriteLine(x[1])
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(["2", "1", "2", "2", "2", "3"], output);
    }

    [Fact]
    public void For_WithGuardedBindingPattern_EmitsAndRuns()
    {
        var code = """
import System.Console.*

record Order(val Id: int, val Amount: int)

class Program {
    static func Main() {
        val orders = [
            Order(1001, 120)
            Order(1002, 80)
            Order(1003, 210)
        ]

        for val (id, amount when > 100) in orders {
            WriteLine("${id}:${amount}")
        }
    }
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(["1001:120", "1003:210"], output);
    }

    [Fact]
    public void For_WithFixedArrayLiteralAndFixedSegmentCapture_EmitsValidProgram()
    {
        var code = """
import System.Console.*

for val [..2, ..2 x, ...] in [[|2, 1..4|]] {
    WriteLine(x[1])
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(["3"], output);
    }

    [Fact]
    public void For_BreakAndContinue_Work()
    {
        var code = """
class C {
    static func Sum() -> int {
        var values = [1, 2, 3, 4, 5]
        var total: int = 0
        for value in values {
            if value == 2 {
                continue
            }

            if value == 5 {
                break
            }

            total = total + value
        }

        return total
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("for-break-continue-sync", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("C", true)!;
        var method = type.GetMethod("Sum", BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic);
        Assert.NotNull(method);

        var value = (int)method!.Invoke(null, Array.Empty<object>())!;
        Assert.Equal(8, value);
    }

    [Fact]
    public void AwaitFor_BreakAndContinue_Emits()
    {
        var code = """
import System.Collections.Generic.*
import System.Threading.Tasks.*

class C {
    static async func Values() -> IAsyncEnumerable<int> {
        yield return 1
        yield return 2
        yield return 3
        yield return 4
        yield return 5
    }

    static async func Sum() -> Task<int> {
        var total: int = 0

        await for value in Values() {
            if value == 2 {
                continue
            }

            if value == 5 {
                break
            }

            total = total + value
        }

        return total
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("for-break-continue-async", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        Assert.NotNull(loaded.Assembly.GetType("C", throwOnError: false));
    }

    [Fact]
    public void For_OverGenericIEnumerable_UsesTypedCurrentGetter()
    {
        // Regression test: IEnumerator<T>.Current was resolved to the non-generic
        // IEnumerator.Current (returning object), causing castclass !!T instead of the typed
        // get_Current(), which produced the boxed-int heap address instead of the unboxed value.
        var code = """
import System.Collections.Generic.*

class C {
    static func Sum(source: IEnumerable<int>) -> int {
        var total: int = 0
        for item in source {
            total = total + item
        }
        return total
    }

    static func Run() -> int {
        val items: IEnumerable<int> = [1, 2, 3]
        return Sum(items)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("for-generic-enumerable", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("C", true)!;
        var method = type.GetMethod("Run", BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic);
        Assert.NotNull(method);
        var value = (int)method!.Invoke(null, Array.Empty<object>())!;
        Assert.Equal(6, value);
    }

    [Fact]
    public void GenericForEach_WithDictionaryArgument_IteratesAllEntries()
    {
        // Regression: ForEach<T>(IEnumerable<T>, T -> ()) called with Dictionary<string, int>
        // must infer T = KeyValuePair<string, int> and iterate the correct number of entries.
        // Previously T stayed as the open KeyValuePair<TKey, TValue> causing a BadImageFormatException.
        // Note: Raven uses value-based lambda capture, so we verify iteration count via stdout.
        var code = """
import System.Console.*
import System.Collections.Generic.*
import System.Linq.*

val dict = ["a", "bb", "ccc"].ToDictionary(x => x, y => y.Length)
ForEach(dict, item => WriteLine("tick"))

func ForEach<T>(source: IEnumerable<T>, callback: T -> ()) -> () {
    for item in source {
        callback(item)
    }
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(3, output.Length);
        Assert.All(output, line => Assert.Equal("tick", line));
    }

    [Fact]
    public void GenericForEach_WithDictionaryAndMatchingMethodReference_CallsCallbackForEachEntry()
    {
        // ForEach(dict, Print) where Print accepts KeyValuePair<string, int> exactly —
        // the method reference must resolve to the right overload without a boxing thunk.
        var code = """
import System.Console.*
import System.Collections.Generic.*
import System.Linq.*

func Main() -> () {
    val dict = ["a", "bb"].ToDictionary(x => x, y => y.Length)
    ForEach(dict, PrintEntry)
}

func PrintEntry(item: KeyValuePair<string, int>) -> () {
    WriteLine(item.Value)
}

func ForEach<T>(source: IEnumerable<T>, callback: T -> ()) -> () {
    for item in source {
        callback(item)
    }
}
""";

        var output = CompileAndRun(code);

        // Dictionary iteration order is not guaranteed, so sort before asserting.
        var sorted = output.OrderBy(x => x).ToArray();
        Assert.Equal(["1", "2"], sorted);
    }

    [Fact]
    public void For_WithPatternTarget_FiltersAndBinds()
    {
        var code = """
import System.Console.*

val points = [(0, 0), (1, 0), (1, 1), (2, 0)]

for (val x, 0) in points {
    WriteLine(x)
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(["0", "1", "2"], output);
    }

    private static string[] CompileAndRun(string code, OutputKind outputKind = OutputKind.ConsoleApplication)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("for-range", new CompilationOptions(outputKind))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var entryPoint = loaded.Assembly.EntryPoint!;

        var originalOut = Console.Out;
        using var writer = new StringWriter();
        try
        {
            Console.SetOut(writer);
            var parameters = entryPoint.GetParameters().Length == 0
                ? null
                : new object?[] { Array.Empty<string>() };
            entryPoint.Invoke(null, parameters);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        return writer.ToString().Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);
    }
}

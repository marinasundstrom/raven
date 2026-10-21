using System;
using System.IO;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class PositionalPatternCodeGenTests
{
    [Fact]
    public void RecordStructEqualityHandlesTypedBoxedNullAndWrongType()
    {
        const string code = """
            import System.*
            record struct Value(Number: int)
            func Main() {
                let value = Value(42)
                Console.WriteLine(value.Equals(Value(42)))
                Console.WriteLine(value.Equals(Value(43)))
                let boxed: object = value
                Console.WriteLine(boxed.Equals(Value(42)))
                Console.WriteLine(boxed.Equals(Value(43)))
                Console.WriteLine(boxed.Equals(null))
                Console.WriteLine(boxed.Equals(42))
            }
            """;
        Assert.Equal("True\nFalse\nTrue\nFalse\nFalse\nFalse", CompileAndRun(code, "record_struct_equality"));
    }

    [Theory]
    [InlineData("record struct")]
    [InlineData("record class")]
    public void NamedAndUnnamedPatternsHandleObjectAndNull(string kind)
    {
        var code = $$"""
            import System.*
            {{kind}} Pair(First: int, Second: int)
            func Named(value: object?) -> int {
                let Pair(first, 2) = value else { return -1 }
                return first
            }
            func Unnamed(value: Pair) -> int {
                let (first, 2) = value else { return -1 }
                return first
            }
            func Main() {
                let value = Pair(42, 2)
                let Pair(first, second) = value
                let (a, b) = value
                Console.WriteLine(first + second == a + b)
                Console.WriteLine(Named(value))
                Console.WriteLine(Named(Pair(42, 3)))
                Console.WriteLine(Named("wrong"))
                Console.WriteLine(Named(null))
                Console.WriteLine(Unnamed(value))
                Console.WriteLine(Unnamed(Pair(42, 3)))
            }
            """;
        Assert.Equal("True\n42\n-1\n-1\n-1\n42\n-1", CompileAndRun(code, "positional_pattern_matrix"));
    }

    [Fact]
    public void DerivedPatternsNarrowBaseAndObjectBeforeInheritedDeconstruction()
    {
        const string code = """
            import System.*
            open class Parent {
                func Deconstruct(out first: int, out second: int) {
                    first = 40
                    second = 2
                }
            }
            class Child : Parent {}
            func Read(value: Parent) -> int {
                let Child(first, second) = value else { return -1 }
                return first + second
            }
            func ReadObject(value: object?) -> int {
                let Child(first, second) = value else { return -1 }
                return first + second
            }
            func Main() {
                let child = Child()
                let Child(first, second) = child
                let (a, b) = child
                Console.WriteLine(first + second == a + b)
                Console.WriteLine(Read(child))
                Console.WriteLine(Read(Parent()))
                Console.WriteLine(ReadObject(child))
                Console.WriteLine(ReadObject(Parent()))
                Console.WriteLine(ReadObject(null))
            }
            """;
        Assert.Equal("True\n42\n-1\n42\n-1\n-1", CompileAndRun(code, "derived_positional_patterns"));
    }

    [Fact]
    public void QualifiedReadonlyValuePatternEvaluatesOnceAndCanFail()
    {
        const string code = """
            import System.*
            record struct Currency(Code: int) {
                static var Reads: int = 0
                static val SEK: Currency {
                    get {
                        Reads += 1
                        return Currency(1)
                    }
                }
            }
            record struct Money(Amount: decimal, Currency: Currency)
            func Read(money: Money) -> decimal {
                let Money(amount, Currency.SEK) = money else { return -1m }
                return amount
            }
            func Main() {
                Console.WriteLine(Read(Money(12m, Currency(1))))
                Console.WriteLine(Read(Money(12m, Currency(2))))
                Console.WriteLine(Currency.Reads)
            }
            """;
        Assert.Equal("12\n-1\n2", CompileAndRun(code, "qualified_readonly_value_pattern"));
    }

    [Fact]
    public void NamedAndTupleRecordDeclarationsPreserveValues()
    {
        const string code = """
            import System.*
            record struct Money(Amount: decimal, Currency: int)
            func Main() {
                let money = Money(12.50m, 1)
                let Money(amount, currency) = money
                let (otherAmount, otherCurrency) = money
                Console.WriteLine(amount == otherAmount && currency == otherCurrency)
                let Money(matched, 1) = money else { return }
                Console.WriteLine(matched == 12.50m)
            }
            """;
        Assert.Equal("True\nTrue", CompileAndRun(code, "named_record_declarations"));
    }

    [Theory]
    [InlineData("struct")]
    [InlineData("ref struct")]
    public void NominalValueDeconstruction_UsesCopy(string kind)
    {
        var code = $$"""
            import System.*

            {{kind}} Counter {
                public field Value: int

                public func Deconstruct(out value: int) -> unit {
                    value = self.Value
                    self.Value = 99
                }
            }

            func Main() {
                var counter = Counter()
                counter.Value = 42
                if counter is Counter(let value) {
                    Console.WriteLine(value)
                }
                Console.WriteLine(counter.Value)
            }
            """;

        Assert.Equal("42\n42", CompileAndRun(code, "nominal_value_deconstruction"));
    }

    [Fact]
    public void NominalReferenceDeconstruction_ChecksNullAndNarrowedType()
    {
        const string code = """
            import System.*

            class Counter {
                public field Value: int

                public func Deconstruct(out value: int) -> unit {
                    value = self.Value
                    self.Value = 99
                }
            }

            func Describe(value: object?) -> int {
                if value is Counter(let amount) {
                    return amount
                }
                return -1
            }

            func Main() {
                let counter = Counter()
                counter.Value = 42
                Console.WriteLine(Describe(counter))
                Console.WriteLine(counter.Value)
                Console.WriteLine(Describe(null))
                Console.WriteLine(Describe("other"))
            }
            """;

        Assert.Equal("42\n99\n-1\n-1", CompileAndRun(code, "nominal_reference_deconstruction"));
    }

    [Theory]
    [InlineData("(1, 2)", "3")]
    [InlineData("(1, 2, 3)", "no match")]
    [InlineData("(1, \"two\")", "no match")]
    [InlineData("42", "no match")]
    public void MatchExpression_WithPositionalPattern_EmitsAndRuns(string value, string expected)
    {
        var code = $$"""
import System.*

func describe(value: object) -> string {
    value match {
        (let first: int, let second: int) => "${first + second}"
        _ => "no match"
    }
}

func Main() {
    let value: object = {{value}}
    Console.WriteLine(describe(value))
}
""";

        Assert.Equal(expected, CompileAndRun(code, "tuple_pattern_emit"));
    }

    [Fact]
    public void LetPositionalPatternAssignment_EmitsAndRuns()
    {
        const string code = """
import System.*

func Main() {
    let (first, second) = (1, 2)
    Console.WriteLine(first)
    Console.WriteLine(second)
}
""";

        Assert.Equal("1\n2", CompileAndRun(code, "let_tuple_pattern_emit"));
    }

    [Fact]
    public void PositionalPatternAssignment_WithExistingLocals_EmitsAndRuns()
    {
        const string code = """
import System.*

func Main() {
    var first = 0
    var second = 0
    (first, second) = (1, 2)
    Console.WriteLine(first + second)
}
""";

        Assert.Equal("3", CompileAndRun(code, "tuple_pattern_existing_locals"));
    }

    [Fact]
    public void VarPositionalPatternAssignment_EmitsAndRuns()
    {
        const string code = """
import System.*

func Main() {
    (var first, var second) = (1, 2)
    Console.WriteLine(first + second)
}
""";

        Assert.Equal("3", CompileAndRun(code, "tuple_pattern_var_assignment"));
    }

    [Fact]
    public void MixedPositionalPatternAssignment_EmitsAndRuns()
    {
        const string code = """
import System.*

func Main() {
    let (first, ignoredFirst) = (1, 2)
    var (second, ignoredSecond) = (3, 4)
    Console.WriteLine(first)
    Console.WriteLine(second)
}
""";

        Assert.Equal("1\n3", CompileAndRun(code, "tuple_pattern_mixed_assignment"));
    }

    private static string CompileAndRun(string code, string assemblyName)
    {
        var tree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create(assemblyName, new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(references);

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var entryPoint = loaded.Assembly.EntryPoint;
        Assert.NotNull(entryPoint);

        using var writer = new StringWriter();
        var originalOut = Console.Out;
        try
        {
            Console.SetOut(writer);
            var arguments = entryPoint!.GetParameters().Length == 0
                ? null
                : new object?[] { Array.Empty<string>() };
            entryPoint.Invoke(null, arguments);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        return writer.ToString().ReplaceLineEndings("\n").TrimEnd('\n');
    }
}

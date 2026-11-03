using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

[Trait("Target", "DotNet")]
public class RecordStoredStateTests
{
    [Theory]
    [InlineData("class")]
    [InlineData("struct")]
    public void BodyStateParticipatesInEqualityHashingAndCopies(string kind)
    {
        var source = $$"""
            record {{kind}} Options {
                private field _secret: int
                init(value: int) {
                    _secret = value
                    Value = value
                }
                init() : self(3) { }
                val Value: int { get; init; }
                val Computed: int => throw System.InvalidOperationException("Equality must not evaluate getters")
                func Secret() -> int => _secret
            }
            class Probe {
                static func Run() -> bool {
                    let first = Options()
                    let equal = Options(3)
                    let other = Options(4)
                    let copy = first with { Value = 8 }
                    return first == equal && first.GetHashCode() == equal.GetHashCode() && first != other &&
                        copy.Secret() == 3 && copy.Value == 8 && first.Value == 3 && first != copy
                }
            }
            """;
        Assert.True(Run<bool>(source));
    }

    [Fact]
    public void PositionalRecordSupportsChainedConstructorsAndAdditionalStorage()
    {
        Assert.True(Run<bool>("""
            record class Item(Name: string) {
                init() : self("default") { Count = 2 }
                val Count: int { get; init; } = 1
            }
            class Probe {
                static func Run() -> bool {
                    let item = Item()
                    let copy = item with { Count = 3 }
                    return item.Name == "default" && item.Count == 2 && copy.Name == item.Name && copy != item
                }
            }
            """));
    }

    [Fact]
    public void DerivedRecordCopiesAndComparesPrivateBaseState()
    {
        Assert.Equal("True|True|True|4|2|1", Run<string>("""
            abstract record class Base {
                private field _id: int
                init(id: int) { _id = id }
                func Id() -> int => _id
            }
            record class Derived : Base {
                init(id: int) : base(id) { }
                val Extra: int { get; init; } = 1
            }
            class Probe {
                static func Run() -> string {
                    let first = Derived(4)
                    let equal = Derived(4)
                    let other = Derived(5)
                    let copy = first with { Extra = 2 }
                    return "${first == equal}|${first.GetHashCode() == equal.GetHashCode()}|${first != other}|${copy.Id()}|${copy.Extra}|${first.Extra}"
                }
            }
            """));
    }

    [Theory]
    [InlineData("class", "Options?")]
    [InlineData("struct", "Options")]
    public void CustomTypedEqualityControlsObjectAndOperatorEquality(string kind, string parameterType)
    {
        Assert.True(Run<bool>($$"""
            record {{kind}} Options(Value: int) {
                func Equals(other: {{parameterType}}) -> bool => other is Options value && Value % 10 == value.Value % 10
                override func GetHashCode() -> int => Value % 10
            }
            class Probe {
                static func Run() -> bool {
                    let first = Options(3)
                    let equal = Options(13)
                    let other = Options(4)
                    let boxed: object = first
                    let equatable: System.IEquatable<Options> = first
                    return first.Equals(equal) && boxed.Equals(equal) && equatable.Equals(equal)
                        && first == equal && !(first != equal) && first != other && !boxed.Equals(other)
                }
            }
            """));
    }

    [Fact]
    public void DerivedEqualityUsesCustomBaseEqualityAndDerivedState()
    {
        Assert.True(Run<bool>("""
            open record Base(Id: int) {
                virtual func Equals(other: Base?) -> bool => other is Base value && Id % 10 == value.Id % 10
                override func GetHashCode() -> int => Id % 10
            }
            record Derived(Id: int, Name: string) : Base(Id)
            class Probe {
                static func Run() -> bool {
                    let first = Derived(3, "same")
                    let equal = Derived(13, "same")
                    let other = Derived(13, "different")
                    let baseValue: Base = first
                    let boxed: object = first
                    return first == equal && first != other && baseValue.Equals(equal) && !baseValue.Equals(other)
                        && boxed.Equals(equal) && !boxed.Equals(other) && first.GetHashCode() == equal.GetHashCode()
                }
            }
            """));
    }

    [Fact]
    public void CustomTypedEqualityReceivesNullFromGeneratedMembers()
    {
        Assert.True(Run<bool>("""
            record Options {
                func Equals(other: Options?) -> bool => other is null
                override func GetHashCode() -> int => 0
            }
            class Probe {
                static func Run() -> bool {
                    let first = Options()
                    let none: Options? = null
                    let boxed: object = first
                    return boxed.Equals(null) && first == none && !(first != none) && !(none == first)
                }
            }
            """));
    }

    [Fact]
    public void UnionPayloadEqualityHonorsCustomRecordEquality()
    {
        Assert.True(Run<bool>("""
            record Reading(Value: int) {
                func Equals(other: Reading?) -> bool => other is Reading value && Value % 10 == value.Value % 10
                override func GetHashCode() -> int => Value % 10
            }
            union State {
                case Available(reading: Reading)
                case Missing
            }
            class Probe {
                static func Run() -> bool {
                    let first: State = State.Available(Reading(3))
                    let equal: State = State.Available(Reading(13))
                    let other: State = State.Available(Reading(4))
                    return first.Equals(equal) && !first.Equals(other)
                        && first.GetHashCode() == equal.GetHashCode()
                }
            }
            """));
    }

    private static T Run<T>(string source)
    {
        var compilation = Compilation.Create("RecordState" + Guid.NewGuid().ToString("N"),
            [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        return (T)loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null)!;
    }
}

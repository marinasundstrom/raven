using System.IO;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public class ImplicitIndexIndexerTests
{
    [Theory]
    [InlineData("values[^1]")]
    [InlineData("{ values[^1] = 42; values[^1] }")]
    [InlineData("{ values[^1] += 2; values[^1] }")]
    [InlineData("{ values[^1]++; values[^1] + 1 }")]
    public void ListFromEndReadAndMutation(string expression)
    {
        var initial = expression == "values[^1]" ? 42 : 40;
        AssertRuns($$"""
            import System.Collections.Generic.*
            class Probe {
                static func Run() -> int {
                    let values = List<int>()
                    values.Add({{initial}})
                    return {{expression}}
                }
            }
            """, 42);
    }

    [Fact]
    public void ExplicitIndexOverloadTakesPriority()
    {
        AssertRuns("""
            import System.*
            class Box {
                val Count: int => 10
                val self[index: int]: int => -1
                val self[index: Index]: int => 42
            }
            class Probe {
                static func Run() -> int => Box()[^1]
            }
            """, 42);
    }

    [Theory]
    [InlineData("class")]
    [InlineData("struct")]
    public void CountAndIndexAreEvaluatedOnceForCompoundAssignment(string kind)
    {
        AssertRuns(kind + """
             Box {
                var CountReads: int = 0
                var Value: int = 40
                val Count: int {
                    get { CountReads++; return 1 }
                }
                var self[index: int]: int {
                    get => Value
                    set => Value = value
                }
            }
            class Probe {
                static func Run() -> int {
                    var box = Box()
                    var indexReads = 0
                    func Next() -> int { indexReads++; return 1 }
                    box[^Next()] += 2
                    if box.CountReads != 1 || indexReads != 1 { return -1 }
                    return box.Value
                }
            }
            """, 42);
    }

    [Fact]
    public void ReceiverIndexLengthAndAccessorsKeepEvaluationOrder()
    {
        AssertRuns("""
            class Box {
                var Log: string = ""
                var Value: int = 40
                val Count: int { get { Log += "L"; return 1 } }
                var self[index: int]: int {
                    get { Log += "G"; return Value }
                    set { Log += "S"; Value = value }
                }
            }
            class Probe {
                static func Run() -> int {
                    let box = Box()
                    func Receiver() -> Box { box.Log += "R"; return box }
                    func Index() -> int { box.Log += "I"; return 1 }
                    func Value() -> int { box.Log += "V"; return 2 }
                    Receiver()[^Index()] += Value()
                    if box.Log != "RILGVS" { return -1 }
                    return box.Value
                }
            }
            """, 42);
    }

    [Fact]
    public void SemanticModelKeepsIndexerSymbolAndAuthoredIndexArgument()
    {
        var tree = SyntaxTree.ParseText("""
            import System.Collections.Generic.*
            class Probe {
                static func Read(values: List<int>) -> int => values[^1]
            }
            """);
        var compilation = Compilation.Create("ImplicitIndexSymbols", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var access = tree.GetRoot().DescendantNodes().OfType<ElementAccessExpressionSyntax>().Single();
        var model = compilation.GetSemanticModel(tree);
        Assert.True(Assert.IsAssignableFrom<PropertySymbol>(model.GetSymbolInfo(access).Symbol).IsIndexer);
        var operation = Assert.IsAssignableFrom<Raven.CodeAnalysis.Operations.IElementAccessOperation>(model.GetOperation(access));
        Assert.Single(operation.Arguments);
        Assert.Equal(SpecialType.System_Int32, model.GetTypeInfo(access).Type!.SpecialType);
    }

    [Fact]
    public void InheritedInterfaceCountAndIndexValueAreSupported()
    {
        AssertRuns("""
            import System.*
            import System.Collections.Generic.*
            class Probe {
                static func Read(values: IReadOnlyList<int>, index: Index) -> int => values[index]
                static func Run() -> int {
                    let values = List<int>()
                    values.Add(42)
                    return Read(values, ^1)
                }
            }
            """, 42);
    }

    [Theory]
    [InlineData("private val Active: bool => frames.Count == 0 || frames[^1].Active", "probe.Active")]
    [InlineData("private func Active() -> bool => frames.Count == 0 || frames[^1].Active", "probe.Active()")]
    public void NestedRecordIndexerInShortCircuitMember(string member, string access)
    {
        AssertRuns($$"""
            import System.Collections.Generic.*
            class Probe {
                private readonly field frames: List<Frame> = []
                {{member}}
                private record struct Frame(Active: bool) {}
                static func Run() -> int {
                    let probe = Probe()
                    if {{access}} { return 42 }
                    return -1
                }
            }
            """, 42, workspaceDiagnostics: true);
    }

    private static void AssertRuns(string source, int expected, bool workspaceDiagnostics = false)
    {
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("ImplicitIndexIndexer", [SyntaxTree.ParseText(source, path: "IndexProbe.rvn")], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        System.Collections.Immutable.ImmutableArray<Diagnostic>? diagnostics = null;
        if (workspaceDiagnostics)
        {
            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.AddProject("ImplicitIndexIndexer", compilationOptions: compilation.Options);
            var project = workspace.CurrentSolution.GetProject(projectId)!;
            foreach (var reference in references)
                project = project.AddMetadataReference(reference);
            project = project.AddDocument("IndexProbe.rvn", Raven.CodeAnalysis.Text.SourceText.From(source)).Project;
            Assert.True(workspace.TryApplyChanges(project.Solution));
            compilation = workspace.GetCompilation(projectId);
            diagnostics = workspace.GetDiagnostics(projectId);
        }
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream, null, diagnostics);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        Assert.Equal(expected, loaded.Assembly.GetType("Probe", true)!.GetMethod("Run")!.Invoke(null, null));
    }
}

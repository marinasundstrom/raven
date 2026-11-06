using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Syntax;
namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class CallerInfoTests
{
    [Theory]
    [InlineData("Capture()", "Run|/compiler/caller.rvn|4")]
    [InlineData("Raven.CodeAnalysis.Tests.CallerInfoCaptured().Text", "Run|/compiler/caller.rvn|4")]
    [InlineData("""Capture(member: "manual", file: "manual.rvn", line: 23)""", "manual|manual.rvn|23")]
    [InlineData("LineFirst()", "4")]
    [InlineData("FileFirst()", "/compiler/caller.rvn")]
    [InlineData("MemberFirst(1 + 2)", "Run")]
    public void ImportedCallerInformation(string expression, string expected)
    {
        var source = $$"""
  import Raven.CodeAnalysis.Tests.CallerInfoFixture.*
  public class Probe {

      static func Run() -> string => {{expression}}
  }
  """;
        Assert.Equal(expected, Run(source));
    }
    [Theory]
    [InlineData("static val Name: string { get => Member() }\nstatic func Run() -> string => Name", "Name")]
    [InlineData("static func Run() -> string { let get = func() -> string => Member(); return get() }", "Run")]
    [InlineData("static func Run() -> string { func Local() -> string => Member(); return Local() }", "Run")]
    [InlineData("val Name: string\ninit() { Name = Member() }\nstatic func Run() -> string => Probe().Name", ".ctor")]
    [InlineData("static val Name: string = Member()\nstatic func Run() -> string => Name", "Name")]
    [InlineData("val Name: string = Member()\nstatic func Run() -> string => Probe().Name", "Name")]
    public void CallerNameUsesEnclosingMember(string members, string expected)
    {
        Assert.Equal(expected, Run("import Raven.CodeAnalysis.Tests.CallerInfoFixture.*\npublic class Probe {\n" + members + "\n}"));
    }
    [Fact]
    public void SourceAttributesWorkWithNullableParameters()
    {
        Assert.Equal("Run|/compiler/caller.rvn|4", Run("""
  import System.Runtime.CompilerServices.*
  public class Probe {
      static func Capture([CallerMemberName] member: string? = null, [CallerFilePath] file: string? = null, [CallerLineNumber] line: int? = null) -> string => "${member}|${file}|${line}"
      static func Run() -> string => Capture()
  }
  """));
    }
    [Fact]
    public void CSharpPrecedenceOracle()
    {
        Assert.IsType<int>(CallerInfoFixture.LineValue());
        Assert.EndsWith("CallerInfoTests.cs", CallerInfoFixture.FileFirst());
        Assert.Equal(nameof(CSharpPrecedenceOracle), CallerInfoFixture.MemberFirst(1 + 2));
        string Local() => CallerInfoFixture.Member();
        Assert.Equal(nameof(CSharpPrecedenceOracle), Local());
        Func<string> lambda = () => CallerInfoFixture.Member();
        Assert.Equal(nameof(CSharpPrecedenceOracle), lambda());
        Assert.Equal("StaticName", CallerFieldOracle.StaticName);
        Assert.Equal("Name", new CallerFieldOracle().Name);
    }
    [Fact]
    public void BaseInitializerSuppliesCallerInformation()
    {
        Assert.Equal(".ctor", Run("""
  import System.Runtime.CompilerServices.*
  public open class Base {
      val Text: string

      init([CallerMemberName] text: string = "fallback") { Text = text }
  }
  public class Probe: Base {
      init(): base() {}

      static func Run() -> string => Probe().Text
  }
  """));
    }
    [Fact]
    public void AttributeConstructorSuppliesCallerInformation()
    {
        Assert.Equal("Run|/compiler/caller.rvn|4", Run("""
  import Raven.CodeAnalysis.Tests.*
  import Raven.CodeAnalysis.Tests.CallerInfoFixture.*
  public class Probe {
      [CallerStamp]
      static func Run() -> string => AttributeText(typeof(Probe))
  }
  """));
    }
    private static string Run(string source)
    {
        var tree = SyntaxTree.ParseText(source, path: "/compiler/caller.rvn");
        var c = Compilation.Create("CallerInfo", [tree], TestMetadataReferences.Default.Append(MetadataReference.CreateFromFile(typeof(CallerInfoFixture).Assembly.Location)).ToArray(), new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream(); var result = c.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, c.References);
        return (string)loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null)!;
    }
}
public static class CallerInfoFixture
{
    public static string AttributeText(Type type) => type.GetMethod("Run")!.GetCustomAttributes(typeof(CallerStampAttribute), false).Cast<CallerStampAttribute>().Single().Text;
    public static string Capture([CallerMemberName] string member = "fallback", [CallerFilePath] string file = "fallback", [CallerLineNumber] int line = 0) => $"{member}|{file}|{line}";
    public static string Member([CallerMemberName] string member = "fallback") => member;
    public static object LineValue([CallerMemberName, CallerFilePath, CallerLineNumber] object value = null!) => value;
    public static string LineFirst([CallerMemberName, CallerFilePath, CallerLineNumber] object value = null!) => value.ToString()!;
    public static string FileFirst([CallerMemberName, CallerFilePath] object value = null!) => value.ToString()!;
    public static string MemberFirst(int value, [CallerArgumentExpression("value"), CallerMemberName] string name = "fallback") => name;
}

public sealed class CallerInfoCaptured
{
    public string Text { get; }
    public CallerInfoCaptured([CallerMemberName] string member = "fallback", [CallerFilePath] string file = "fallback", [CallerLineNumber] int line = 0) { Text = $"{member}|{file}|{line}"; }
}

public sealed class CallerInfoSemanticTests
{
    [Fact, Trait("Target", "DotNet")]
    public void DotNet_FieldCallerNameUsesSharedBinding() => CheckField(TargetPlatform.DotNet);
    [Fact, Trait("Target", "NeoCLR")]
    public void NeoCLR_FieldCallerNameUsesSharedBinding() => CheckField(TargetPlatform.NeoCLR);
    private static void CheckField(TargetPlatform target)
    {
        var tree = SyntaxTree.ParseText("import Raven.CodeAnalysis.Tests.CallerInfoFixture.*\nclass Probe { static val Name: string = Member() }");
        var c = Compilation.Create("CallerFieldSemantic", [tree], TestMetadataReferences.Default.Append(MetadataReference.CreateFromFile(typeof(CallerInfoFixture).Assembly.Location)).ToArray(), new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        var model = c.GetSemanticModel(tree);
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        Assert.Equal("Member", Assert.IsAssignableFrom<MethodSymbol>(model.GetSymbolInfo(invocation).Symbol).Name);
        Assert.Equal(SpecialType.System_String, model.GetTypeInfo(invocation).Type?.SpecialType);
        Assert.IsAssignableFrom<Raven.CodeAnalysis.Operations.IInvocationOperation>(model.GetOperation(invocation));
        var bound = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.Equal("Name", Assert.IsType<BoundLiteralExpression>(bound.Arguments.Single()).Value);
    }

    [Fact, Trait("Target", "DotNet")]
    public void DotNet_CallerInformationUsesSharedBinding() => Check(TargetPlatform.DotNet);
    [Fact, Trait("Target", "NeoCLR")]
    public void NeoCLR_CallerInformationUsesSharedBinding() => Check(TargetPlatform.NeoCLR);
    private static void Check(TargetPlatform target)
    {
        var tree = SyntaxTree.ParseText("""
  import Raven.CodeAnalysis.Tests.CallerInfoFixture.*
  public class Probe {

      static func Run() -> string => Capture()
  }
  """, path: "/compiler/semantic-caller.rvn");
        var c = Compilation.Create("CallerInfoSemantic", [tree], TestMetadataReferences.Default.Append(MetadataReference.CreateFromFile(typeof(CallerInfoFixture).Assembly.Location)).ToArray(), new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        var m = c.GetSemanticModel(tree);
        Assert.DoesNotContain(m.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundInvocationExpression>(m.GetBoundNode(invocation));
        Assert.Equal("Run", Assert.IsType<BoundLiteralExpression>(bound.Arguments.ElementAt(0)).Value);
        Assert.Equal("/compiler/semantic-caller.rvn", Assert.IsType<BoundLiteralExpression>(bound.Arguments.ElementAt(1)).Value);
        Assert.Equal(4, Assert.IsType<BoundLiteralExpression>(bound.Arguments.ElementAt(2)).Value);
    }
}

public sealed class CallerStampAttribute : Attribute
{
    public string Text { get; }
    public CallerStampAttribute([CallerMemberName] string member = "fallback", [CallerFilePath] string file = "fallback", [CallerLineNumber] int line = 0) { Text = $"{member}|{file}|{line}"; }
}

public sealed class CallerFieldOracle
{
    public static readonly string StaticName = CallerInfoFixture.Member();
    public readonly string Name = CallerInfoFixture.Member();
}

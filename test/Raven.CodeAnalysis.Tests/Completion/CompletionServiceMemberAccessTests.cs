using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Completion;

public class CompletionServiceMemberAccessTests
{
    [Fact]
    public void GetCompletions_AfterDot_OnType_ReturnsStaticMembers()
    {
        var code = """
import System.*;

string.
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.InteropServices.dll")),
                MetadataReference.CreateFromFile(typeof(System.Runtime.InteropServices.Marshal).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "IsNullOrEmpty");
        Assert.DoesNotContain(items, i => i.DisplayText == "Length");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnType_IncludesStaticExtensionMethodsAndProperties()
    {
        var code = """
class Counter { }

extension CounterExtensions for Counter {
    static func Build() -> Counter {
        return Counter()
    }

    static val Name: string {
        get { return "counter"; }
    }
}

Counter.
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        compilation.EnsureSetup();

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Build" && i.InsertionText == "Build()");
        Assert.Contains(items, i => i.DisplayText == "Name" && i.InsertionText == "Name");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnQualifiedNamespaceInTypeAnnotation_ReturnsNestedTypes()
    {
        var code = """
import System.*
import System.IO.*

class SessionsStorage(
    private val file: System.IO.
) {
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "File");
        Assert.Contains(items, i => i.DisplayText == "FileInfo");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnConstrainedTypeParameter_ReturnsStaticConstraintMembers()
    {
        var code = """
import System.*;

func Parse<T>(text: string) -> T
    where T: IParsable<T>
{
    T.
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Parse");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnVariable_ReturnsInstanceMembers()
    {
        var code = """
import System.*;

val text = "";
text.
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.InteropServices.dll")),
                MetadataReference.CreateFromFile(typeof(System.Runtime.InteropServices.Marshal).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Length");
        Assert.DoesNotContain(items, i => i.DisplayText == "IsNullOrEmpty");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnVariable_IncludesExtensionProperties()
    {
        var code = """
class Counter { }

extension CounterExtensions for Counter {
    func Increment() -> int {
        return 1
    }

    val Total: int {
        get { return 42; }
    }
}

val counter = Counter()
counter.
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        compilation.EnsureSetup();

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Increment" && i.InsertionText == "Increment()");
        Assert.Contains(items, i => i.DisplayText == "Total" && i.InsertionText == "Total");
    }

    [Fact]
    public void GetCompletions_AfterDot_WithPriorDeconstruction_DoesNotThrow()
    {
        var code = """
import System.*;

func GetResult() -> (string, int) {
    return ("hello", 42)
}

func Main() -> unit {
    val result = GetResult()
    val [first, _] = result
    result.
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf(".", StringComparison.Ordinal) + 1;

        var exception = Record.Exception(() => service.GetCompletions(compilation, syntaxTree, position).ToList());
        Assert.Null(exception);
    }

    [Fact]
    public void GetCompletions_AfterDot_BeforeAddAssignment_ReturnsInstanceMembers()
    {
        var code = """
import System.*;

class User {
    public event Changed: System.Action?;
    public val Name: string = ""
}

func Main() -> unit {
    val user = User()
    user. += () => ()
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.IndexOf("user.", StringComparison.Ordinal) + "user.".Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Changed");
        Assert.Contains(items, i => i.DisplayText == "Name");
    }

    [Fact]
    public void GetCompletions_AfterDot_BeforeSimpleAssignment_ReturnsInstanceMembers()
    {
        var code = """
import System.*;

class User {
    public val Name: string = ""
}

func Main() -> unit {
    val user = User()
    user. = 2
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.IndexOf("user.", StringComparison.Ordinal) + "user.".Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Name");
    }

    [Fact]
    public void GetCompletions_AfterConditionalAccessBeforeSimpleAssignment_ReturnsInstanceMembers()
    {
        var code = """
import System.*;

class User {
    public val Name: string = ""
}

func Main() -> unit {
    val user = User()
    user?. = 2
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.IndexOf("user?.", StringComparison.Ordinal) + "user?.".Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Name");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnLiteralType_ReturnsInstanceMembers()
    {
        var code = """
import System.*;

val literal = "foo";
literal.
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.InteropServices.dll")),
                MetadataReference.CreateFromFile(typeof(System.Runtime.InteropServices.Marshal).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Length");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnProperty_ReturnsInstanceMembers()
    {
        var code = """
import System.*;

Console.Out.
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.InteropServices.dll")),
                MetadataReference.CreateFromFile(typeof(System.Runtime.InteropServices.Marshal).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "WriteLine");
        Assert.DoesNotContain(items, i => i.DisplayText == "Synchronized");
    }

    [Fact]
    public void GetCompletions_AfterDot_InInvocationArgument_DoesNotFallbackToUnrelatedSymbols()
    {
        var code = """
class Person {
    func Rename(first: string) -> unit { }
}

func WriteLine(value: Person) -> unit { }

val bob = Person()
WriteLine(bob.)
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.DoesNotContain(items, i => i.DisplayText == "Abs");
        Assert.DoesNotContain(items, i => i.DisplayText == "AddRange");
    }

    [Fact]
    public void GetCompletions_AfterDot_IncludesExtensionMethods()
    {
        var code = """
import System.Collections.Generic.*;
import System.Linq.*;

val numbers = List<int>();
numbers.
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var references = TestMetadataReferences.Default
            .Concat([
                MetadataReference.CreateFromFile(typeof(Enumerable).Assembly.Location),
            ])
            .ToArray();

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        compilation.EnsureSetup();

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Where");
    }

    [Fact]
    public void GetCompletions_OnType_DoesNotIncludeAccessorMethods()
    {
        var code = """
import System.*;

Console.
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.InteropServices.dll")),
                MetadataReference.CreateFromFile(typeof(System.Runtime.InteropServices.Marshal).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.DoesNotContain(items, i => i.DisplayText == "get_Out");
        Assert.DoesNotContain(items, i => i.DisplayText == "set_Out");
        Assert.DoesNotContain(items, i => i.DisplayText == "add_CancelKeyPress");
        Assert.DoesNotContain(items, i => i.DisplayText == "remove_CancelKeyPress");
    }

    [Fact]
    public void GetCompletions_OnType_DoesNotIncludeOperatorMetadataMethods()
    {
        var code = """
class Number {
    static func +(left: Number, right: Number) -> Number { return left }
    static func Parse() -> Number { return Number() }
}

Number.
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Parse");
        Assert.DoesNotContain(items, i => i.DisplayText == "op_Addition");
    }

    [Fact]
    public void GetCompletions_OnInstance_DoesNotIncludeFinalize()
    {
        var code = """
import System.*;

val text = "";
text.
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.InteropServices.dll")),
                MetadataReference.CreateFromFile(typeof(System.Runtime.InteropServices.Marshal).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Length");
        Assert.DoesNotContain(items, i => i.DisplayText == "Finalize");
    }

    [Fact]
    public void GetCompletions_OnType_ExcludesInaccessibleMembers()
    {
        var code = """
class Container {
    private static func Hidden() -> unit { }
    public static func Visible() -> unit { }
}

Container.
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences([
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.InteropServices.dll")),
                MetadataReference.CreateFromFile(typeof(System.Runtime.InteropServices.Marshal).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(object).Assembly.Location),
            ]);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Visible");
        Assert.DoesNotContain(items, i => i.DisplayText == "Hidden");
    }

    [Fact]
    public void GetCompletions_InInstanceMethod_IncludesSelf()
    {
        var code = """
class Counter {
    private value: int;

    public func Increment(delta: int) -> int {
        sel
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        compilation.EnsureSetup();

        var service = new CompletionService();
        var position = code.LastIndexOf("sel", StringComparison.Ordinal) + "sel".Length;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "self");
    }

    [Fact]
    public void GetCompletions_InInstanceMethodWithoutIdentifier_IncludesSelf()
    {
        var code = """
class Counter {
    private value: int;

    public func Increment(delta: int) -> int {
        /*caret*/
    }
}
""";

        var caret = code.IndexOf("/*caret*/", StringComparison.Ordinal);
        code = code.Remove(caret, "/*caret*/".Length);

        var syntaxTree = SyntaxTree.ParseText(code);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        compilation.EnsureSetup();

        var service = new CompletionService();
        var items = service.GetCompletions(compilation, syntaxTree, caret).ToList();

        Assert.Contains(items, i => i.DisplayText == "self");
    }

    [Fact]
    public void GetCompletions_InConstructor_IncludesSelf()
    {
        var code = """
class Counter {
    private value: int;

    public init(value: int) {
        /*caret*/
    }
}
""";

        var caret = code.IndexOf("/*caret*/", StringComparison.Ordinal);
        code = code.Remove(caret, "/*caret*/".Length);

        var syntaxTree = SyntaxTree.ParseText(code);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        compilation.EnsureSetup();

        var service = new CompletionService();
        var items = service.GetCompletions(compilation, syntaxTree, caret).ToList();

        Assert.Contains(items, i => i.DisplayText == "self");
    }

    [Fact(Skip = "Self member-access completion currently returns no items; tracked separately.")]
    public void GetCompletions_OnSelfMemberAccess_ReturnsInstanceMembers()
    {
        var code = """
class Counter {
    private value: int;

    public func Increment(delta: int) -> int {
        return self./*caret*/value
    }
}
""";

        var position = code.IndexOf("/*caret*/", StringComparison.Ordinal);
        code = code.Replace("/*caret*/", string.Empty, StringComparison.Ordinal);

        var syntaxTree = SyntaxTree.ParseText(code);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        compilation.EnsureSetup();

        var service = new CompletionService();
        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "value");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnArray_IncludesLinqExtensionMethods()
    {
        // Regression test: LINQ extension methods on T[] were previously missing because
        // IArrayTypeSymbol fell through to null in the instanceTypeForExtensions switch.
        var code = """
import System.Collections.Generic.*;
import System.Linq.*;

val numbers: int[] = [1, 2, 3]
numbers.
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var references = TestMetadataReferences.Default
            .Concat([
                MetadataReference.CreateFromFile(typeof(Enumerable).Assembly.Location),
            ])
            .ToArray();

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        compilation.EnsureSetup();

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Where");
        Assert.Contains(items, i => i.DisplayText == "Select");
        Assert.Contains(items, i => i.DisplayText == "FirstOrDefault");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnGenericArray_IncludesLinqExtensionMethods()
    {
        // Regression test: same issue but with a generic array T[].
        var code = """
import System.Collections.Generic.*;
import System.Linq.*;

func Test<T>(items: T[]) {
    items.
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var references = TestMetadataReferences.Default
            .Concat([
                MetadataReference.CreateFromFile(typeof(Enumerable).Assembly.Location),
            ])
            .ToArray();

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        compilation.EnsureSetup();

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;

        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "Where");
        Assert.Contains(items, i => i.DisplayText == "Select");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnUnitValue_IncludesInheritedObjectMembers()
    {
        var code = """
func MakeUnit() -> unit {
    ()
}

val value = MakeUnit()
value.
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;
        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "ToString");
        Assert.Contains(items, i => i.DisplayText == "GetType");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnUnitLiteral_IncludesInheritedObjectMembers()
    {
        var code = """
func Main() -> unit {
    ().
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;
        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "ToString");
        Assert.Contains(items, i => i.DisplayText == "GetType");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnDiscriminatedUnionCaseValue_IncludesInheritedObjectMembers()
    {
        var code = """
union Result {
    Ok
    Err(message: string)
}

val value = Result.Ok
value.
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;
        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "ToString");
        Assert.Contains(items, i => i.DisplayText == "GetType");
    }

    [Fact]
    public void GetCompletions_AfterDot_OnDiscriminatedUnionCaseInvocation_IncludesInheritedObjectMembers()
    {
        var code = """
union Option<T> {
    None
    Some(value: T)
}

val x = Option<int>.Some(value: 2)
x.
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        var service = new CompletionService();
        var position = code.LastIndexOf('.') + 1;
        var items = service.GetCompletions(compilation, syntaxTree, position).ToList();

        Assert.Contains(items, i => i.DisplayText == "ToString");
        Assert.Contains(items, i => i.DisplayText == "GetType");
    }

}

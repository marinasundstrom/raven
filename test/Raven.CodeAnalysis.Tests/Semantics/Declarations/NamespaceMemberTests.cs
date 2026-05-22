using System;
using System.Linq;
using System.Threading.Tasks;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests.Declarations;

public sealed class NamespaceMemberTests : CompilationTestBase
{
    [Fact]
    public void MemberSignatureDeclaration_MetadataWildcardImport_IsStableUnderConcurrentDeclarationQueries()
    {
        const string source = """
namespace App

import System.Text.Json.*

public func Normalize(options: JsonSerializerOptions) -> JsonSerializerOptions {
    return options
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);

        var exception = Record.Exception(() =>
            Parallel.For(0, 16, _ => compilation.EnsureSourceDeclarationsDeclared()));

        Assert.Null(exception);
        Assert.DoesNotContain(compilation.GetDiagnostics(), static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var function = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single(static function => function.Identifier.ValueText == "Normalize");
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(function));

        Assert.Equal("JsonSerializerOptions", method.ReturnType.Name);
        Assert.Equal("JsonSerializerOptions", method.Parameters.Single().Type.Name);
    }

    [Fact]
    public void MemberSignatureDeclaration_SourceWildcardImport_ResolvesDeclaredTypes()
    {
        var libraryTree = SyntaxTree.ParseText("""
namespace Shared

public class Payload {
}
""");
        var appTree = SyntaxTree.ParseText("""
namespace App

import Shared.*

public func Create() -> Payload {
    return Payload()
}
""");
        var compilation = CreateCompilation([libraryTree, appTree]);

        compilation.EnsureSourceDeclarationsDeclared();

        Assert.DoesNotContain(compilation.GetDiagnostics(), static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(appTree);
        var function = appTree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single(static function => function.Identifier.ValueText == "Create");
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(function));

        Assert.Equal("Payload", method.ReturnType.Name);
    }

    [Fact]
    public void NamespaceWildcardImport_AfterEarlyMissingNamespaceLookup_ResolvesSourceMembers()
    {
        var membersTree = SyntaxTree.ParseText("""
namespace Samples.NamespaceMembers.Members

public const DefaultTopic: string = "binding"
public const DefaultCount: int = 3

public func PrintSummary(topic: string, count: int) {
}
""");
        var mainTree = SyntaxTree.ParseText("""
namespace Samples.NamespaceMembers.App

import Samples.NamespaceMembers.Members.*

func Main() {
    PrintSummary(DefaultTopic, DefaultCount)
}
""");
        var compilation = CreateCompilation([mainTree, membersTree]);

        Assert.Null(compilation.GetNamespaceSymbol("Samples.NamespaceMembers.Members"));
        Assert.DoesNotContain(compilation.GetDiagnostics(mainTree), static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(mainTree);
        var invocation = mainTree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.Expression is IdentifierNameSyntax { Identifier.ValueText: "PrintSummary" });
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);

        Assert.Equal("PrintSummary", method.Name);
        Assert.Equal("NamespaceMembers", method.ContainingType?.Name);
    }

    [Fact]
    public void NamespaceWildcardImport_BringsTopLevelFunctionAndConstIntoScope()
    {
        const string source = """
import Utilities.*

namespace Utilities {
    public const Answer: int = 41

    public func AddOne(value: int) -> int => value + 1
}

func Run() -> int {
    return AddOne(Answer)
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        Assert.DoesNotContain(compilation.GetDiagnostics(), static d => d.Severity == DiagnosticSeverity.Error);

        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.Expression is IdentifierNameSyntax { Identifier.ValueText: "AddOne" });
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.Equal("NamespaceMembers", method.ContainingType?.Name);
        Assert.Equal(Accessibility.Public, method.DeclaredAccessibility);

        var answerIdentifier = tree.GetRoot()
            .DescendantNodes()
            .OfType<ReturnStatementSyntax>()
            .Single()
            .Expression!
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(static identifier => identifier.Identifier.ValueText == "Answer");
        var field = Assert.IsAssignableFrom<IFieldSymbol>(model.GetSymbolInfo(answerIdentifier).Symbol);
        Assert.True(field.IsConst);
        Assert.Equal(41, field.GetConstantValue());
        Assert.Equal("NamespaceMembers", field.ContainingType?.Name);
    }

    [Fact]
    public void NamespaceWildcardImport_MultiFileGlobalFunction_ResolvesSourceMembers()
    {
        var membersTree = SyntaxTree.ParseText("""
namespace Utilities

public const Answer: int = 41

public func AddOne(value: int) -> int => value + 1
""");
        var mainTree = SyntaxTree.ParseText("""
import Utilities.*

func Run() -> int {
    return AddOne(Answer)
}
""");
        var compilation = CreateCompilation([membersTree, mainTree]);
        var model = compilation.GetSemanticModel(mainTree);

        Assert.DoesNotContain(compilation.GetDiagnostics(mainTree), static d => d.Severity == DiagnosticSeverity.Error);

        var invocation = mainTree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.Expression is IdentifierNameSyntax { Identifier.ValueText: "AddOne" });
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.Equal("AddOne", method.Name);
        Assert.Equal("NamespaceMembers", method.ContainingType?.Name);

        var invocationIdentifier = (IdentifierNameSyntax)invocation.Expression;
        var identifierMethod = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocationIdentifier).Symbol);
        Assert.Equal("AddOne", identifierMethod.Name);
    }

    [Fact]
    public void NamespaceWildcardImport_MultiFileGlobalFunction_ColdSymbolQuery_ResolvesSourceMembers()
    {
        var membersTree = SyntaxTree.ParseText("""
namespace Utilities

public const Answer: int = 41

public func AddOne(value: int) -> int => value + 1
""");
        var mainTree = SyntaxTree.ParseText("""
import Utilities.*

func Run() -> int {
    return AddOne(Answer)
}
""");
        var compilation = CreateCompilation([membersTree, mainTree]);
        var model = compilation.GetSemanticModel(mainTree);

        var invocation = mainTree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.Expression is IdentifierNameSyntax { Identifier.ValueText: "AddOne" });
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);

        Assert.Equal("AddOne", method.Name);
        Assert.Equal("NamespaceMembers", method.ContainingType?.Name);
    }

    [Fact]
    public void NamespaceScopedImport_DoesNotLeakToSiblingNamespace()
    {
        const string source = """
namespace Utilities {
    public func AddOne(value: int) -> int => value + 1
}

namespace A {
    import Utilities.*

    public func Run() -> int {
        return AddOne(1)
    }
}

namespace B {
    public func Run() -> int {
        return AddOne(1)
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, diagnostic =>
            diagnostic.Descriptor.Id == "RAV0103" &&
            diagnostic.GetMessage().Contains("'AddOne' is not in scope", StringComparison.Ordinal));
        Assert.Single(diagnostics.Where(diagnostic =>
            diagnostic.Descriptor.Id == "RAV0103" &&
            diagnostic.GetMessage().Contains("'AddOne' is not in scope", StringComparison.Ordinal)));
    }

    [Fact]
    public void CompilationUnitImport_IsVisibleInsideNamespaceDeclaration()
    {
        const string source = """
import Utilities.*

namespace Utilities {
    public func AddOne(value: int) -> int => value + 1
}

namespace App {
    public func Run() -> int {
        return AddOne(1)
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        Assert.DoesNotContain(compilation.GetDiagnostics(), static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.Expression is IdentifierNameSyntax { Identifier.ValueText: "AddOne" });
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);

        Assert.Equal("AddOne", method.Name);
    }

    [Fact]
    public void FileScopedNamespaceImport_IsVisibleToNamespaceMembers()
    {
        var membersTree = SyntaxTree.ParseText("""
namespace Utilities

public func AddOne(value: int) -> int => value + 1
""");
        var mainTree = SyntaxTree.ParseText("""
namespace App

import Utilities.*

public func Run() -> int {
    return AddOne(1)
}
""");
        var compilation = CreateCompilation([membersTree, mainTree]);
        var model = compilation.GetSemanticModel(mainTree);

        Assert.DoesNotContain(compilation.GetDiagnostics(mainTree), static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var invocation = mainTree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.Expression is IdentifierNameSyntax { Identifier.ValueText: "AddOne" });
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);

        Assert.Equal("AddOne", method.Name);
    }

    [Fact]
    public void CompilationUnitImport_IsVisibleInsideFileScopedNamespace()
    {
        var membersTree = SyntaxTree.ParseText("""
namespace Utilities

public func AddOne(value: int) -> int => value + 1
""");
        var mainTree = SyntaxTree.ParseText("""
import Utilities.*

namespace App

public func Run() -> int {
    return AddOne(1)
}
""");
        var compilation = CreateCompilation([membersTree, mainTree]);
        var model = compilation.GetSemanticModel(mainTree);

        Assert.DoesNotContain(compilation.GetDiagnostics(mainTree), static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var invocation = mainTree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.Expression is IdentifierNameSyntax { Identifier.ValueText: "AddOne" });
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);

        Assert.Equal("AddOne", method.Name);
    }

    [Fact]
    public void NamespaceWildcardImport_BringsTopLevelMarkedStaticTypeMembersIntoScope()
    {
        const string source = """
import System.*
import Utilities.*

namespace Utilities {
    public class TopLevelAttribute : Attribute {
    }

    [TopLevel]
    public static class Helpers {
        public static const DefaultCount: int = 3

        public static func Twice(value: int) -> int => value * 2
    }
}

func Run() -> int {
    return Twice(DefaultCount)
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        Assert.DoesNotContain(compilation.GetDiagnostics(), static d => d.Severity == DiagnosticSeverity.Error);

        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.Expression is IdentifierNameSyntax { Identifier.ValueText: "Twice" });
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.Equal("Helpers", method.ContainingType?.Name);

        var defaultCountIdentifier = tree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(static identifier => identifier.Identifier.ValueText == "DefaultCount");
        var field = Assert.IsAssignableFrom<IFieldSymbol>(model.GetSymbolInfo(defaultCountIdentifier).Symbol);
        Assert.Equal("Helpers", field.ContainingType?.Name);
    }

    [Fact]
    public void NamespaceWildcardImport_DoesNotBindSourceAttributesWhileDiscoveringTopLevelContainers()
    {
        const string source = """
import System.*
import Utilities.*

namespace Utilities {
    [Obsolete("Not a namespace member container")]
    public class RegularType {
    }

    public func AddOne(value: int) -> int => value + 1
}

func Run() -> int {
    return AddOne(41)
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        Assert.DoesNotContain(compilation.GetDiagnostics(), static d => d.Severity == DiagnosticSeverity.Error);

        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.Expression is IdentifierNameSyntax { Identifier.ValueText: "AddOne" });
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.Equal("NamespaceMembers", method.ContainingType?.Name);
    }

    [Fact]
    public void NamespaceWildcardImport_BringsMetadataTopLevelMarkedStaticTypeMembersIntoScope()
    {
        var metadataReference = TestMetadataFactory.CreateFromSource(
            """
import System.*

namespace Utilities {
    class TopLevelAttribute : Attribute {
    }

    [TopLevel]
    public static class MetadataHelpers {
        public static func RavenTriple(value: int) -> int => value * 3
    }
}
""",
            assemblyName: "topLevelMetadataHelpers");

        const string source = """
import Utilities.*

func Run() -> int {
    return RavenTriple(14)
}
""";

        var tree = SyntaxTree.ParseText(source);
        var references = TestMetadataReferences.Default
            .Concat([metadataReference])
            .ToArray();
        var compilation = CreateCompilation(tree, references: references);
        var model = compilation.GetSemanticModel(tree);

        Assert.DoesNotContain(compilation.GetDiagnostics(), static d => d.Severity == DiagnosticSeverity.Error);

        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.Expression is IdentifierNameSyntax { Identifier.ValueText: "RavenTriple" });
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.Equal("MetadataHelpers", method.ContainingType?.Name);
    }

    [Fact]
    public void NamespaceMemberImportsOption_DisablesNamespacePromotion()
    {
        const string source = """
namespace Utilities {
    public func AddOne(value: int) -> int => value + 1
}

func Run() -> int {
    return Utilities.AddOne(1)
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(
            tree,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithAllowNamespaceMemberImports(false));

        Assert.Contains(compilation.GetDiagnostics(), static d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void NamespaceQualifiedAccess_ResolvesTopLevelFunctionAndConst()
    {
        const string source = """
namespace Utilities {
    public const Answer: int = 41

    public func AddOne(value: int) -> int => value + 1
}

func Run() -> int {
    return Utilities.AddOne(Utilities.Answer)
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        Assert.DoesNotContain(compilation.GetDiagnostics(), static d => d.Severity == DiagnosticSeverity.Error);

        var addOneAccess = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(static access => access.Name.Identifier.ValueText == "AddOne");
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(addOneAccess).Symbol);
        Assert.Equal("NamespaceMembers", method.ContainingType?.Name);

        var answerAccess = tree.GetRoot()
            .DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single(static access => access.Name.Identifier.ValueText == "Answer");
        var field = Assert.IsAssignableFrom<IFieldSymbol>(model.GetSymbolInfo(answerAccess).Symbol);
        Assert.True(field.IsConst);
    }

    [Fact]
    public void SpecificImport_BringsTopLevelFunctionAndConstIntoScope()
    {
        const string source = """
import Utilities.AddOne
import Utilities.Answer

namespace Utilities {
    public const Answer: int = 41

    public func AddOne(value: int) -> int => value + 1
}

func Run() -> int {
    return AddOne(Answer)
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        Assert.DoesNotContain(compilation.GetDiagnostics(), static d => d.Severity == DiagnosticSeverity.Error);

        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.Expression is IdentifierNameSyntax { Identifier.ValueText: "AddOne" });
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.Equal("AddOne", method.Name);

        var answerIdentifier = tree.GetRoot()
            .DescendantNodes()
            .OfType<ReturnStatementSyntax>()
            .Single()
            .Expression!
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(static identifier => identifier.Identifier.ValueText == "Answer");
        var field = Assert.IsAssignableFrom<IFieldSymbol>(model.GetSymbolInfo(answerIdentifier).Symbol);
        Assert.Equal("Answer", field.Name);
    }

    [Fact]
    public void SameNamespaceDifferentFile_ResolvesTopLevelFunctionWithoutImport()
    {
        var file1 = SyntaxTree.ParseText(
            """
namespace Utilities

public func AddOne(value: int) -> int => value + 1
""",
            path: "/tmp/namespace-members-same-namespace-a.rvn");

        var file2 = SyntaxTree.ParseText(
            """
namespace Utilities

public func Run() -> int {
    return AddOne(41)
}
""",
            path: "/tmp/namespace-members-same-namespace-b.rvn");

        var compilation = CreateCompilation([file1, file2]);
        var model = compilation.GetSemanticModel(file2);

        Assert.DoesNotContain(compilation.GetDiagnostics(), static d => d.Severity == DiagnosticSeverity.Error);

        var invocation = file2.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.Expression is IdentifierNameSyntax { Identifier.ValueText: "AddOne" });
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.Equal("AddOne", method.Name);
        Assert.Equal("Utilities", method.ContainingNamespace?.ToString());
    }

    [Fact]
    public void GetDeclaredSymbol_ReturnsTopLevelFunctionAndConstMembers()
    {
        const string source = """
namespace Utilities {
    public const Answer: int = 41

    public func AddOne(value: int) -> int => value + 1
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        Assert.DoesNotContain(compilation.GetDiagnostics(), static d => d.Severity == DiagnosticSeverity.Error);

        var function = root.DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(function));
        Assert.Equal("AddOne", method.Name);
        Assert.Equal("NamespaceMembers", method.ContainingType?.Name);

        var declarator = root.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var field = Assert.IsAssignableFrom<IFieldSymbol>(model.GetDeclaredSymbol(declarator));
        Assert.Equal("Answer", field.Name);
        Assert.Equal("NamespaceMembers", field.ContainingType?.Name);
    }

    [Fact]
    public void OverloadResolution_SelectsTopLevelFunctionOverload()
    {
        const string source = """
func Pick(value: int) -> int => value

func Pick(value: string) -> string => value

func Run() -> int {
    return Pick(1)
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        Assert.DoesNotContain(compilation.GetDiagnostics(), static d => d.Severity == DiagnosticSeverity.Error);

        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(static invocation => invocation.Expression is IdentifierNameSyntax { Identifier.ValueText: "Pick" });
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);

        Assert.Equal("Pick", method.Name);
        Assert.Equal(SpecialType.System_Int32, method.Parameters.Single().Type.SpecialType);
    }

    [Fact]
    public void TopLevelFunction_DoesNotCaptureFileScopeStatementLocal()
    {
        const string source = """
val prefix = "point"

func Describe() -> string {
    return prefix
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));

        Assert.Contains(
            compilation.GetDiagnostics(),
            static d => d.Descriptor == CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext &&
                        d.GetMessage().Contains("'prefix' is not in scope.", StringComparison.Ordinal));
    }

    [Fact]
    public void FileprivateTopLevelFunction_IsVisibleOnlyWithinDeclaringFile()
    {
        var file1 = SyntaxTree.ParseText(
            """
namespace Hidden

fileprivate func Secret() -> int => 1

public func SameFile() -> int => Secret()
""",
            path: "/tmp/namespace-members-a.rvn");

        var file2 = SyntaxTree.ParseText(
            """
namespace Hidden

public func OtherFile() -> int => Secret()
""",
            path: "/tmp/namespace-members-b.rvn");

        var compilation = CreateCompilation([file1, file2]);

        Assert.Contains(
            compilation.GetDiagnostics(),
            static d => d.Descriptor == CompilerDiagnostics.SymbolIsInaccessible);
    }

    [Fact]
    public void FileprivateTopLevelConst_IsVisibleOnlyWithinDeclaringFile()
    {
        var file1 = SyntaxTree.ParseText(
            """
namespace Hidden

fileprivate const Secret: int = 1

public func SameFile() -> int {
    return Secret
}
""",
            path: "/tmp/namespace-members-const-a.rvn");

        var file2 = SyntaxTree.ParseText(
            """
namespace Hidden

public func OtherFile() -> int {
    return Secret
}
""",
            path: "/tmp/namespace-members-const-b.rvn");

        var compilation = CreateCompilation([file1, file2]);

        Assert.Contains(
            compilation.GetDiagnostics(),
            static d => d.Descriptor == CompilerDiagnostics.SymbolIsInaccessible);
    }

    [Fact]
    public void StaticModifier_OnTopLevelFunction_IsDiagnosed()
    {
        const string source = "static func Helper() -> int => 1";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);

        Assert.Contains(
            compilation.GetDiagnostics(),
            static d => d.Descriptor == CompilerDiagnostics.ModifierNotValidOnMember &&
                        d.GetMessage().Contains("static", StringComparison.Ordinal));
    }

    [Fact]
    public void NamespaceMembers_AreAllowedWhenTopLevelStatementsAreDisabled()
    {
        const string source = """
const Answer: int = 41

func AddOne(value: int) -> int => value + 1
""";

        var tree = SyntaxTree.ParseText(source);
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithAllowGlobalStatements(false);
        var compilation = CreateCompilation(tree, options);

        Assert.DoesNotContain(compilation.GetDiagnostics(), static d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void NamespaceMembers_CanBeDisabledIndependently()
    {
        const string source = """
const Answer: int = 41

func AddOne(value: int) -> int => value + 1
""";

        var tree = SyntaxTree.ParseText(source);
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithAllowNamespaceMembers(false);
        var compilation = CreateCompilation(tree, options);

        var disabledDiagnostics = compilation.GetDiagnostics()
            .Where(static d => d.Descriptor.Id == "RAV7003")
            .ToArray();

        Assert.Equal(2, disabledDiagnostics.Length);
    }
}

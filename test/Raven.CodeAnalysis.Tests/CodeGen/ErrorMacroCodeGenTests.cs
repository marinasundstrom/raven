using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class ErrorMacroCodeGenTests
{
    [Fact]
    public void ErrorMacro_DerivesErrorContractForUnion()
    {
        var result = InvokeRun("""
            import System.*
            import Raven.Macros.*

            #[Error]
            union FileError {
                case NotFound(path: string)
                case AccessDenied
            }

            class Harness {
                public static func Run() -> string {
                    let failure: FileError = .NotFound("settings.rvn")
                    let error: IError = failure
                    return error.Message
                }
            }
            """);

        Assert.Equal("FileError.NotFound(\"settings.rvn\")", result);
    }

    [Fact]
    public void ErrorMacro_PreservesExistingInterfacesAndMessage()
    {
        var result = InvokeRun("""
            import System.*
            import Raven.Macros.*

            interface IHasCode {
                func GetCode() -> int
            }

            #[Error]
            union FileError: IHasCode {
                case NotFound

                val Message: string => "custom message"
                func GetCode() -> int => 404
            }

            class Harness {
                public static func Run() -> string {
                    let failure: FileError = .NotFound
                    let error: IError = failure
                    let coded: IHasCode = failure
                    return error.Message + ":" + coded.GetCode().ToString()
                }
            }
            """);

        Assert.Equal("custom message:404", result);
    }

    [Fact]
    public void ErrorMessageMacro_UsesCasePayloadInRavenInterpolation()
    {
        var result = InvokeRun("""
            import System.*
            import Raven.Macros.*

            #[Error]
            union ParseError {
                #[ErrorMessage("Invalid value: $value")]
                case InvalidValue(value: string)

                #[ErrorMessage("A value is required")]
                case MissingValue
            }

            class Harness {
                public static func Run() -> string {
                    let invalid: IError = ParseError.InvalidValue("age")
                    let missing: IError = ParseError.MissingValue
                    return invalid.Message + "; " + missing.Message
                }
            }
            """);

        Assert.Equal("Invalid value: age; A value is required", result);
    }

    [Fact]
    public void ErrorMessageMacro_UnannotatedCaseUsesDefaultMessage()
    {
        var result = InvokeRun("""
            import System.*
            import Raven.Macros.*

            #[Error]
            union ParseError {
                #[ErrorMessage("Invalid value: $value")]
                case InvalidValue(value: string)
                case MissingValue
            }

            class Harness {
                public static func Run() -> string {
                    let missing: IError = ParseError.MissingValue
                    return missing.Message
                }
            }
            """);

        Assert.Equal("ParseError.MissingValue", result);
    }

    [Fact]
    public void ErrorMessageMacro_UsesStructLikeCaseFieldsInRavenInterpolation()
    {
        var result = InvokeRun("""
            import System.*
            import Raven.Macros.*

            #[Error]
            union ProcessError {
                #[ErrorMessage("Process $Code failed: $Reason")]
                case Failed {
                    Code: int
                    Reason: string
                }
            }

            class Harness {
                public static func Run() -> string {
                    let failure: IError = ProcessError.Failed {
                        Code = 17
                        Reason = "timeout"
                    }
                    return failure.Message
                }
            }
            """);

        Assert.Equal("Process 17 failed: timeout", result);
    }

    [Theory]
    [InlineData("#[ErrorMessage(42)] case Invalid", "ERRORMESSAGE001")]
    [InlineData("#[ErrorMessage(\"Invalid\")] case Invalid", "ERRORMESSAGE002")]
    public void ErrorMessageMacro_InvalidUseReportsDiagnostic(string declaration, string expectedCode)
    {
        var source = $$"""
            import Raven.Macros.*

            union ParseError {
                {{declaration}}
            }
            """;
        if (expectedCode == "ERRORMESSAGE001")
            source = source.Replace("union ParseError", "#[Error]\nunion ParseError", StringComparison.Ordinal);

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(
                [
                    .. TestMetadataReferences.Default,
                    TestMetadataReferences.RavenCore,
                    TestMetadataReferences.RavenMacros
                ]);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic =>
                diagnostic.Id == "RAVM021" &&
                diagnostic.GetMessage().Contains(expectedCode, StringComparison.Ordinal));
    }

    [Fact]
    public void ErrorMessageMacro_DuplicateMessageReportsDiagnostic()
    {
        const string source = """
            import Raven.Macros.*

            #[Error]
            union ParseError {
                #[ErrorMessage("First")]
                #[ErrorMessage("Second")]
                case Invalid
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(
                [
                    .. TestMetadataReferences.Default,
                    TestMetadataReferences.RavenCore,
                    TestMetadataReferences.RavenMacros
                ]);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic =>
                diagnostic.Id == "RAVM021" &&
                diagnostic.GetMessage().Contains("ERRORMESSAGE003", StringComparison.Ordinal));
    }

    private static object? InvokeRun(string source)
    {
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(
                [
                    .. TestMetadataReferences.Default,
                    TestMetadataReferences.RavenCore,
                    TestMetadataReferences.RavenMacros
                ]);
        var attribute = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<AttributeSyntax>()
            .Single(static attribute => attribute.Name.ToString() == "Error");
        var expansion = compilation.GetSemanticModel(syntaxTree).GetMacroExpansion(attribute);
        Assert.NotNull(expansion);
        Assert.NotNull(expansion.ReplacementDeclaration?.SyntaxTree);
        Assert.All(expansion.IntroducedMembers, static member => Assert.NotNull(member.SyntaxTree));
        var union = syntaxTree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single();
        var unionSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetSemanticModel(syntaxTree).GetDeclaredSymbol(union));
        Assert.Contains(
            unionSymbol.GetMembers().OfType<IPropertySymbol>(),
            static property => property.Name == "Message" && property.GetMethod is not null);
        Assert.Contains(
            unionSymbol.GetMembers().OfType<IPropertySymbol>(),
            static property => property.Name == "Cause" && property.GetMethod is not null);

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(
            emitResult.Success,
            string.Join(Environment.NewLine, emitResult.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(
            peStream,
            TestMetadataReferences.Default);
        var method = loaded.Assembly
            .GetType("Harness", throwOnError: true)!
            .GetMethod("Run", BindingFlags.Public | BindingFlags.Static);
        return method!.Invoke(null, null);
    }
}

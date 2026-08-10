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

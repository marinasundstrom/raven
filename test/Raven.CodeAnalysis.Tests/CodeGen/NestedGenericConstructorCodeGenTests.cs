using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.CodeGen;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class NestedGenericConstructorCodeGenTests
{
    private const string Source = """
class Foo<T> {
    public class Bar {
        let value: T
    }
}

class Outer<A> {
    public class Inner<B> {
        let value: A
        let b: B
    }
}

let a = Foo<int>.Bar()
let b = Outer<int>.Inner<string>()
""";

    [Fact]
    public void Newobj_ForNestedConstructedTypes_UsesClosedConstructors()
    {
        var instructions = CaptureMainInstructions();
        var newobjInstructions = instructions
            .Where(instruction => instruction.Opcode == OpCodes.Newobj)
            .ToArray();

        Assert.Collection(newobjInstructions,
            instruction => AssertConstructedConstructor(
                instruction,
                expectedTypeName: "Foo`1+Bar",
                typeof(int)),
            instruction => AssertConstructedConstructor(
                instruction,
                expectedTypeName: "Outer`1+Inner`1",
                typeof(int),
                typeof(string)));
    }

    private static RecordedInstruction[] CaptureMainInstructions()
    {
        var syntaxTree = SyntaxTree.ParseText(Source);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("nested", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        _ = compilation.GetSpecialType(SpecialType.System_Object);

        var recordingFactory = new RecordingILBuilderFactory(
            ReflectionEmitILBuilderFactory.Instance,
            methodGenerator =>
                methodGenerator.MethodSymbol.Name == "Main" &&
                methodGenerator.MethodSymbol.ContainingType?.Name == "Program");

        var codeGenerator = new CodeGenerator(compilation)
        {
            ILBuilderFactory = recordingFactory
        };

        using var peStream = new MemoryStream();
        codeGenerator.Emit(peStream, pdbStream: null);

        return recordingFactory.CapturedInstructions?.ToArray()
            ?? throw new InvalidOperationException("Failed to capture Program.Main.");
    }

    private static void AssertConstructedConstructor(
        RecordedInstruction instruction,
        string expectedTypeName,
        params Type[] expectedArguments)
    {
        Assert.Equal(OpCodes.Newobj, instruction.Opcode);
        Assert.Equal(RecordedOperandKind.ConstructorInfo, instruction.Operand.Kind);

        var constructor = Assert.IsAssignableFrom<ConstructorInfo>(instruction.Operand.Value);
        var declaringType = constructor.DeclaringType ??
            throw new InvalidOperationException("Constructor has no declaring type.");

        Assert.Contains(expectedTypeName, declaringType.FullName);

        var arguments = declaringType.GetGenericArguments();
        Assert.Equal(expectedArguments.Length, arguments.Length);

        for (var i = 0; i < arguments.Length; i++)
            Assert.Equal(expectedArguments[i], arguments[i]);
    }
}

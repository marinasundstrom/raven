using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ConstructedNamedTypeSymbolTests
{
    [Fact]
    public void ConstructedType_FromMetadata_CanonicalizesByArguments()
    {
        var compilation = Compilation.Create("constructed-metadata-canonicalization", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);

        var first = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.Construct(intType));
        var second = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.Construct(intType));

        Assert.Same(first, second);
    }

    [Fact]
    public void TypeParameterSubstitutionComparer_FallsBackToOrdinalAndContainingSymbol()
    {
        var compilation = Compilation.Create(
                "type-parameter-substitution-comparer",
                new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var method = new SourceMethodSymbol(
            "Compute",
            compilation.GetSpecialType(SpecialType.System_Void),
            ImmutableArray<SourceParameterSymbol>.Empty,
            compilation.GlobalNamespace,
            containingType: null,
            compilation.GlobalNamespace,
            Array.Empty<Location>(),
            Array.Empty<SyntaxReference>(),
            isStatic: true);

        var first = new SourceTypeParameterSymbol(
            "T",
            method,
            containingType: null,
            compilation.GlobalNamespace,
            Array.Empty<Location>(),
            Array.Empty<SyntaxReference>(),
            ordinal: 0,
            TypeParameterConstraintKind.None,
            ImmutableArray<SyntaxReference>.Empty,
            VarianceKind.None);

        var second = new SourceTypeParameterSymbol(
            "T",
            method,
            containingType: null,
            compilation.GlobalNamespace,
            Array.Empty<Location>(),
            Array.Empty<SyntaxReference>(),
            ordinal: 0,
            TypeParameterConstraintKind.None,
            ImmutableArray<SyntaxReference>.Empty,
            VarianceKind.None);

        Assert.True(TypeParameterSubstitutionComparer.Instance.Equals(first, second));
        Assert.Equal(
            TypeParameterSubstitutionComparer.Instance.GetHashCode(first),
            TypeParameterSubstitutionComparer.Instance.GetHashCode(second));
    }

    [Fact]
    public void ConstructedType_FromSource_SubstitutesTypeArgumentsInMethods()
    {
        var source = """
class Container<T>
{
    public Identity(value: T) -> T => value;
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "constructed-source-method-substitution",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));

        var containerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("Container"));

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var constructed = Assert.IsAssignableFrom<INamedTypeSymbol>(containerDefinition.Construct(stringType));

        var identity = Assert.Single(constructed.GetMembers("Identity").OfType<IMethodSymbol>());

        var parameter = Assert.Single(identity.Parameters);

        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, identity.ReturnType));
        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, parameter.Type));
    }

    [Fact]
    public void ConstructedType_FromSource_SubstitutesNestedTypeMembers()
    {
        var source = """
class Container<T>
{
    public class Holder
    {
        var value: T;
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "constructed-source-nested-type",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));

        var containerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("Container"));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var constructed = Assert.IsAssignableFrom<INamedTypeSymbol>(containerDefinition.Construct(intType));

        var holder = Assert.IsAssignableFrom<INamedTypeSymbol>(constructed.LookupType("Holder"));
        var field = Assert.Single(holder.GetMembers("value").OfType<IFieldSymbol>());

        Assert.True(SymbolEqualityComparer.Default.Equals(intType, field.Type));
        Assert.True(SymbolEqualityComparer.Default.Equals(constructed, holder.ContainingType));
    }

    [Fact]
    public void ConstructedType_FromSynthesizedAsyncStateMachine_RefreshesMembersAfterMutation()
    {
        var compilation = Compilation.Create(
                "constructed-async-state-machine",
                new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var taskOfT = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Threading.Tasks.Task`1"));

        var method = new SourceMethodSymbol(
            "RunAsync",
            compilation.GetSpecialType(SpecialType.System_Void),
            ImmutableArray<SourceParameterSymbol>.Empty,
            compilation.GlobalNamespace,
            containingType: null,
            compilation.GlobalNamespace,
            Array.Empty<Location>(),
            Array.Empty<SyntaxReference>(),
            isStatic: true,
            methodKind: MethodKind.Ordinary,
            isAsync: true);

        var typeParameter = new SourceTypeParameterSymbol(
            "T",
            method,
            containingType: null,
            compilation.GlobalNamespace,
            Array.Empty<Location>(),
            Array.Empty<SyntaxReference>(),
            ordinal: 0,
            TypeParameterConstraintKind.None,
            ImmutableArray<SyntaxReference>.Empty,
            VarianceKind.None);

        method.SetTypeParameters([typeParameter]);
        method.SetReturnType(taskOfT.Construct(typeParameter));

        var stateMachine = new SynthesizedAsyncStateMachineTypeSymbol(
            compilation,
            method,
            "Program+<>c__AsyncStateMachine_Test");

        method.SetAsyncStateMachine(stateMachine);

        var initial = Assert.IsAssignableFrom<INamedTypeSymbol>(
            stateMachine.GetConstructedStateMachine(method));

        var initialMemberCount = initial.GetMembers().Length;

        stateMachine.AddHoistedLocal("hoisted", compilation.GetSpecialType(SpecialType.System_Int32));

        var updated = Assert.IsAssignableFrom<INamedTypeSymbol>(
            stateMachine.GetConstructedStateMachine(method));

        Assert.NotSame(initial, updated);
        Assert.NotEqual(initialMemberCount, updated.GetMembers().Length);
        Assert.NotEmpty(updated.GetMembers("hoisted"));
    }

    [Fact]
    public void ConstructedType_Substitute_AllowsRecursiveFieldTypes()
    {
        var source = """
class Node<T>
{
    var next: Node<T>;
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "constructed-recursive-field",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));

        var nodeDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("Node"));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var constructedNode = Assert.IsAssignableFrom<INamedTypeSymbol>(nodeDefinition.Construct(intType));

        var nextField = Assert.Single(constructedNode.GetMembers("next").OfType<IFieldSymbol>());

        Assert.True(SymbolEqualityComparer.Default.Equals(constructedNode, nextField.Type));
    }

    [Fact]
    public void ConstructedType_NestedTypeInstantiation_RetainsContainingType()
    {
        var source = """
class Container<T>
{
    public class Holder
    {
        var value: T;
    }

    public class Inner<U>
    {
        var value: T;
        var data: U;
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "constructed-nested-instantiation",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));

        var containerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("Container"));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var constructed = Assert.IsAssignableFrom<INamedTypeSymbol>(containerDefinition.Construct(intType));

        var holder = Assert.IsAssignableFrom<INamedTypeSymbol>(constructed.LookupType("Holder"));
        Assert.True(SymbolEqualityComparer.Default.Equals(constructed, holder.ContainingType));

        var innerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(constructed.LookupType("Inner"));
        var constructedInner = Assert.IsAssignableFrom<INamedTypeSymbol>(innerDefinition.Construct(stringType));

        Assert.True(SymbolEqualityComparer.Default.Equals(constructed, constructedInner.ContainingType));

        var valueField = Assert.Single(constructedInner.GetMembers("value").OfType<IFieldSymbol>());
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, valueField.Type));

        var dataField = Assert.Single(constructedInner.GetMembers("data").OfType<IFieldSymbol>());
        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, dataField.Type));
    }

    [Fact]
    public void ConstructedType_NestedTypeInstantiation_PreservesAllTypeArguments()
    {
        var source = """
class Outer<T>
{
    public class Inner<U>
    {
        var outerValue: T;
        var innerValue: U;
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "constructed-nested-arguments",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));

        var outerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("Outer"));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var constructedOuter = Assert.IsAssignableFrom<INamedTypeSymbol>(outerDefinition.Construct(intType));

        var innerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(constructedOuter.LookupType("Inner"));
        var constructedInner = Assert.IsAssignableFrom<ConstructedNamedTypeSymbol>(innerDefinition.Construct(stringType));

        var runtimeArguments = constructedInner.GetAllTypeArguments();
        Assert.Equal(constructedInner.TypeParameters.Length, runtimeArguments.Length);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, runtimeArguments[0]));
        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, runtimeArguments[1]));

        var display = constructedInner.ToDisplayString(SymbolDisplayFormat.CSharpErrorMessageFormat);
        Assert.Equal("Outer<int>.Inner<string>", display);

        var metadataName = constructedInner.ToFullyQualifiedMetadataName();
        Assert.Equal("Outer`1+Inner`1", metadataName);
        Assert.Equal(1, constructedInner.Arity);
    }

    [Fact]
    public void ConstructedType_MethodGenerics_PreservesMethodTypeParameters()
    {
        var source = """
import System.*

class Container<T>
{
    public Combine<U>(factory: Func<T, U>, seed: T) -> U => factory(seed);
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "constructed-source-method-generics",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));

        var containerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("Container"));

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var constructed = Assert.IsAssignableFrom<INamedTypeSymbol>(containerDefinition.Construct(stringType));

        var combine = Assert.IsType<SubstitutedMethodSymbol>(
            constructed
                .GetMembers("Combine")
                .OfType<IMethodSymbol>()
                .Single());

        var factoryParameter = combine.Parameters[0];
        var funcType = Assert.IsAssignableFrom<INamedTypeSymbol>(factoryParameter.Type);

        Assert.Equal("Func", funcType.Name);
        Assert.Equal(2, funcType.TypeArguments.Length);

        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, funcType.TypeArguments[0]));

        var methodTypeParameter = Assert.IsAssignableFrom<ITypeParameterSymbol>(funcType.TypeArguments[1]);
        Assert.Same(combine, methodTypeParameter.ContainingSymbol);

        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, combine.Parameters[1].Type));

        Assert.IsAssignableFrom<ITypeParameterSymbol>(combine.ReturnType);
        Assert.Same(combine, ((ITypeParameterSymbol)combine.ReturnType).ContainingSymbol);
    }

    [Fact]
    public void ConstructedType_ToStringAndDebuggerDisplay_UseDisplayStrings()
    {
        var source = """
class Outer<T>
{
    public class Inner<U>
    {
        var value: T;
        var data: U;
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
                "constructed-debugger-display-nested",
                [syntaxTree],
                TestMetadataReferences.Default,
                new CompilationOptions(OutputKind.ConsoleApplication));

        var outerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("Outer"));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var constructedOuter = Assert.IsAssignableFrom<INamedTypeSymbol>(outerDefinition.Construct(intType));

        var innerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(constructedOuter.LookupType("Inner"));
        var constructedInner = Assert.IsAssignableFrom<ConstructedNamedTypeSymbol>(innerDefinition.Construct(stringType));

        var errorDisplay = constructedInner.ToDisplayString(SymbolDisplayFormat.CSharpErrorMessageFormat);
        Assert.Equal(errorDisplay, constructedInner.ToString());

        var debuggerMethod = typeof(ConstructedNamedTypeSymbol).GetMethod(
            "GetDebuggerDisplay",
            BindingFlags.Instance | BindingFlags.NonPublic);

        var debuggerDisplay = Assert.IsType<string>(debuggerMethod!.Invoke(constructedInner, null));
        var fullyQualified = constructedInner.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
        Assert.Equal($"{constructedInner.Kind}: {fullyQualified}", debuggerDisplay);
    }

    [Fact]
    public void LookupType_SubstitutesOuterTypeArguments()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var listOfInt = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.Construct(intType));

        var enumeratorType = Assert.IsAssignableFrom<INamedTypeSymbol>(listOfInt.LookupType("Enumerator"));

        Assert.Equal("Enumerator", enumeratorType.Name);
        Assert.Equal(1, enumeratorType.Arity);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, enumeratorType.TypeArguments[0]));
    }

    [Fact]
    public void ConstructedMetadataType_DisplayAndMetadataNamesStayClosed()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var listOfInt = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.Construct(intType));

        var enumerator = Assert.IsAssignableFrom<INamedTypeSymbol>(listOfInt.LookupType("Enumerator"));

        var errorDisplay = enumerator.ToDisplayString(SymbolDisplayFormat.CSharpErrorMessageFormat);
        Assert.Equal("List<int>.Enumerator<int>", errorDisplay);

        var fullyQualified = enumerator.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
        Assert.Equal("System.Collections.Generic.List<int>.Enumerator<int>", fullyQualified);

        Assert.Equal(errorDisplay, enumerator.ToString());

        var metadataName = enumerator.ToFullyQualifiedMetadataName();
        Assert.Equal("System.Collections.Generic.List`1+Enumerator", metadataName);
    }

    [Fact]
    public void IsMemberDefined_ReturnsSubstitutedMember()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var listOfInt = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.Construct(intType));

        Assert.True(listOfInt.IsMemberDefined("Add", out var symbol));
        var addMethod = Assert.IsAssignableFrom<IMethodSymbol>(symbol);

        Assert.Single(addMethod.Parameters);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, addMethod.Parameters[0].Type));
    }

    [Fact]
    public void GetMembers_SubstitutedMethod_UsesConstructedContainingType()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var listOfString = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.Construct(stringType));

        var addRange = Assert.Single(
            listOfString.GetMembers("AddRange").OfType<IMethodSymbol>());

        Assert.True(SymbolEqualityComparer.Default.Equals(listOfString, addRange.ContainingType));

        var parameter = Assert.Single(addRange.Parameters);
        var enumerable = Assert.IsAssignableFrom<INamedTypeSymbol>(parameter.Type);

        Assert.Equal("IEnumerable", enumerable.Name);
        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, enumerable.TypeArguments[0]));
    }

    [Fact]
    public void SubstitutedMethod_ReturnType_UsesTypeArgument()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var listOfString = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.Construct(stringType));

        var find = Assert.Single(
            listOfString.GetMembers("Find").OfType<IMethodSymbol>());

        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, find.ReturnType));

        var predicate = Assert.Single(find.Parameters);
        var predicateType = Assert.IsAssignableFrom<INamedTypeSymbol>(predicate.Type);

        Assert.Equal("Predicate", predicateType.Name);
        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, predicateType.TypeArguments[0]));
    }

    [Fact]
    public void MetadataDiscriminatedUnion_UsesConstructedConstructors()
    {
        var ravenCoreSourcePath = Path.GetFullPath(Path.Combine(
            "..", "..", "..", "..", "..", "src", "Raven.Core", "Result.rav"));
        var ravenCoreSource = File.ReadAllText(ravenCoreSourcePath);

        var ravenCoreTree = SyntaxTree.ParseText(ravenCoreSource);
        var ravenCoreCompilation = Compilation.Create(
            "raven-core-fixture",
            [ravenCoreTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var ravenCoreAssemblyPath = Path.Combine(Path.GetTempPath(), $"raven-core-fixture-{Guid.NewGuid():N}.dll");

        try
        {
            using var ravenCoreStream = new MemoryStream();
            var ravenCoreEmit = ravenCoreCompilation.Emit(ravenCoreStream);
            Assert.True(ravenCoreEmit.Success, string.Join(Environment.NewLine, ravenCoreEmit.Diagnostics));

            File.WriteAllBytes(ravenCoreAssemblyPath, ravenCoreStream.ToArray());

            var ravenCoreReference = MetadataReference.CreateFromFile(ravenCoreAssemblyPath);

            var consumerSource = """
import System.*

class Program
{
    public static Main() -> unit
    {
        let result: Result<int> = .Ok(42)
        Console.WriteLine(result)
    }
}
""";

            var consumerTree = SyntaxTree.ParseText(consumerSource);
            var consumerCompilation = Compilation.Create(
                "metadata-union-consumer",
                [consumerTree],
                [.. TestMetadataReferences.Default, ravenCoreReference],
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

            using var consumerStream = new MemoryStream();
            var consumerEmit = consumerCompilation.Emit(consumerStream);
            Assert.True(consumerEmit.Success, string.Join(Environment.NewLine, consumerEmit.Diagnostics));

            var consumerImage = consumerStream.ToArray();

            var systemNamespace = consumerCompilation.GlobalNamespace.LookupNamespace("System")
                ?? consumerCompilation.GlobalNamespace;
            var resultDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
                systemNamespace
                    .GetMembers("Result")
                    .OfType<INamedTypeSymbol>()
                    .First(symbol => symbol.Arity == 1));

            Assert.Equal(TypeKind.Struct, resultDefinition.TypeKind);
            Assert.True(resultDefinition.IsValueType);

            var intType = consumerCompilation.GetSpecialType(SpecialType.System_Int32);
            var constructedResult = Assert.IsAssignableFrom<INamedTypeSymbol>(resultDefinition.Construct(intType));

            Assert.True(constructedResult.IsValueType);

            var okCase = Assert.IsAssignableFrom<INamedTypeSymbol>(constructedResult.LookupType("Ok"));
            var constructor = Assert.Single(okCase.Constructors);

            var parameterType = Assert.Single(constructor.Parameters).Type;
            Assert.True(SymbolEqualityComparer.Default.Equals(intType, parameterType));

            Assert.True(okCase.IsValueType);

            var mainIl = GetMethodIl(consumerImage, "Program", "Main");
            Assert.True(ContainsOpCode(mainIl, ILOpCode.Box), "Expected Main to box Result<int> before calling Console.WriteLine(object).");
        }
        finally
        {
            if (File.Exists(ravenCoreAssemblyPath))
                File.Delete(ravenCoreAssemblyPath);
        }
    }

    [Fact]
    public void TupleElements_AreSubstitutedFromDefinition()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var tupleDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.ValueTuple`2"));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);

        var tuple = Assert.IsAssignableFrom<INamedTypeSymbol>(tupleDefinition.Construct(intType, stringType));

        Assert.True(SymbolEqualityComparer.Default.Equals(tuple, tuple.UnderlyingTupleType));

        var elements = tuple.TupleElements;
        Assert.Equal(2, elements.Length);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, elements[0].Type));
        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, elements[1].Type));
    }

    private static byte[] GetMethodIl(byte[] peImage, string typeName, string methodName)
    {
        using var peReader = new PEReader(new MemoryStream(peImage, writable: false));
        var reader = peReader.GetMetadataReader();

        foreach (var methodHandle in reader.MethodDefinitions)
        {
            var methodDefinition = reader.GetMethodDefinition(methodHandle);
            var name = reader.GetString(methodDefinition.Name);

            if (!string.Equals(name, methodName, StringComparison.Ordinal))
                continue;

            var typeDefinition = reader.GetTypeDefinition(methodDefinition.GetDeclaringType());
            var declaredTypeName = reader.GetString(typeDefinition.Name);

            if (!string.Equals(declaredTypeName, typeName, StringComparison.Ordinal))
                continue;

            var body = peReader.GetMethodBody(methodDefinition.RelativeVirtualAddress);
            var ilReader = body.GetILReader();
            return ilReader.ReadBytes(ilReader.Length);
        }

        throw new InvalidOperationException($"Method {typeName}.{methodName} not found in PE image.");
    }

    private static bool ContainsOpCode(byte[] ilBytes, ILOpCode opcode)
    {
        var opcodeByte = (byte)opcode;

        for (int i = 0; i < ilBytes.Length; i++)
        {
            if (ilBytes[i] == opcodeByte)
                return true;

            // Skip two-byte instruction prefix when present
            if (ilBytes[i] == 0xFE && i + 1 < ilBytes.Length && ilBytes[i + 1] == opcodeByte)
                return true;
        }

        return false;
    }
}

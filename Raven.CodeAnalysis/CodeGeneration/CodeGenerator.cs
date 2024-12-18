using System.Collections.Immutable;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;

namespace Raven.CodeAnalysis.CodeGeneration;

public class CodeGenerator
{
    PersistedAssemblyBuilder assemblyBuilder;
    ModuleBuilder moduleBuilder;

    IEnumerable<string> versions = [
        ".NETStandard,Version=v2.0",
        ".NETStandard,Version=v2.1",
        ".NETFramework,Version=v7.8",
        ".NETCoreApp,Version=v6.0",
        ".NETCoreApp,Version=v7.0",
        ".NETCoreApp,Version=v8.0",
        ".NETCoreApp,Version=v9.0"
    ];

    public void Generate(Compilation compilation, string assemblyPath)
    {
        string assemblyNameStr = Path.GetFileNameWithoutExtension(assemblyPath);

        var assemblyName = new AssemblyName(assemblyNameStr);
        assemblyName.Version = new Version(1, 0, 0, 0);

        var targetFrameworkAttribute = new CustomAttributeBuilder(
            typeof(System.Runtime.Versioning.TargetFrameworkAttribute).GetConstructor([typeof(string)]),
            [".NETCoreApp,Version=v9.0"] // Replace with your version
        );

        assemblyBuilder = new PersistedAssemblyBuilder(assemblyName, typeof(object).Assembly, [targetFrameworkAttribute]);

        moduleBuilder = assemblyBuilder.DefineDynamicModule("MyModule");

        TypeBuilder tb = GenerateType();

        //assemblyBuilder.Save(assemblyPath); // or could save to a Stream

        var entryPoint = GenerateEntryPoint(tb);

        tb.CreateType();

        MetadataBuilder metadataBuilder = assemblyBuilder.GenerateMetadata(out BlobBuilder ilStream, out _, out MetadataBuilder pdbBuilder);
        MethodDefinitionHandle entryPointHandle = MetadataTokens.MethodDefinitionHandle(entryPoint.MetadataToken);
        DebugDirectoryBuilder debugDirectoryBuilder = GeneratePdb(pdbBuilder, metadataBuilder.GetRowCounts(), entryPointHandle);

        ManagedPEBuilder peBuilder = new ManagedPEBuilder(
                        header: new PEHeaderBuilder(imageCharacteristics: Characteristics.ExecutableImage, subsystem: Subsystem.WindowsCui),
                        metadataRootBuilder: new MetadataRootBuilder(metadataBuilder),
                        ilStream: ilStream,
                        debugDirectoryBuilder: debugDirectoryBuilder,
                        entryPoint: entryPointHandle);

        BlobBuilder peBlob = new BlobBuilder();
        peBuilder.Serialize(peBlob);

        using var fileStream = new FileStream(assemblyPath, FileMode.Create, FileAccess.Write);
        peBlob.WriteContentTo(fileStream);
    }

    private TypeBuilder GenerateType()
    {
        TypeBuilder tb = moduleBuilder.DefineType("MyType", TypeAttributes.Public | TypeAttributes.Class);

        GenerateMethod(tb);

        return tb;
    }

    private static MethodBuilder GenerateMethod(TypeBuilder tb)
    {
        var mb = tb.DefineMethod("SumMethod", MethodAttributes.Public | MethodAttributes.Static,
            typeof(int), [typeof(int), typeof(int)]);

        ILGenerator il = mb.GetILGenerator();
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Ldarg_1);
        il.Emit(OpCodes.Add);
        il.Emit(OpCodes.Ret);

        return mb;
    }

    private MethodBuilder GenerateEntryPoint(TypeBuilder typeBuilder)
    {
        MethodBuilder entryPoint = typeBuilder.DefineMethod("Main", MethodAttributes.HideBySig | MethodAttributes.Public | MethodAttributes.Static);
        ILGenerator il2 = entryPoint.GetILGenerator();
        // ...
        il2.Emit(OpCodes.Ret);

        return entryPoint;
    }

    static DebugDirectoryBuilder GeneratePdb(MetadataBuilder pdbBuilder, ImmutableArray<int> rowCounts, MethodDefinitionHandle entryPointHandle)
    {
        BlobBuilder portablePdbBlob = new BlobBuilder();
        PortablePdbBuilder portablePdbBuilder = new PortablePdbBuilder(pdbBuilder, rowCounts, entryPointHandle);
        BlobContentId pdbContentId = portablePdbBuilder.Serialize(portablePdbBlob);
        // In case saving PDB to a file
        using FileStream fileStream = new FileStream("MyAssemblyEmbeddedSource.pdb", FileMode.Create, FileAccess.Write);
        portablePdbBlob.WriteContentTo(fileStream);

        DebugDirectoryBuilder debugDirectoryBuilder = new DebugDirectoryBuilder();
        debugDirectoryBuilder.AddCodeViewEntry("MyAssemblyEmbeddedSource.pdb", pdbContentId, portablePdbBuilder.FormatVersion);
        // In case embedded in PE:
        // debugDirectoryBuilder.AddEmbeddedPortablePdbEntry(portablePdbBlob, portablePdbBuilder.FormatVersion);
        return debugDirectoryBuilder;
    }
}
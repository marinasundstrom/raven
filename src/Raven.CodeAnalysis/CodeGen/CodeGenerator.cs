using System.Collections.Immutable;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

internal class CodeGenerator
{
    readonly Dictionary<ITypeSymbol, TypeGenerator> _typeGenerators = new Dictionary<ITypeSymbol, TypeGenerator>(SymbolEqualityComparer.Default);

    private readonly Compilation _compilation;

    public Compilation Compilation => _compilation;

    public PersistedAssemblyBuilder AssemblyBuilder { get; private set; }
    public ModuleBuilder ModuleBuilder { get; private set; }

    private MethodBuilder EntryPoint { get; set; }

    public CodeGenerator(Compilation compilation)
    {
        _compilation = compilation;
    }

    public void Generate(Stream peStream, Stream? pdbStream)
    {
        var assemblyName = new AssemblyName(_compilation.AssemblyName);
        assemblyName.Version = new Version(1, 0, 0, 0);

        var targetFrameworkAttribute = new CustomAttributeBuilder(
            // TODO: This should not be set here
            typeof(System.Runtime.Versioning.TargetFrameworkAttribute).GetConstructor([typeof(string)]),
            [".NETCoreApp,Version=v9.0"]  // Replace with your version
        );

        AssemblyBuilder = new PersistedAssemblyBuilder(assemblyName, _compilation.CoreAssembly, [targetFrameworkAttribute]);
        ModuleBuilder = AssemblyBuilder.DefineDynamicModule(_compilation.AssemblyName);

        var globalNamespace = _compilation.SourceGlobalNamespace;

        DefineTypeBuilders();

        GenerateMemberILBodies();

        CreateTypes();

        EntryPoint = _typeGenerators.Values
            .SelectMany(x => x.MethodGenerators)
            .Where(x => x.IsEntryPointCandidate)
            .First().MethodBuilder;

        MetadataBuilder metadataBuilder = AssemblyBuilder.GenerateMetadata(out BlobBuilder ilStream, out _, out MetadataBuilder pdbBuilder);
        MethodDefinitionHandle entryPointHandle = MetadataTokens.MethodDefinitionHandle(EntryPoint.MetadataToken);
        DebugDirectoryBuilder debugDirectoryBuilder = GeneratePdb(pdbBuilder, metadataBuilder.GetRowCounts(), entryPointHandle);

        Characteristics imageCharacteristics = _compilation.Options.OutputKind switch
        {
            OutputKind.ConsoleApplication => Characteristics.ExecutableImage,
            OutputKind.DynamicallyLinkedLibrary => Characteristics.Dll,
            _ => Characteristics.Dll,
        };

        ManagedPEBuilder peBuilder = new ManagedPEBuilder(
                        header: new PEHeaderBuilder(imageCharacteristics: imageCharacteristics, subsystem: Subsystem.WindowsCui),
                        metadataRootBuilder: new MetadataRootBuilder(metadataBuilder),
                        ilStream: ilStream,
                        debugDirectoryBuilder: debugDirectoryBuilder,
                        entryPoint: entryPointHandle);

        BlobBuilder peBlob = new BlobBuilder();
        peBuilder.Serialize(peBlob);

        peBlob.WriteContentTo(peStream);
    }

    private void DefineTypeBuilders()
    {
        foreach (var typeSymbol in Compilation.Module.GlobalNamespace.GetAllMembersRecursive().OfType<ITypeSymbol>())
        {
            var generator = new TypeGenerator(this, typeSymbol);
            _typeGenerators[typeSymbol] = generator;
            generator.DefineTypeBuilder();
        }
    }

    private void GenerateMemberILBodies()
    {
        foreach (var typeGenerator in _typeGenerators.Values)
        {
            typeGenerator.GenerateMemberILBodies();
        }
    }

    private void CreateTypes()
    {
        foreach (var typeGenerator in _typeGenerators.Values)
        {
            typeGenerator.CreateType();
        }
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

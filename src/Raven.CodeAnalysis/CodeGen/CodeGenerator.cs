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
    readonly Dictionary<SourceSymbol, MemberInfo> _mappings = new Dictionary<SourceSymbol, MemberInfo>(SymbolEqualityComparer.Default);

    public void AddMemberBuilder(SourceSymbol symbol, MemberInfo memberInfo) => _mappings[symbol] = memberInfo;

    public MemberInfo? GetMemberBuilder(SourceSymbol symbol) => _mappings[symbol];

    private readonly Compilation _compilation;

    public Compilation Compilation => _compilation;

    public PersistedAssemblyBuilder AssemblyBuilder { get; private set; }
    public ModuleBuilder ModuleBuilder { get; private set; }

    private MethodBase EntryPoint { get; set; }

    public Type TypeUnionAttributeType { get; private set; }

    public CodeGenerator(Compilation compilation)
    {
        _compilation = compilation;
    }

    public Type? GetTypeBuilder(INamedTypeSymbol namedTypeSymbol)
    {
        var e = _typeGenerators[namedTypeSymbol];

        return e.Type ?? e?.TypeBuilder;
    }

    public void Emit(Stream peStream, Stream? pdbStream)
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

        CreateTypeUnionAttribute();

        DefineTypeBuilders();

        DefineMemberBuilders();

        EmitMemberILBodies();

        CreateTypes();

        EntryPoint = _typeGenerators.Values
            .SelectMany(x => x.MethodGenerators)
            .Where(x => x.IsEntryPointCandidate)
            .First().MethodBase;

        MetadataBuilder metadataBuilder = AssemblyBuilder.GenerateMetadata(out BlobBuilder ilStream, out _, out MetadataBuilder pdbBuilder);
        MethodDefinitionHandle entryPointHandle = MetadataTokens.MethodDefinitionHandle(EntryPoint.MetadataToken);
        DebugDirectoryBuilder debugDirectoryBuilder = EmitPdb(pdbBuilder, metadataBuilder.GetRowCounts(), entryPointHandle);

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

    private void CreateTypeUnionAttribute()
    {
        // Define the attribute class
        var attrBuilder = ModuleBuilder.DefineType(
            "TypeUnionAttribute",
            TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.Sealed,
            typeof(Attribute));

        // Mark as AttributeUsage (optional)
        var attrUsageCtor = typeof(AttributeUsageAttribute).GetConstructor([typeof(AttributeTargets)]);
        var attrUsageBuilder = new CustomAttributeBuilder(attrUsageCtor, [AttributeTargets.Parameter | AttributeTargets.Field | AttributeTargets.ReturnValue | AttributeTargets.Property]);
        attrBuilder.SetCustomAttribute(attrUsageBuilder);

        // Define a private readonly field: private readonly Type[] _types;
        var typesField = attrBuilder.DefineField(
            "_types",
            typeof(Type[]),
            FieldAttributes.Private | FieldAttributes.InitOnly);

        // Define the public property: public Type[] Types { get; }
        var propBuilder = attrBuilder.DefineProperty(
            "Types",
            PropertyAttributes.None,
            typeof(Type[]),
            null);

        var getterMethod = attrBuilder.DefineMethod(
            "get_Types",
            MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.SpecialName,
            typeof(Type[]),
            Type.EmptyTypes);

        var ilGet = getterMethod.GetILGenerator();
        ilGet.Emit(OpCodes.Ldarg_0); // this
        ilGet.Emit(OpCodes.Ldfld, typesField); // _types
        ilGet.Emit(OpCodes.Ret);

        // Attach the getter to the property
        propBuilder.SetGetMethod(getterMethod);

        // Define the constructor: public TypeUnionAttribute(params Type[] types)
        var ctorBuilder = attrBuilder.DefineConstructor(
            MethodAttributes.Public,
            CallingConventions.Standard,
            new[] { typeof(Type[]) });

        // Add [ParamArray] attribute to the parameter
        var paramArrayAttrCtor = typeof(ParamArrayAttribute).GetConstructor(Type.EmptyTypes);
        var paramBuilder = ctorBuilder.DefineParameter(1, ParameterAttributes.None, "types");
        var paramArrayAttr = new CustomAttributeBuilder(paramArrayAttrCtor, Array.Empty<object>());
        paramBuilder.SetCustomAttribute(paramArrayAttr);

        // Emit constructor body
        var ilCtor = ctorBuilder.GetILGenerator();
        var attributeCtor = typeof(Attribute).GetConstructor(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic, null, Type.EmptyTypes, null);
        if (attributeCtor is null)
            throw new InvalidOperationException("Missing Attribute base constructor.");

        ilCtor.Emit(OpCodes.Ldarg_0);
        ilCtor.Emit(OpCodes.Call, attributeCtor);

        ilCtor.Emit(OpCodes.Ldarg_0); // this
        ilCtor.Emit(OpCodes.Ldarg_1); // types (argument)
        ilCtor.Emit(OpCodes.Stfld, typesField); // this._types = types

        ilCtor.Emit(OpCodes.Ret);

        // Create the type
        TypeUnionAttributeType = attrBuilder.CreateType();
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

    private void DefineMemberBuilders()
    {
        foreach (var typeGenerator in _typeGenerators.Values)
        {
            typeGenerator.DefineMemberBuilders();
        }
    }

    private void CreateTypes()
    {
        foreach (var typeGenerator in _typeGenerators.Values)
        {
            typeGenerator.CreateType();
        }
    }

    private void EmitMemberILBodies()
    {
        foreach (var typeGenerator in _typeGenerators.Values)
        {
            typeGenerator.EmitMemberILBodies();
        }
    }

    static DebugDirectoryBuilder EmitPdb(MetadataBuilder pdbBuilder, ImmutableArray<int> rowCounts, MethodDefinitionHandle entryPointHandle)
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

    public bool TryGetRuntimeTypeForSymbol(INamedTypeSymbol symbol, out Type type)
    {
        if (_typeGenerators.TryGetValue(symbol, out var builder))
        {
            type = builder.TypeBuilder!; //.CreateType();
            return true;
        }

        type = null!;
        return false;
    }
}
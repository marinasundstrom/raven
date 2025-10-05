using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;
using System.Reflection2;
using System.Text.Json;
using System.Text.Json.Nodes;

var outputPath = args.Length > 0
    ? Path.GetFullPath(args[0])
    : Path.Combine(Environment.CurrentDirectory, "HelloReflection2.dll");

var assemblyName = new AssemblyName("HelloReflection2.Generated");
var persistedBuilder = new PersistedAssemblyBuilder(assemblyName, typeof(object).Assembly, Array.Empty<CustomAttributeBuilder>());
var moduleBuilder = persistedBuilder.DefineDynamicModule("MainModule");
var programBuilder = moduleBuilder.DefineType("Hello.Program", TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.Sealed);

var mainBuilder = programBuilder.DefineMethod(
    "Main",
    MethodAttributes.Public | MethodAttributes.Static,
    typeof(int),
    new[] { typeof(string[]) });

var il = mainBuilder.GetILGenerator();
il.Emit(OpCodes.Ldstr, "Hello from Reflection2!");
il.Emit(OpCodes.Call, typeof(Console).GetMethod(nameof(Console.WriteLine), new[] { typeof(string) })!);
il.Emit(OpCodes.Ldc_I4_0);
il.Emit(OpCodes.Ret);

programBuilder.CreateType();

var ilStream = new BlobBuilder();
var mappedFieldData = new BlobBuilder();
var metadataBuilder = persistedBuilder.GenerateMetadata(out ilStream, out mappedFieldData);
var entryPointHandle = MetadataTokens.MethodDefinitionHandle(mainBuilder.MetadataToken);
var peBuilder = new ManagedPEBuilder(
    new PEHeaderBuilder(imageCharacteristics: Characteristics.ExecutableImage | Characteristics.LargeAddressAware),
    new MetadataRootBuilder(metadataBuilder),
    ilStream: ilStream,
    mappedFieldData: mappedFieldData,
    entryPoint: entryPointHandle,
    flags: CorFlags.ILOnly);

var peBlob = new BlobBuilder();
peBuilder.Serialize(peBlob);
var peImage = peBlob.ToImmutableArray();

using var loadContext = CreateLoadContext();
var peReader = new PEReader(peImage);
var metadataProvider = MetadataReaderProvider.FromMetadataImage(peReader.GetMetadata().GetContent());
var metadataAssembly = loadContext.RegisterAssembly(new MetadataResolutionResult(metadataProvider, location: null, peReader));
var metadataProgram = metadataAssembly.GetType("Hello.Program", throwOnError: true, ignoreCase: false)!.GetTypeInfo();
var metadataMain = metadataProgram.GetMethod("Main", BindingFlags.Public | BindingFlags.Static)
    ?? throw new InvalidOperationException("Unable to locate metadata entry point.");

Console.WriteLine("Inspecting metadata assembly produced by PersistedAssemblyBuilder...");
Console.WriteLine($"Metadata entry point located: {metadataMain.Name}");

Directory.CreateDirectory(Path.GetDirectoryName(outputPath)!);
File.WriteAllBytes(outputPath, peImage.ToArray());

var runtimeConfigPath = Path.ChangeExtension(outputPath, ".runtimeconfig.json");
var runtimeConfig = new JsonObject
{
    ["runtimeOptions"] = new JsonObject
    {
        ["tfm"] = $"net{Environment.Version.Major}.{Environment.Version.Minor}",
        ["framework"] = new JsonObject
        {
            ["name"] = "Microsoft.NETCore.App",
            ["version"] = $"{Environment.Version.Major}.{Environment.Version.Minor}.0",
        },
        ["rollForward"] = "LatestMinor",
    },
};

File.WriteAllText(runtimeConfigPath, runtimeConfig.ToJsonString(new JsonSerializerOptions { WriteIndented = true }));

Console.WriteLine($"Wrote {outputPath}");
Console.WriteLine($"Wrote {runtimeConfigPath}");
Console.WriteLine("Run the generated program with: dotnet " + outputPath);

static MetadataLoadContext CreateLoadContext()
{
    var runtimeAssembly = typeof(object).Assembly.Location;
    var searchPaths = new List<string>();

    if (!string.IsNullOrEmpty(runtimeAssembly))
    {
        searchPaths.Add(runtimeAssembly);
        var runtimeDirectory = Path.GetDirectoryName(runtimeAssembly);
        if (!string.IsNullOrEmpty(runtimeDirectory))
        {
            searchPaths.Add(runtimeDirectory);
        }
    }

    searchPaths.Add(AppContext.BaseDirectory);

    var resolver = new PathMetadataAssemblyResolver(searchPaths);
    return new MetadataLoadContext(resolver);
}

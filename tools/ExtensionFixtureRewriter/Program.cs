using System;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;

using Mono.Cecil;
using Mono.Cecil.Cil;

namespace ExtensionFixtureRewriter;

public static class Program
{
    private const string GroupingTypeName = "<>__RavenExtensionGrouping_For_Widget";
    private const string MarkerTypeName = "<>__RavenExtensionMarker_WidgetExtensions_for_Widget";
    private const string MarkerMethodName = "<Extension>$";

    public static int Main(string[] args)
    {
        if (args.Length != 1)
        {
            Console.Error.WriteLine("Usage: ExtensionFixtureRewriter <assembly-path>");
            return 1;
        }

        var assemblyPath = args[0];
        if (!File.Exists(assemblyPath))
        {
            Console.Error.WriteLine($"Assembly not found: {assemblyPath}");
            return 1;
        }

        var readerParameters = new ReaderParameters
        {
            ReadWrite = true,
            InMemory = true
        };

        using var assembly = AssemblyDefinition.ReadAssembly(assemblyPath, readerParameters);
        var module = assembly.MainModule;

        var widgetType = module.Types.FirstOrDefault(t => t.FullName == "Raven.MetadataFixtures.StaticExtensions.Widget");
        var containerType = module.Types.FirstOrDefault(t => t.FullName == "Raven.MetadataFixtures.StaticExtensions.WidgetExtensions");

        if (widgetType is null || containerType is null)
        {
            Console.Error.WriteLine("Expected Widget/WidgetExtensions types were not found.");
            return 1;
        }

        if (containerType.NestedTypes.Any(t => t.Name == GroupingTypeName))
        {
            return 0;
        }

        var groupingType = new TypeDefinition(
            string.Empty,
            GroupingTypeName,
            TypeAttributes.NestedPublic | TypeAttributes.Class | TypeAttributes.Sealed | TypeAttributes.SpecialName,
            module.TypeSystem.Object);
        containerType.NestedTypes.Add(groupingType);

        var extensionAttributeCtor = module.ImportReference(typeof(ExtensionAttribute).GetConstructor(Type.EmptyTypes)!);
        groupingType.CustomAttributes.Add(new CustomAttribute(extensionAttributeCtor));

        var markerType = new TypeDefinition(
            string.Empty,
            MarkerTypeName,
            TypeAttributes.NestedPublic | TypeAttributes.Class | TypeAttributes.Abstract | TypeAttributes.Sealed | TypeAttributes.SpecialName,
            module.TypeSystem.Object);
        groupingType.NestedTypes.Add(markerType);

        var markerMethod = new MethodDefinition(
            MarkerMethodName,
            MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig | MethodAttributes.SpecialName,
            module.TypeSystem.Void);
        markerMethod.Parameters.Add(new ParameterDefinition("self", ParameterAttributes.None, module.ImportReference(widgetType)));
        markerMethod.Body.Instructions.Add(Instruction.Create(OpCodes.Ret));
        markerType.Methods.Add(markerMethod);

        var createSkeleton = new MethodDefinition(
            "Create",
            MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig,
            module.ImportReference(widgetType));
        createSkeleton.Parameters.Add(new ParameterDefinition("value", ParameterAttributes.None, module.TypeSystem.Int32));
        groupingType.Methods.Add(createSkeleton);

        var notImplementedCtor = module.ImportReference(typeof(NotImplementedException).GetConstructor(Type.EmptyTypes)!);
        var il = createSkeleton.Body.GetILProcessor();
        il.Append(Instruction.Create(OpCodes.Newobj, notImplementedCtor));
        il.Append(Instruction.Create(OpCodes.Throw));

        var markerAttributeCtor = GetOrCreateExtensionMarkerNameAttribute(module);
        var markerAttribute = new CustomAttribute(markerAttributeCtor);
        markerAttribute.ConstructorArguments.Add(new CustomAttributeArgument(module.TypeSystem.String, MarkerTypeName));
        createSkeleton.CustomAttributes.Add(markerAttribute);

        assembly.Write(assemblyPath);
        return 0;
    }

    private static MethodReference GetOrCreateExtensionMarkerNameAttribute(ModuleDefinition module)
    {
        const string attributeNamespace = "System.Runtime.CompilerServices";
        const string attributeName = "ExtensionMarkerNameAttribute";

        var existing = module.Types.FirstOrDefault(t => t.Namespace == attributeNamespace && t.Name == attributeName);
        if (existing is null)
        {
            var attributeBase = module.ImportReference(typeof(Attribute));
            existing = new TypeDefinition(
                attributeNamespace,
                attributeName,
                TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.Sealed,
                attributeBase);
            module.Types.Add(existing);

            var ctor = new MethodDefinition(
                ".ctor",
                MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.SpecialName | MethodAttributes.RTSpecialName,
                module.TypeSystem.Void);
            ctor.Parameters.Add(new ParameterDefinition("markerName", ParameterAttributes.None, module.TypeSystem.String));
            var il = ctor.Body.GetILProcessor();
            var baseCtor = module.ImportReference(attributeBase.Resolve()!.Methods.Single(m => m.IsConstructor && !m.IsStatic && !m.HasParameters));
            il.Append(Instruction.Create(OpCodes.Ldarg_0));
            il.Append(Instruction.Create(OpCodes.Call, baseCtor));
            il.Append(Instruction.Create(OpCodes.Ret));
            existing.Methods.Add(ctor);
        }

        var markerCtor = existing.Methods.Single(m => m.IsConstructor && m.Parameters.Count == 1 && m.Parameters[0].ParameterType.MetadataType == MetadataType.String);
        return module.ImportReference(markerCtor);
    }
}

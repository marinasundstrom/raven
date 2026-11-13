using System;
using System.Linq;
using System.Reflection;

namespace Raven.CodeAnalysis.Macros;

internal static class MacroExportDiscovery
{
    public static IMacroDefinition CreateMacroInstance(Type macroType)
    {
        try
        {
            return (IMacroDefinition)Activator.CreateInstance(macroType, nonPublic: true)!;
        }
        catch (Exception exception)
        {
            var failure = exception;
            while (failure is TargetInvocationException { InnerException: { } inner })
                failure = inner;

            var detail = string.IsNullOrWhiteSpace(failure.Message)
                ? failure.GetType().Name
                : failure.Message;
            throw new InvalidOperationException(
                $"Macro provider '{macroType.FullName ?? macroType.Name}' could not be created: {detail}",
                failure);
        }
    }

    public static Type[] GetExportedTypes(Assembly assembly)
    {
        var markers = assembly.GetCustomAttributes<RavenCompilerPluginAttribute>().ToArray();
        var declaredTypes = markers
            .Select(static marker => marker.ExportedType)
            .Where(static type => type is not null)
            .Cast<Type>()
            .ToArray();

        if (declaredTypes.Length > 0)
        {
            if (markers.Any(static marker => marker.ExportedType is null))
            {
                throw new InvalidOperationException(
                    $"Compiler plugin assembly '{assembly.GetName().Name}' mixes explicit entry points with the fallback-discovery marker.");
            }

            foreach (var declaredType in declaredTypes)
                ValidateExportedType(assembly, declaredType);

            return declaredTypes
                .Where(static type => typeof(IMacroDefinition).IsAssignableFrom(type))
                .Distinct()
                .ToArray();
        }

        var exportedTypes = assembly.GetTypes();
        if (markers.Length > 0)
            exportedTypes = exportedTypes.Where(static type => type.IsPublic).ToArray();

        return exportedTypes
            .Where(IsConstructibleExportedType)
            .ToArray();
    }

    private static void ValidateExportedType(Assembly assembly, Type exportedType)
    {
        if (exportedType.Assembly != assembly)
        {
            throw new InvalidOperationException(
                $"Compiler plugin export '{exportedType.FullName}' must be declared in assembly '{assembly.GetName().Name}'.");
        }

        if (!IsConstructibleExportedType(exportedType))
        {
            throw new InvalidOperationException(
                $"Compiler plugin export '{exportedType.FullName}' must be a non-abstract macro class with one supported Expand contract and a parameterless constructor.");
        }
    }

    public static bool IsConstructibleExportedType(Type type)
        => type.IsClass
            && typeof(IMacroDefinition).IsAssignableFrom(type)
            && HasExactlyOneMacroRole(type)
            && !type.IsAbstract
            && !type.ContainsGenericParameters
            && type.GetConstructor(
                BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance,
                binder: null,
                Type.EmptyTypes,
                modifiers: null) is not null;

    private static bool HasExactlyOneMacroRole(Type type)
        => typeof(IMacroExecutor).IsAssignableFrom(type) ||
            MethodMacroFacts.TryGetExpandMethod(type, out _);
}

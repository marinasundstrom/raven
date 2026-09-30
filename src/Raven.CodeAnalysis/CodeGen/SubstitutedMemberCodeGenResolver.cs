using System;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

// Resolve substituted semantic members against this emission's reflection types.
// Builder caches remain on CodeGenerator, never on the semantic symbols.
internal static class SubstitutedMemberCodeGenResolver
{
    internal static ConstructorInfo GetConstructorInfo(SubstitutedMethodSymbol symbol, CodeGenerator codeGen)
    {
        var original = symbol.OriginalDefinition!;
        var constructed = (ConstructedNamedTypeSymbol)symbol.ContainingType!;
        var cacheArguments = constructed.GetAllTypeArguments();

        if (original is SourceMethodSymbol cachedSource &&
            codeGen.TryGetMemberBuilder(cachedSource, cacheArguments, out var cachedMember) &&
            cachedMember is ConstructorInfo cachedConstructor)
        {
            return cachedConstructor;
        }

        if (original is PEMethodSymbol peMethod)
        {
            var baseCtor = MethodSymbolCodeGenResolver.GetClrConstructorInfo(peMethod, codeGen);

            if (baseCtor.DeclaringType.IsGenericType)
            {
                var constructedType = ConstructedTypeCodeGenResolver.GetTypeInfo(constructed, codeGen).AsType();

                if (constructedType.GetType().FullName == "System.Reflection.Emit.TypeBuilderInstantiation")
                {
                    var constructedCtor = TypeBuilder.GetConstructor(constructedType, baseCtor);
                    if (constructedCtor is not null)
                        return constructedCtor;
                }

                var parameterTypes = symbol.Parameters
                    .Select(parameter => TypeSymbolExtensionsForCodeGen.GetClrType(parameter.Type, codeGen))
                    .ToArray();

                var resolved = constructedType.GetConstructor(
                    BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic,
                    binder: null,
                    types: parameterTypes,
                    modifiers: null);

                if (resolved is not null)
                    return resolved;

                if (constructedType is TypeBuilder typeBuilder)
                {
                    var constructedCtor = TypeBuilder.GetConstructor(typeBuilder, baseCtor);
                    if (constructedCtor is not null)
                        return constructedCtor;
                }

                throw new InvalidOperationException($"Unable to resolve constructed constructor for '{constructed}' from metadata definition '{original}'.");
            }

            return baseCtor;
        }

        if (original is SubstitutedMethodSymbol substitutedMethod)
            return GetConstructorInfo(substitutedMethod, codeGen);

        if (original is ConstructedMethodSymbol constructedMethod)
            return MethodSymbolCodeGenResolver.GetClrConstructorInfo(constructedMethod, codeGen);

        var unwrapped = original;
        while (unwrapped.UnderlyingSymbol is IMethodSymbol underlying &&
               !ReferenceEquals(underlying, unwrapped))
        {
            unwrapped = underlying;
        }

        if (!ReferenceEquals(unwrapped, original))
        {
            var rebound = unwrapped is SubstitutedMethodSymbol or ConstructedMethodSymbol
                ? unwrapped
                : new SubstitutedMethodSymbol(unwrapped, constructed);

            return MethodSymbolCodeGenResolver.GetClrConstructorInfo(rebound, codeGen);
        }

        if (original is SourceMethodSymbol sourceMethod)
        {
            var constructedType = ConstructedTypeCodeGenResolver.GetTypeInfo(constructed, codeGen).AsType();
            if (codeGen.GetMemberBuilder(sourceMethod) is ConstructorInfo definitionCtor)
            {
                var constructedCtor = TypeBuilder.GetConstructor(constructedType, definitionCtor);
                if (constructedCtor is not null)
                {
                    codeGen.AddMemberBuilder(sourceMethod, constructedCtor, cacheArguments);
                    return constructedCtor;
                }
            }

            throw new InvalidOperationException("Constructor builder not found for source method.");
        }

        throw new Exception("Unexpected method kind");
    }

    internal static MethodInfo GetMethodInfo(SubstitutedMethodSymbol symbol, CodeGenerator codeGen)
    {
        var original = symbol.OriginalDefinition!;
        var constructed = (ConstructedNamedTypeSymbol)symbol.ContainingType!;
        var cacheArguments = constructed.GetAllTypeArguments();

        if (original is SourceMethodSymbol cachedSource &&
            codeGen.TryGetMemberBuilder(cachedSource, cacheArguments, out var cachedMember) &&
            cachedMember is MethodInfo cachedMethod)
        {
            return cachedMethod;
        }

        if (original is PEMethodSymbol peMethod)
        {
            var baseMethod = MethodSymbolCodeGenResolver.GetClrMethodInfo(peMethod, codeGen);

            // Resolve the constructed runtime type
            var constructedType = ConstructedTypeCodeGenResolver.GetTypeInfo(constructed, codeGen).AsType();

            if (constructedType.IsGenericType &&
                baseMethod.DeclaringType is Type baseDeclaringTypeForInstantiation &&
                baseDeclaringTypeForInstantiation.IsGenericTypeDefinition &&
                ReferenceEquals(constructedType.GetGenericTypeDefinition(), baseDeclaringTypeForInstantiation))
            {
                try
                {
                    var instantiated = TypeBuilder.GetMethod(constructedType, baseMethod);
                    if (instantiated is not null)
                        return instantiated;
                }
                catch (Exception ex) when (ex is NotSupportedException or ArgumentException)
                {
                }
            }

            // Use metadata name and parameter types to resolve the method on the constructed type
            if (!IsSignaturePlaceholderType(constructedType))
            {
                var parameterTypes = symbol.Parameters
                    .Select(x => TypeSymbolExtensionsForCodeGen.GetClrType(x.Type, codeGen))
                    .ToArray();
                var method = constructedType.GetMethod(
                    baseMethod.Name,
                    BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static,
                    null,
                    parameterTypes,
                    null
                );

                if (method != null)
                    return method;

                // Fallback: metadata-token matching is more resilient for generic instantiations
                // where reflected parameter types can differ from substituted symbol projections.
                if (baseMethod.DeclaringType is Type baseDeclaringType &&
                    constructedType.IsGenericType &&
                    baseDeclaringType.IsGenericTypeDefinition &&
                    ReferenceEquals(constructedType.GetGenericTypeDefinition(), baseDeclaringType))
                {
                    var candidates = constructedType.GetMethods(
                        BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);

                    foreach (var candidate in candidates)
                    {
                        if (candidate.Name != baseMethod.Name)
                            continue;

                        if (candidate.GetParameters().Length != baseMethod.GetParameters().Length)
                            continue;

                        if (candidate.IsGenericMethod != baseMethod.IsGenericMethod)
                            continue;

                        try
                        {
                            if (candidate.MetadataToken == baseMethod.MetadataToken)
                                return candidate;
                        }
                        catch
                        {
                            // Some reflected members (e.g. dynamic methods) can throw for MetadataToken;
                            // skip and continue probing.
                        }
                    }
                }
            }
            else
            {
                return baseMethod;
            }

            throw new MissingMethodException($"Method '{baseMethod.Name}' with specified parameters not found on constructed type '{constructedType}'.");
        }

        if (original is SourceMethodSymbol sourceMethod)
        {
            if (codeGen.GetMemberBuilder(sourceMethod) is not MethodInfo definitionMethod)
                throw new InvalidOperationException("Method builder not found for source method.");

            var constructedType = ConstructedTypeCodeGenResolver.GetTypeInfo(constructed, codeGen).AsType();

            if (!ReferenceEquals(constructedType, definitionMethod.DeclaringType) && constructedType.IsGenericType)
            {
                var constructedMethod = TypeBuilder.GetMethod(constructedType, definitionMethod);
                if (constructedMethod is not null)
                {
                    codeGen.AddMemberBuilder(sourceMethod, constructedMethod, cacheArguments);
                    return constructedMethod;
                }
            }

            if (ReferenceEquals(constructedType, definitionMethod.DeclaringType))
                return definitionMethod;

            if (!IsSignaturePlaceholderType(constructedType))
            {
                var parameterTypes = sourceMethod.Parameters
                    .Select(p => TypeSymbolExtensionsForCodeGen.GetClrType(p.Type, codeGen))
                    .ToArray();

                var bindingFlags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;
                var resolved = constructedType.GetMethod(definitionMethod.Name, bindingFlags, null, parameterTypes, null);
                if (resolved is not null)
                {
                    codeGen.AddMemberBuilder(sourceMethod, resolved, cacheArguments);
                    return resolved;
                }
            }
            else
            {
                return definitionMethod;
            }

            throw new MissingMethodException($"Method '{definitionMethod.Name}' with specified parameters not found on constructed type '{constructedType}'.");
        }

        throw new InvalidOperationException("Expected PE or source method symbol.");
    }

    private static bool IsSignaturePlaceholderType(Type runtimeType)
    {
        try
        {
            _ = runtimeType.Assembly;
            return false;
        }
        catch (NotSupportedException)
        {
            return true;
        }
    }

    internal static FieldInfo GetFieldInfo(SubstitutedFieldSymbol symbol, CodeGenerator codeGen)
    {
        var original = symbol.OriginalDefinition;
        var constructed = (ConstructedNamedTypeSymbol)symbol.ContainingType!;
        var cacheArguments = constructed.GetAllTypeArguments();

        if (original is SourceFieldSymbol cachedSource &&
            codeGen.TryGetMemberBuilder(cachedSource, cacheArguments, out var cachedMember) &&
            cachedMember is FieldInfo cachedField)
        {
            return cachedField;
        }

        if (original is PEFieldSymbol peField)
        {
            var constructedType = ConstructedTypeCodeGenResolver.GetTypeInfo(constructed, codeGen).AsType();
            var bindingFlags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;
            var expectedType = TypeSymbolExtensionsForCodeGen.GetClrTypeTreatingUnitAsVoid(symbol.Type, codeGen);

            FieldInfo[] fields;
            try
            {
                fields = constructedType.GetFields(bindingFlags);
            }
            catch (NotSupportedException) when (constructedType.IsConstructedGenericType)
            {
                // A runtime generic containing an unbaked source type cannot enumerate
                // members. Map the definition's field onto its emitted construction.
                var definitionField = constructedType.GetGenericTypeDefinition()
                    .GetField(peField.MetadataName, bindingFlags)
                    ?? throw new MissingFieldException(constructedType.FullName, peField.MetadataName);
                return TypeBuilder.GetField(constructedType, definitionField);
            }

            foreach (var candidate in fields)
            {
                if (!string.Equals(candidate.Name, peField.MetadataName, StringComparison.Ordinal))
                    continue;

                if (candidate.IsStatic != peField.IsStatic)
                    continue;

                var candidateFieldType = candidate.FieldType;
                if (candidateFieldType == expectedType ||
                    (candidateFieldType.IsGenericTypeDefinition && expectedType.IsGenericType && candidateFieldType == expectedType.GetGenericTypeDefinition()) ||
                    (candidateFieldType.IsGenericType && expectedType.IsGenericTypeDefinition && candidateFieldType.GetGenericTypeDefinition() == expectedType))
                {
                    return candidate;
                }
            }

            throw new MissingFieldException(constructedType.FullName, peField.MetadataName);
        }

        if (original is SourceFieldSymbol sourceField)
        {
            if (codeGen.GetMemberBuilder(sourceField) is not FieldInfo definitionField)
                throw new InvalidOperationException("Field builder not found for source field.");

            var constructedType = ConstructedTypeCodeGenResolver.GetTypeInfo(constructed, codeGen).AsType();

            if (!ReferenceEquals(constructedType, definitionField.DeclaringType) && constructedType.IsGenericType)
            {
                if (constructed.TypeArguments.Any(static argument =>
                        argument is ITypeParameterSymbol typeParameter &&
                        typeParameter.OwnerKind == TypeParameterOwnerKind.Method))
                {
                    try
                    {
                        var genericDefinition = constructedType.IsGenericTypeDefinition
                            ? constructedType
                            : constructedType.GetGenericTypeDefinition();
                        var projectedArguments = constructed.TypeArguments
                            .Select(argument =>
                            {
                                if (argument is ITypeParameterSymbol typeParameter &&
                                    typeParameter.OwnerKind == TypeParameterOwnerKind.Method &&
                                    typeParameter.Ordinal >= 0)
                                {
                                    return System.Type.MakeGenericMethodParameter(typeParameter.Ordinal);
                                }

                                return TypeSymbolExtensionsForCodeGen.GetClrTypeTreatingUnitAsVoid(argument, codeGen);
                            })
                            .ToArray();

                        if (genericDefinition.GetGenericArguments().Length == projectedArguments.Length)
                            constructedType = genericDefinition.MakeGenericType(projectedArguments);
                    }
                    catch (NotSupportedException)
                    {
                    }
                    catch (InvalidOperationException)
                    {
                    }
                    catch (ArgumentException)
                    {
                    }
                }

                var constructedField = TypeBuilder.GetField(constructedType, definitionField);
                if (constructedField is not null)
                {
                    if (CodeGenFlags.PrintDebug)
                    {
                        static string GetOwner(Type argument)
                        {
                            if (!argument.IsGenericParameter)
                                return "n/a";

                            try
                            {
                                return argument.DeclaringMethod is null ? "type" : "method";
                            }
                            catch (NotSupportedException)
                            {
                                return "method";
                            }
                        }

                        var fieldType = constructedField.FieldType;
                        var owner = fieldType.IsGenericParameter
                            ? GetOwner(fieldType)
                            : "n/a";
                        var containingArgs = constructedType.IsGenericType
                            ? string.Join(
                                ", ",
                                constructedType.GetGenericArguments().Select(argument =>
                                    argument.IsGenericParameter
                                        ? $"{argument}[owner={GetOwner(argument)}]"
                                        : argument.ToString()))
                            : "<non-generic>";
                        DebugUtils.PrintDebug(
                            $"[CodeGen:Field] Constructed field {original.Name} on {constructedType} args=[{containingArgs}] -> {fieldType} (genericOwner={owner})");
                    }

                    codeGen.AddMemberBuilder(sourceField, constructedField, cacheArguments);
                    return constructedField;
                }
            }

            if (ReferenceEquals(constructedType, definitionField.DeclaringType))
                return definitionField;

            var bindingFlags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;
            var resolved = constructedType.GetField(definitionField.Name, bindingFlags);
            if (resolved is not null)
            {
                codeGen.AddMemberBuilder(sourceField, resolved, cacheArguments);
                return resolved;
            }

            throw new MissingFieldException(constructedType.FullName, definitionField.Name);
        }

        throw new Exception("Not a supported field symbol.");
    }
}

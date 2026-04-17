using System;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static partial class SynthesizedMethodBodyFactory
{
    public static bool TryCreate(Compilation compilation, IMethodSymbol method, out BoundBlockStatement body)
    {
        if (method.Name == SynthesizedUnionMethodNames.DisplayNameHelper &&
            method.Parameters.Length == 0 &&
            method.ReturnType.SpecialType == SpecialType.System_String)
        {
            switch (method.ContainingType)
            {
                case SourceUnionSymbol unionSymbol:
                    body = CreateUnionDisplayNameHelperBody(compilation, unionSymbol);
                    return true;

                case SourceUnionCaseTypeSymbol caseSymbol:
                    body = CreateUnionDisplayNameHelperBody(compilation, method, caseSymbol);
                    return true;
            }
        }

        if (method.Name == SynthesizedUnionMethodNames.FriendlyTypeNameHelper &&
            method.IsStatic &&
            method.Parameters.Length == 1 &&
            method.Parameters[0].Type.SpecialType == SpecialType.System_Type &&
            method.ReturnType.SpecialType == SpecialType.System_String)
        {
            switch (method.ContainingType)
            {
                case SourceUnionSymbol:
                case SourceUnionCaseTypeSymbol:
                case SourceNamedTypeSymbol { IsRecord: true }:
                    body = CreateFriendlyTypeNameHelperBody(compilation, method);
                    return true;
            }
        }

        if (method.Name == SynthesizedUnionMethodNames.FormatValueHelper &&
            method.IsStatic &&
            method.Parameters.Length == 3 &&
            method.Parameters[0].Type.SpecialType == SpecialType.System_Object &&
            method.Parameters[1].Type.SpecialType == SpecialType.System_Type &&
            method.Parameters[2].Type.SpecialType == SpecialType.System_Boolean &&
            method.ReturnType.SpecialType == SpecialType.System_String)
        {
            body = CreateUnionFormatValueHelperBody(compilation, method);
            return true;
        }

        if (method.MethodKind is MethodKind.PropertyGet or MethodKind.PropertySet or MethodKind.InitOnly &&
            method.ContainingSymbol is SourcePropertySymbol { IsAutoProperty: true, BackingField: SourceFieldSymbol } autoProperty)
        {
            body = CreateAutoPropertyAccessorBody(compilation, method, autoProperty);
            return true;
        }

        if (method.Name == "TryGetValue" &&
            method.Parameters.Length == 1 &&
            method.Parameters[0].RefKind == RefKind.Out &&
            method.ReturnType.SpecialType == SpecialType.System_Boolean &&
            method.DeclaringSyntaxReferences.IsDefaultOrEmpty &&
            TryGetSourceDiscriminatedUnionDefinition(method.ContainingType) is { } tryGetUnion)
        {
            body = CreateUnionTryGetValueBody(compilation, method, tryGetUnion, method.Parameters[0]);
            return true;
        }

        if (method.MethodKind == MethodKind.Constructor &&
            method.DeclaringSyntaxReferences.IsDefaultOrEmpty &&
            TryGetSourceDiscriminatedUnionDefinition(method.ContainingType) is { } constructorUnion)
        {
            body = CreateUnionCarrierConstructorBody(compilation, method, constructorUnion);
            return true;
        }

        if (method.MethodKind == MethodKind.Constructor &&
            method.DeclaringSyntaxReferences.IsDefaultOrEmpty &&
            TryGetSourceNamedTypeDefinition(method.ContainingType) is { IsRecord: true } recordNominalType &&
            method.Parameters.Length == 1 &&
            SymbolEqualityComparer.Default.Equals(method.Parameters[0].Type, method.ContainingType))
        {
            body = CreateRecordCopyConstructorBody(compilation, method, recordNominalType);
            return true;
        }

        if (method.MethodKind == MethodKind.Constructor &&
            TryGetUnionCaseType(method.ContainingType) is { } caseType)
        {
            body = CreateUnionCaseConstructorBody(compilation, method, caseType);
            return true;
        }

        if (method.MethodKind == MethodKind.PropertyGet &&
            method.ContainingSymbol is SourcePropertySymbol { Name: "Value" } &&
            method.ContainingType is SourceUnionSymbol unionValueType)
        {
            body = CreateUnionValuePropertyGetterBody(compilation, method, unionValueType);
            return true;
        }

        if (method.MethodKind == MethodKind.PropertyGet &&
            method.ContainingSymbol is SourcePropertySymbol { Name: "HasValue" } &&
            method.ContainingType is SourceUnionSymbol unionHasValueType)
        {
            body = CreateUnionHasValuePropertyGetterBody(compilation, method, unionHasValueType);
            return true;
        }

        if (method.MethodKind == MethodKind.PropertyGet &&
            TryGetSynthesizableUnionCaseProperty(method) is { } unionCaseProperty)
        {
            body = CreateUnionCasePropertyGetterBody(method, unionCaseProperty);
            return true;
        }

        if (method.Name == "Deconstruct" &&
            method.MethodKind == MethodKind.Ordinary &&
            method.ReturnType.SpecialType == SpecialType.System_Unit &&
            method.DeclaringSyntaxReferences.IsDefaultOrEmpty &&
            TryGetUnionCaseType(method.ContainingType) is { } deconstructCase)
        {
            body = CreateUnionCaseDeconstructBody(compilation, method, deconstructCase);
            return true;
        }

        if (method.Name == "Deconstruct" &&
            method.MethodKind == MethodKind.Ordinary &&
            method.ReturnType.SpecialType == SpecialType.System_Unit &&
            method.DeclaringSyntaxReferences.IsDefaultOrEmpty &&
            TryGetSourceNamedTypeDefinition(method.ContainingType) is { } nominalType)
        {
            var deconstructProperties = GetNominalDeconstructProperties(nominalType);
            if (!deconstructProperties.IsDefaultOrEmpty)
            {
                body = CreateNominalDeconstructBody(compilation, method, deconstructProperties);
                return true;
            }
        }

        if (method.Name == nameof(object.ToString) &&
            method.Parameters.Length == 0 &&
            method.ReturnType.SpecialType == SpecialType.System_String)
        {
            switch (method.ContainingType)
            {
                case SourceNamedTypeSymbol { IsRecord: true } recordType:
                    body = CreateRecordToStringBody(compilation, method, recordType);
                    return true;

                case SourceUnionSymbol unionSymbol:
                    body = CreateUnionToStringBody(compilation, method, unionSymbol);
                    return true;

                case SourceUnionCaseTypeSymbol caseSymbol:
                    body = CreateUnionCaseToStringBody(compilation, method, caseSymbol);
                    return true;
            }
        }

        if (method.Name == nameof(object.GetHashCode) &&
            method.Parameters.Length == 0 &&
            method.ReturnType.SpecialType == SpecialType.System_Int32 &&
            method.DeclaringSyntaxReferences.IsDefaultOrEmpty &&
            TryGetSourceNamedTypeDefinition(method.ContainingType) is { IsRecord: true } hashRecordType)
        {
            body = CreateRecordGetHashCodeBody(compilation, method, hashRecordType);
            return true;
        }

        if (method.Name == nameof(object.Equals) &&
            method.Parameters.Length == 1 &&
            method.ReturnType.SpecialType == SpecialType.System_Boolean &&
            method.DeclaringSyntaxReferences.IsDefaultOrEmpty &&
            TryGetSourceNamedTypeDefinition(method.ContainingType) is { IsRecord: true, IsValueType: false } equalsRecordType)
        {
            if (method.Parameters[0].Type.SpecialType == SpecialType.System_Object)
            {
                body = CreateRecordObjectEqualsBody(compilation, method, equalsRecordType);
                return true;
            }

            if (SymbolEqualityComparer.Default.Equals(method.Parameters[0].Type, method.ContainingType))
            {
                body = CreateRecordTypedEqualsBody(compilation, method, equalsRecordType);
                return true;
            }
        }

        if (method.MethodKind == MethodKind.UserDefinedOperator &&
            method.Parameters.Length == 2 &&
            method.ReturnType.SpecialType == SpecialType.System_Boolean &&
            method.DeclaringSyntaxReferences.IsDefaultOrEmpty &&
            TryGetSourceNamedTypeDefinition(method.ContainingType) is { IsRecord: true } operatorRecordType &&
            SymbolEqualityComparer.Default.Equals(method.Parameters[0].Type, method.ContainingType) &&
            SymbolEqualityComparer.Default.Equals(method.Parameters[1].Type, method.ContainingType))
        {
            if (string.Equals(method.Name, "op_Equality", StringComparison.Ordinal))
            {
                body = CreateRecordEqualityOperatorBody(compilation, method, operatorRecordType);
                return true;
            }

            if (string.Equals(method.Name, "op_Inequality", StringComparison.Ordinal))
            {
                body = CreateRecordInequalityOperatorBody(compilation, method, operatorRecordType);
                return true;
            }
        }

        body = null!;
        return false;
    }
}

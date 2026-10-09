using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen.Portable;

internal enum EmissionOverrideKind { None, ObjectToString, ObjectHashCode, ObjectEquals }

// A source declaration, not a backend definition. In particular, an assembly function
// has no logical type owner even when the CLI symbol model supplies a carrier type.
internal sealed record SourceCallablePlan(
    IMethodSymbol Symbol, SyntaxNode Syntax, SyntaxNode? Body,
    INamedTypeSymbol? TypeOwner, string MetadataName, CallableSignature Signature, bool IsSynthesizedStatic = false, BoundBlockStatement? PreparedBody = null)
{
    internal string Namespace { get; } = GetNamespace(Symbol.ContainingNamespace);
    private static string GetNamespace(INamespaceSymbol? scope)
    {
        if (scope is null || scope.IsGlobalNamespace) return "";
        var names = new Stack<string>();
        for (; scope is { IsGlobalNamespace: false }; scope = scope.ContainingNamespace)
            names.Push(scope.Name);
        return string.Join(".", names);
    }
    internal EmissionOverrideKind Override { get; } = ClassifyOverride(Symbol);
    // Reference nullability does not change this physical slot; binding still owns
    // Raven's override compatibility rules and supplies the resolved target.
    internal static EmissionOverrideKind ClassifyOverride(IMethodSymbol method)
    {
        if (method.OriginalDefinition is not SourceMethodSymbol
            {
                IsOverride: true, IsStatic: false, IsAbstract: false, MethodKind: MethodKind.Ordinary,
                DeclaredAccessibility: Accessibility.Public, TypeParameters.Length: 0
            } source) return EmissionOverrideKind.None;
        IMethodSymbol? slot = source.OverriddenMethod;
        for (int depth = 0; depth < 16 && slot is SourceMethodSymbol { IsOverride: true } inherited; depth++) slot = inherited.OverriddenMethod;
        if (slot is not
            {
                IsStatic: false, IsVirtual: true, IsAbstract: false, TypeParameters.Length: 0,
                ContainingType.SpecialType: SpecialType.System_Object
            } || slot.Name != source.Name ||
            slot.Parameters.Length != source.Parameters.Length) return EmissionOverrideKind.None;
        bool Result(SpecialType type) => source.ReturnType.GetNonNullableType().SpecialType == type && slot.ReturnType.GetNonNullableType().SpecialType == type;
        return source.Name switch
        {
            "ToString" when source.Parameters.IsEmpty && Result(SpecialType.System_String) => EmissionOverrideKind.ObjectToString,
            "GetHashCode" when source.Parameters.IsEmpty && Result(SpecialType.System_Int32) => EmissionOverrideKind.ObjectHashCode,
            "Equals" when source.Parameters is [{ RefKind: RefKind.None, Type: var value }] &&
                value.GetNonNullableType().SpecialType == SpecialType.System_Object &&
                slot.Parameters[0].Type.GetNonNullableType().SpecialType == SpecialType.System_Object && Result(SpecialType.System_Boolean) => EmissionOverrideKind.ObjectEquals,
            _ => EmissionOverrideKind.None
        };
    }

    internal static bool IsClassVirtualSlot(IMethodSymbol method, EmissionCapabilities? capabilities) =>
        capabilities?.AllowsClassVirtualSlots == true &&
        method is
        {
            IsStatic: false, MethodKind: MethodKind.Ordinary, Arity: 0, DeclaredAccessibility: Accessibility.Public,
            ContainingType: { IsReferenceType: true, Arity: 0, ContainingType: null } owner
        } &&
        owner.OriginalDefinition is SourceNamedTypeSymbol && (method.IsAbstract || method.IsVirtual || method.IsOverride);

    internal bool IsObjectRootSlot => IsRootSlot(Symbol);
    private static bool IsRootSlot(IMethodSymbol method) => method.ContainingType is { } owner && SourceTypePlan.IsSourceObjectRoot(owner) &&
        method is { IsVirtual: true, IsOverride: false, IsAbstract: false, IsStatic: false, Arity: 0, DeclaredAccessibility: Accessibility.Public } &&
        (method.Name == "ToString" && method.Parameters.IsEmpty && method.ReturnType.SpecialType == SpecialType.System_String ||
         method.Name == "GetHashCode" && method.Parameters.IsEmpty && method.ReturnType.SpecialType == SpecialType.System_Int32 ||
         method.Name == "Equals" && method.ReturnType.SpecialType == SpecialType.System_Boolean && method.Parameters is [{ RefKind: RefKind.None, Type: var argument }] &&
         SymbolEqualityComparer.Default.Equals(argument.GetNonNullableType(), owner));

    internal EmissionDeclarationKind DeclarationKind => IsObjectRootSlot ? EmissionDeclarationKind.ObjectRootSlot : Override != EmissionOverrideKind.None ? Symbol.ContainingType.IsValueType ? EmissionDeclarationKind.ValueObjectOverride : EmissionDeclarationKind.ReferenceObjectOverride : IsAssemblyFunction
        ? Namespace.Length == 0 ? EmissionDeclarationKind.AssemblyFunction : EmissionDeclarationKind.NamespacedAssemblyFunction
        : Symbol.MethodKind == MethodKind.Constructor ? EmissionDeclarationKind.Constructor
        : Symbol.MethodKind is MethodKind.PropertyGet or MethodKind.PropertySet
            ? Symbol.ContainingSymbol is IPropertySymbol { IsIndexer: true } ? EmissionDeclarationKind.IndexerAccessor : EmissionDeclarationKind.PropertyAccessor
        : Symbol.IsStatic ? EmissionDeclarationKind.StaticMethod : EmissionDeclarationKind.InstanceMethod;
    internal Accessibility Visibility => IsSynthesizedStatic ? Accessibility.Internal : Symbol.DeclaredAccessibility;
    internal bool IsSupportedBy(EmissionCapabilities capabilities) => capabilities.Allows(DeclarationKind) && capabilities.Allows(Signature) &&
        (IsAssemblyFunction ? capabilities.AllowsFunctionVisibility(Visibility) :
            Visibility == Accessibility.ProtectedAndProtected ? Symbol.MethodKind == MethodKind.Constructor && capabilities.AllowsProtectedConstructors :
            capabilities.AllowsMethodVisibility(Visibility));

    internal bool IsAssemblyFunction => TypeOwner is null;

    internal static bool TryCreate(IMethodSymbol symbol, out SourceCallablePlan? plan, EmissionCapabilities? capabilities = null, SyntaxNode? synthesizedAnchor = null)
    {
        plan = null;
        if (symbol.IsExtern || symbol.IsOverride && symbol.ContainingType is { IsReferenceType: true, Arity: > 0 } ||
            !CallableSignature.TryCreate(symbol, out var signature, capabilities)) return false;
        if (!symbol.IsStatic && (symbol.MethodKind is not (MethodKind.Ordinary or MethodKind.Constructor or MethodKind.PropertyGet or MethodKind.PropertySet) || symbol.IsAbstract && !IsClassVirtualSlot(symbol, capabilities) || (symbol.IsVirtual || symbol.IsOverride) && !IsClassVirtualSlot(symbol, capabilities) &&
            (!(IsRootSlot(symbol) && capabilities?.Allows(EmissionDeclarationKind.ObjectRootSlot) == true) &&
             (ClassifyOverride(symbol) == EmissionOverrideKind.None || capabilities?.Allows(symbol.ContainingType?.IsValueType == true ? EmissionDeclarationKind.ValueObjectOverride : EmissionDeclarationKind.ReferenceObjectOverride) != true)) ||
            symbol.ContainingType is not { } receiver || !SourceTypePlan.TryCreate(receiver, out _, capabilities))) return false;
        if (symbol.ContainingSymbol is SourcePropertySymbol { IsAutoProperty: true, IsStatic: false, BackingField: { } } property &&
            (symbol.DeclaringSyntaxReferences.IsEmpty || symbol.DeclaringSyntaxReferences is [{ } accessorReference] &&
                accessorReference.GetSyntax() is AccessorDeclarationSyntax { Body: null, ExpressionBody: null } ||
                capabilities?.Allows(EmissionDeclarationKind.PositionalRecordStorage) == true && symbol.DeclaringSyntaxReferences is [{ } parameterReference] &&
                parameterReference.GetSyntax() is ParameterSyntax && symbol.ContainingType is SourceNamedTypeSymbol { IsRecord: true, IsValueType: true }) && property.DeclaringSyntaxReferences.Length == 1)
        {
            var propertySyntax = property.DeclaringSyntaxReferences[0].GetSyntax();
            plan = new(symbol, propertySyntax, propertySyntax, symbol.ContainingType, symbol.MetadataName, signature);
            if (capabilities is null || plan.IsSupportedBy(capabilities)) return true;
            plan = null; return false;
        }
        if (symbol is { MethodKind: MethodKind.Constructor, IsStatic: false, Parameters.Length: 0 } &&
            symbol.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax() is TypeDeclarationSyntax ownerSyntax && ownerSyntax is ClassDeclarationSyntax or StructDeclarationSyntax)
        {
            plan = new(symbol, ownerSyntax, ownerSyntax, symbol.ContainingType, symbol.MetadataName, signature);
            if (capabilities is null || plan.IsSupportedBy(capabilities)) return true;
            plan = null; return false;
        }
        if (symbol is SourceMethodSymbol && synthesizedAnchor is UnionDeclarationSyntax unionAnchor &&
            HasSynthesizedUnionBody(symbol, unionAnchor))
        {
            // The anchor supplies diagnostics and the semantic model. The body still
            // comes from Compilation.TryGetSynthesizedMethodBody during lowering.
            plan = new(symbol, synthesizedAnchor, synthesizedAnchor, symbol.ContainingType, symbol.MetadataName, signature);
            if (capabilities is null || plan.IsSupportedBy(capabilities)) return true;
            plan = null; return false;
        }
        if (synthesizedAnchor is RecordDeclarationSyntax recordAnchor &&
            (symbol.MethodKind == MethodKind.Constructor || symbol.Name == "Deconstruct") &&
            symbol.ContainingType is SourceNamedTypeSymbol { IsRecord: true, IsValueType: true } recordOwner &&
            capabilities?.Allows(EmissionDeclarationKind.PositionalRecordStorage) == true &&
            recordOwner.DeclaringSyntaxReferences.Any(reference => reference.SyntaxTree == recordAnchor.SyntaxTree && reference.Span == recordAnchor.Span) &&
            (symbol.DeclaringSyntaxReferences.IsEmpty || symbol.MethodKind == MethodKind.Constructor &&
                symbol.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax() is RecordDeclarationSyntax))
        {
            plan = new(symbol, recordAnchor, recordAnchor, symbol.ContainingType, symbol.MetadataName, signature);
            if (plan.IsSupportedBy(capabilities)) return true;
            plan = null; return false;
        }
        if (symbol.DeclaringSyntaxReferences.Length != 1) return false;
        var syntax = symbol.DeclaringSyntaxReferences[0].GetSyntax();
        switch (syntax)
        {
            case IndexerDeclarationSyntax indexerDeclaration when symbol.MethodKind == MethodKind.PropertyGet && indexerDeclaration.ExpressionBody is { } indexerBody:
                plan = new(symbol, syntax, indexerBody, symbol.ContainingType, symbol.MetadataName, signature);
                if (capabilities is null || plan.IsSupportedBy(capabilities)) return true;
                plan = null; return false;
            case PropertyDeclarationSyntax propertyDeclaration when symbol.MethodKind == MethodKind.PropertyGet && propertyDeclaration.ExpressionBody is { } expressionBody:
                plan = new(symbol, syntax, expressionBody, symbol.ContainingType, symbol.MetadataName, signature);
                if (capabilities is null || plan.IsSupportedBy(capabilities)) return true;
                plan = null; return false;
            case AccessorDeclarationSyntax accessor when symbol.MethodKind is MethodKind.PropertyGet or MethodKind.PropertySet:
                plan = new(symbol, syntax, (SyntaxNode?)accessor.Body ?? accessor.ExpressionBody, symbol.ContainingType, symbol.MetadataName, signature);
                if (plan.Body is not null && (capabilities is null || plan.IsSupportedBy(capabilities))) return true;
                plan = null; return false;
            case ConstructorDeclarationSyntax constructor when (HasRootInitialization(symbol, constructor) || capabilities?.AllowsLocalClassInheritance == true &&
                symbol is SourceMethodSymbol { ConstructorInitializer: { } initializer } &&
                SymbolEqualityComparer.Default.Equals(initializer.Constructor.ContainingType, symbol.ContainingType?.BaseType)) && symbol.ContainingType is { } constructorOwner:
                plan = new(symbol, syntax, (SyntaxNode?)constructor.Body ?? constructor.ExpressionBody, constructorOwner, symbol.MetadataName, signature);
                if (capabilities is null || plan.IsSupportedBy(capabilities)) return true;
                plan = null; return false;
            case ConversionOperatorDeclarationSyntax conversion when symbol.IsStatic && symbol.MethodKind == MethodKind.Conversion:
                plan = new(symbol, syntax, (SyntaxNode?)conversion.Body ?? conversion.ExpressionBody, symbol.ContainingType, symbol.MetadataName, signature);
                if (plan.Body is not null && (capabilities is null || plan.IsSupportedBy(capabilities))) return true;
                plan = null; return false;
            case OperatorDeclarationSyntax op when symbol.IsStatic && symbol.MethodKind == MethodKind.UserDefinedOperator:
                plan = new(symbol, syntax, (SyntaxNode?)op.Body ?? op.ExpressionBody, symbol.ContainingType, symbol.MetadataName, signature);
                if (plan.Body is not null && (capabilities is null || plan.IsSupportedBy(capabilities))) return true;
                plan = null; return false;
            case MethodDeclarationSyntax method when symbol.ContainingType is { } owner:
                plan = new(symbol, syntax, (SyntaxNode?)method.Body ?? method.ExpressionBody, owner, symbol.MetadataName, signature);
                if (capabilities is null || plan.IsSupportedBy(capabilities)) return true;
                plan = null;
                return false;
            case FunctionStatementSyntax function when symbol.IsStatic && function.Parent is GlobalStatementSyntax { Parent: CompilationUnitSyntax or BaseNamespaceDeclarationSyntax }:
                plan = new(symbol, syntax, (SyntaxNode?)function.Body ?? function.ExpressionBody, null, symbol.Name, signature);
                if (capabilities is null || plan.IsSupportedBy(capabilities)) return true;
                plan = null;
                return false;
            default:
                return false;
        }
    }

    private static bool HasSynthesizedUnionBody(IMethodSymbol method, UnionDeclarationSyntax anchor)
    {
        var union = method.ContainingType switch
        {
            SourceUnionSymbol owner => owner,
            SourceUnionCaseTypeSymbol @case => @case.Union as SourceUnionSymbol,
            _ => null
        };
        if (union is null || !union.DeclaringSyntaxReferences.Any(reference =>
            reference.SyntaxTree == anchor.SyntaxTree && reference.Span == anchor.Span)) return false;
        if (method.DeclaringSyntaxReferences.IsEmpty) return true;
        if (method.ContainingType is not SourceUnionCaseTypeSymbol || method.DeclaringSyntaxReferences.Length != 1) return false;
        // Case constructors and payload getters are generated, but retain case/parameter
        // source locations. They have no user-authored callable body to bind.
        return (method.MethodKind, method.DeclaringSyntaxReferences[0].GetSyntax()) switch
        {
            (MethodKind.Constructor, CaseDeclarationSyntax) => true,
            (MethodKind.PropertyGet, ParameterSyntax) => true,
            _ => false
        };
    }

    // Root construction is backend policy: CLI calls Object::.ctor; native roots have no base.
    // An explicit base() can use that policy only after binding proves the same contract.
    private static bool HasRootInitialization(IMethodSymbol symbol, ConstructorDeclarationSyntax syntax)
        => syntax.Initializer is null ||
           syntax.Initializer.Keyword.IsKind(SyntaxKind.BaseKeyword) &&
           syntax.Initializer.ArgumentList.Arguments.Count == 0 &&
           symbol is SourceMethodSymbol { ConstructorInitializer: { } initializer } &&
           initializer.Constructor is { MethodKind: MethodKind.Constructor, IsStatic: false, Parameters.Length: 0 } target &&
           target.ContainingType?.SpecialType == SpecialType.System_Object && !initializer.Arguments.Any();

    // The CLI adapter can supply a carrier/lifted name; native emission uses the source
    // metadata name. Concrete owner selection and physical visibility encoding remain backend policies.
    internal TMethod Define<TMethod>(ICallableDefinitionBuilder<TMethod> builder, string? emittedName = null)
        => builder.DefineMethod(emittedName ?? MetadataName, this);

    internal bool TryLowerBody(Compilation compilation, Func<BoundInvocationExpression, bool> permitsConsoleWrite,
        out LinearMethodBody? body, out LinearBodyFailure? failure, EmissionCapabilities capabilities)
    {
        if (!IsSupportedBy(capabilities))
        {
            body = null;
            failure = new("target does not support declaration " + DeclarationKind + " or its signature", Syntax);
            return false;
        }
        if (Body is null)
        {
            body = null;
            failure = new("source callable body unavailable", Syntax);
            return false;
        }
        return LinearMethodBody.TryLower(Symbol, compilation.GetSemanticModel(Body.SyntaxTree), Body,
            permitsConsoleWrite, out body, out failure, capabilities, preparedBody: PreparedBody);
    }
}

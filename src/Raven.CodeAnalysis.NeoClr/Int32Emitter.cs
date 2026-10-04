using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using MetadataMethod = NeoCLR.Metadata.Experimental.Model.MethodBuilder;

namespace Raven.CodeAnalysis.NeoClr;

// Native declaration collection and reference resolution. Body emission consumes the same
// lowered bound trees as the .NET backend through the shared portable instruction plan.
internal static class Int32Emitter
{
    internal static byte[] Emit(Compilation compilation, NeoClrEmitOptions options,
        IReadOnlyList<(IAssemblySymbol Symbol, NeoClrMetadataDependency Dependency)> dependencies, bool metadataAssembly = false)
    {
        SyntaxNode diagnosticSyntax = compilation.SyntaxTrees[0].GetRoot();
        var plans = new List<SourceCallablePlan>();
        var interfaces = new List<SourceInterfacePlan>();
        var unions = new List<SourceUnionDeclarationPlan>();
        var properties = new List<SourcePropertySymbol>();
        var storageFields = new List<IFieldSymbol>();
        var declaredTypes = new Dictionary<INamedTypeSymbol, SourceTypePlan>(SymbolEqualityComparer.Default);
        // Collect all declarations before emitting any body, so calls do not depend on file order.
        foreach (var tree in compilation.SyntaxTrees)
        {
            var model = compilation.GetSemanticModel(tree);
            var root = (CompilationUnitSyntax)tree.GetRoot();
            diagnosticSyntax = root;
            if (root.AttributeLists.Count != 0) throw Unsupported("assembly attributes");
            foreach (var member in Flatten(root.Members))
            {
                diagnosticSyntax = member;
                if (member is GlobalStatementSyntax { Statement: FunctionStatementSyntax declaration })
                {
                    diagnosticSyntax = declaration;
                    if ((declaration.Body is null && declaration.ExpressionBody is null) || declaration.AttributeLists.Count != 0 ||
                        declaration.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword)))
                        throw Unsupported("only top-level functions with block or expression bodies");
                    var symbol = model.GetDeclaredSymbol(declaration) as IMethodSymbol ?? throw Unsupported("function symbol unavailable");
                    var plan = GetPlan(symbol);
                    plans.Add(plan);
                }
                else if (member is ExtensionDeclarationSyntax extension)
                {
                    if (extension.AttributeLists.Count != 0 || extension.ConstraintClauses.Count != 0 ||
                        model.GetDeclaredSymbol(extension) is not INamedTypeSymbol extensionSymbol ||
                        !SourceTypePlan.TryCreate(extensionSymbol, out var extensionPlan, NeoClrCapabilities.Shared))
                        throw Unsupported("supported extension container contract");
                    declaredTypes.TryAdd(extensionSymbol, extensionPlan!);
                    foreach (var extensionMember in extension.Members)
                    {
                        diagnosticSyntax = extensionMember;
                        if (extensionMember is not MethodDeclarationSyntax method ||
                            (method.Body is null && method.ExpressionBody is null) || method.AttributeLists.Count != 0 ||
                            method.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword)) ||
                            model.GetDeclaredSymbol(method) is not IMethodSymbol { IsStatic: true, IsExtensionMethod: true, Parameters.IsEmpty: false } symbol)
                            throw Unsupported("implemented instance extension methods");
                        plans.Add(GetPlan(symbol));
                    }
                }
                else if (member is EnumDeclarationSyntax enumeration)
                {
                    if (enumeration.AttributeLists.Count != 0 || model.GetDeclaredSymbol(enumeration) is not INamedTypeSymbol enumSymbol ||
                        !SourceTypePlan.TryCreate(enumSymbol, out var enumPlan, NeoClrCapabilities.Shared))
                        throw Unsupported("top-level public/internal Int32 enum without attributes");
                    declaredTypes.TryAdd(enumSymbol, enumPlan!);
                }
                else if (member is UnionDeclarationSyntax unionSyntax)
                {
                    var union = model.GetDeclaredSymbol(unionSyntax) as SourceUnionSymbol
                        ?? throw Unsupported("union symbol unavailable");
                    var declarations = SourceUnionDeclarationPlan.Create(union);
                    unions.Add(declarations);
                    foreach (var unionType in declarations.Types)
                    {
                        if (!SourceTypePlan.TryCreate(unionType.Symbol, out var unionTypePlan, NeoClrCapabilities.Shared))
                            throw Unsupported("union type contract: " + unionType.Symbol.ToDisplayString());
                        declaredTypes.TryAdd(unionType.Symbol, unionTypePlan!);
                        foreach (var field in unionType.Fields)
                        {
                            if (field.IsStatic || field.IsConst || field.RefKind != RefKind.None ||
                                !CallableSignature.TryType(field.Type, false, out _, NeoClrCapabilities.Shared))
                                throw Unsupported("union field contract: " + field.ToDisplayString());
                            storageFields.Add(field);
                        }
                        foreach (var property in unionType.Properties)
                        {
                            if (property is not SourcePropertySymbol sourceProperty)
                                throw Unsupported("union property contract: " + property.ToDisplayString());
                            properties.Add(sourceProperty);
                        }
                        foreach (var method in unionType.Methods)
                        {
                            if (!SourceCallablePlan.TryCreate(method, out var unionCallable, NeoClrCapabilities.Shared, unionSyntax))
                                throw Unsupported("union callable contract: " + method.ToDisplayString());
                            plans.Add(unionCallable!);
                            if (!unionCallable!.TryLowerBody(compilation, IsConsoleCall, out _, out var unionFailure, NeoClrCapabilities.Shared))
                                throw new UnsupportedInputException("union body " + method.Name + ": " + unionFailure!.Detail, unionFailure.Syntax.GetLocation());
                        }
                    }
                    // The full declaration graph, including uncalled synthesized members,
                    // is emitted with the union attributes below.
                }
                else if (member is InterfaceDeclarationSyntax interfaceSyntax)
                {
                    if (model.GetDeclaredSymbol(interfaceSyntax) is not INamedTypeSymbol interfaceSymbol ||
                        !SourceInterfacePlan.TryCreate(interfaceSymbol, NeoClrCapabilities.Shared, out var interfacePlan))
                        throw Unsupported("only invariant owned interfaces with public abstract method contracts");
                    interfaces.Add(interfacePlan!);
                }
                else if (member is TypeDeclarationSyntax type && type is ClassDeclarationSyntax or StructDeclarationSyntax)
                {
                    if (type.AttributeLists.Count != 0 || type.ParameterList is not null ||
                        type is ClassDeclarationSyntax { PermitsClause: not null } ||
                        type.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.StaticKeyword or SyntaxKind.PartialKeyword or SyntaxKind.OpenKeyword)))
                        throw Unsupported("only public or internal static/root classes or value types without additional contracts");
                    var typeSymbol = model.GetDeclaredSymbol(type) as INamedTypeSymbol ?? throw Unsupported("type symbol unavailable");
                    if (!SourceTypePlan.TryCreate(typeSymbol, out var typePlan, NeoClrCapabilities.Shared))
                        throw Unsupported("supported public/internal static classes, root classes or unconstrained value types");
                    // Partial declarations share one semantic identity and one metadata definition.
                    // Still validate every part and collect all of its members.
                    declaredTypes.TryAdd(typeSymbol, typePlan!);
                    foreach (var typeMember in type.Members)
                    {
                        diagnosticSyntax = typeMember;
                        if (typeMember is ClassDeclarationSyntax or StructDeclarationSyntax) continue;
                        if (!typeSymbol.IsStatic && typeMember is FieldDeclarationSyntax fieldSyntax)
                        {
                            if (fieldSyntax.AttributeLists.Count != 0 ||
                                fieldSyntax.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword)))
                                throw Unsupported("only public/internal/private mutable instance fields with target-supported storage types");
                            foreach (var variable in fieldSyntax.Declaration.Declarators)
                            {
                                diagnosticSyntax = variable;
                                if (model.GetDeclaredSymbol(variable) is not IFieldSymbol { IsStatic: false, IsReadOnly: false, IsConst: false, RefKind: RefKind.None } field ||
                                    field.DeclaredAccessibility is not (Accessibility.Public or Accessibility.Internal or Accessibility.Private) ||
                                    !CallableSignature.TryType(field.Type, false, out _, NeoClrCapabilities.Shared))
                                    throw Unsupported("only public/internal/private mutable instance fields with target-supported storage types");
                                storageFields.Add(field);
                            }
                            continue;
                        }
                        if (!typeSymbol.IsStatic && typeMember is IndexerDeclarationSyntax indexerSyntax)
                        {
                            if (indexerSyntax.AttributeLists.Count != 0 || indexerSyntax.ExplicitInterfaceSpecifier is not null || indexerSyntax.Initializer is not null ||
                                indexerSyntax.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword)) ||
                                model.GetDeclaredSymbol(indexerSyntax) is not SourcePropertySymbol { IsStatic: false, IsIndexer: true } indexer ||
                                !CallableSignature.TryType(indexer.Type, false, out _, NeoClrCapabilities.Shared))
                                throw Unsupported("only implemented root-class indexers with supported value types");
                            if (indexerSyntax.AccessorList is { } indexerAccessors && indexerAccessors.Accessors.Any(a =>
                                a.Kind is not (SyntaxKind.GetAccessorDeclaration or SyntaxKind.SetAccessorDeclaration) ||
                                a.AttributeLists.Count != 0 || (a.Body is null && a.ExpressionBody is null) ||
                                a.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword))))
                                throw Unsupported("only implemented indexer get/set accessors");
                            properties.Add(indexer);
                            if (indexer.GetMethod is { } indexGet) plans.Add(GetPlan(indexGet));
                            if (indexer.SetMethod is { } indexSet) plans.Add(GetPlan(indexSet));
                            continue;
                        }
                        if (typeMember is PropertyDeclarationSyntax propertySyntax)
                        {
                            if (propertySyntax.AttributeLists.Count != 0 || propertySyntax.ExplicitInterfaceSpecifier is not null ||
                                propertySyntax.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword or SyntaxKind.StaticKeyword)) ||
                                model.GetDeclaredSymbol(propertySyntax) is not SourcePropertySymbol property ||
                                property.IsStatic && (property.BackingField is not null || propertySyntax.Initializer is not null) ||
                                !CallableSignature.TryType(property.Type, false, out _, NeoClrCapabilities.Shared))
                                throw Unsupported("only supported instance properties/storage or implemented static properties without storage");
                            if (propertySyntax.AccessorList is { } accessorList && accessorList.Accessors.Any(a =>
                                a.Kind is not (SyntaxKind.GetAccessorDeclaration or SyntaxKind.SetAccessorDeclaration) ||
                                a.AttributeLists.Count != 0 || (a.Body is null && a.ExpressionBody is null) ||
                                a.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword))))
                                throw Unsupported("only implemented get/set accessors without additional contracts");
                            if (property.BackingField is { } backingField) storageFields.Add(backingField);
                            if (property.EmitAsFieldOnly) continue;
                            properties.Add(property);
                            if (property.GetMethod is { } get) plans.Add(GetPlan(get));
                            if (property.SetMethod is { } set) plans.Add(GetPlan(set));
                            continue;
                        }
                        if (!typeSymbol.IsStatic && typeMember is ConstructorDeclarationSyntax constructor)
                        {
                            if ((constructor.Body is null && constructor.ExpressionBody is null) || constructor.AttributeLists.Count != 0 ||
                                constructor.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword)))
                                throw Unsupported("only explicit root constructors with a block or expression body");
                            plans.Add(GetPlan((IMethodSymbol)model.GetDeclaredSymbol(constructor)!));
                            continue;
                        }
                        if (typeMember is OperatorDeclarationSyntax op && (op.Body is not null || op.ExpressionBody is not null) &&
                            op.AttributeLists.Count == 0 && op.Modifiers.All(m => m.Kind is SyntaxKind.PublicKeyword or SyntaxKind.StaticKeyword))
                        {
                            plans.Add(GetPlan((IMethodSymbol)model.GetDeclaredSymbol(op)!));
                            continue;
                        }
                        if (typeMember is not MethodDeclarationSyntax method || (method.Body is null && method.ExpressionBody is null) || method.AttributeLists.Count != 0 ||
                            method.ExplicitInterfaceSpecifier is not null || method.ConstraintClauses.Count != 0 ||
                            method.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword or SyntaxKind.StaticKeyword or SyntaxKind.OverrideKeyword)))
                            throw Unsupported("only ordinary primitive methods, explicit constructors and auto-properties");
                        var symbol = model.GetDeclaredSymbol(method) as IMethodSymbol ?? throw Unsupported("method symbol unavailable");

                        var plan = GetPlan(symbol);
                        plans.Add(plan);
                    }
                }
                else throw Unsupported("only top-level functions and supported source type declarations");
            }
        }
        var primitiveOwners = new Dictionary<INamedTypeSymbol, PrimitiveType>(SymbolEqualityComparer.Default);
        var primitiveFields = new Dictionary<IFieldSymbol, PrimitiveType>(SymbolEqualityComparer.Default);
        foreach (var primitive in options.PrimitiveImplementations)
        {
            var symbol = declaredTypes.Keys.SingleOrDefault(t => t.ToFullyQualifiedMetadataName() == "System." + primitive)
                ?? throw Unsupported("selected primitive implementation is missing: " + primitive);
            var fieldsForType = storageFields.Where(f => SymbolEqualityComparer.Default.Equals(f.ContainingType, symbol)).ToArray();
            if (!symbol.IsValueType || symbol.Arity != 0 || symbol.ContainingType is not null || fieldsForType.Length != 1 ||
                fieldsForType[0] is not { Name: "m_value", DeclaredAccessibility: Accessibility.Private, IsStatic: false, IsReadOnly: false } field ||
                field.Type.SpecialType.ToString() != "System_" + primitive ||
                plans.Any(p => SymbolEqualityComparer.Default.Equals(p.Symbol.ContainingType, symbol) && p.Symbol.MethodKind == MethodKind.Constructor))
                throw Unsupported("primitive implementation requires canonical numeric storage and no explicit constructors: " + primitive);
            primitiveOwners.Add(symbol, primitive);
            primitiveFields.Add(field, primitive);
        }
        foreach (var type in declaredTypes.Values.Where(t => !t.IsStatic && !primitiveOwners.ContainsKey(t.Symbol)))
            foreach (var constructor in type.Symbol.GetMembers().OfType<IMethodSymbol>().Where(m => m.MethodKind == MethodKind.Constructor))
                if (!plans.Any(p => SymbolEqualityComparer.Default.Equals(p.Symbol, constructor)))
                {
                    if (constructor.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax() is not (ClassDeclarationSyntax or StructDeclarationSyntax)) throw Unsupported("constructor unavailable");
                    plans.Add(GetPlan(constructor));
                }
        var prepared = new List<(SourceCallablePlan Plan, LinearMethodBody Body)>();
        foreach (var plan in plans)
        {
            if (!plan.TryLowerBody(compilation, IsConsoleCall, out var body, out var failure, NeoClrCapabilities.Shared))
                throw new UnsupportedInputException(failure!.Detail, failure.Syntax.GetLocation());
            prepared.Add((plan, body!));
        }
        var closureCaptures = new Dictionary<IMethodSymbol, ILocalSymbol[]>(SymbolEqualityComparer.Default);
        var lambdaSymbols = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);
        for (var index = 0; index < prepared.Count; index++)
            foreach (var (function, syntax) in prepared[index].Body.Functions)
            {
                var symbol = (IMethodSymbol)function.Symbol!;
                if (!lambdaSymbols.Add(symbol)) continue;
                if (!CallableSignature.TryCreate(symbol, out var signature, NeoClrCapabilities.Shared)) throw Unsupported("unsupported Function body signature");
                var captures = function.CapturedVariables.Cast<ILocalSymbol>().ToArray();
                if (captures.Length != 0) closureCaptures.Add(symbol, captures);
                signature = signature with { IsInstance = captures.Length != 0 };
                if (!LinearMethodBody.TryLower(symbol, compilation.GetSemanticModel(syntax.SyntaxTree), syntax, IsConsoleCall,
                    out var body, out var failure, NeoClrCapabilities.Shared, function))
                    throw new UnsupportedInputException(failure!.Detail, failure.Syntax.GetLocation());
                var plan = new SourceCallablePlan(symbol, syntax, syntax, null, "$function$" + lambdaSymbols.Count, signature, IsSynthesizedStatic: true);
                prepared.Add((plan, body!));
            }
        // Materialize definitions only after all source declarations and body capabilities pass.
        // Every definition exists before reference resolution or method-body emission.
        var assembly = new AssemblyBuilder(options.Identity, options.CoreLibrary);
        foreach (var (_, binding) in dependencies)
            if (binding.NativeImplementation is { } implementation)
                assembly.BindNativeLibrary(binding.Definition, implementation, binding.CoreLibrary);
        var owners = new Dictionary<INamedTypeSymbol, NeoClrCallableDefinitionBuilder>(SymbolEqualityComparer.Default);
        var nativeTypes = new Dictionary<INamedTypeSymbol, TypeBuilder>(SymbolEqualityComparer.Default);
        var typeDefinitions = new NeoClrTypeDefinitionBuilder(assembly, type => nativeTypes[type]);
        var importedTypes = new Dictionary<INamedTypeSymbol, ImportedTypeReference>(SymbolEqualityComparer.Default);
        SignatureType ImportExternalType(INamedTypeSymbol type)
        {
            // The configured semantic marker is transport only; native metadata keeps Self.
            if (RuntimeSelfTypes.IsSelf(compilation, type)) return SignatureType.Self;
            var original = (INamedTypeSymbol)type.OriginalDefinition;
            if (!importedTypes.TryGetValue(original, out var imported))
            {
                var binding = dependencies.SingleOrDefault(d => SymbolEqualityComparer.Default.Equals(d.Symbol, original.ContainingAssembly)).Dependency
                    ?? throw Unsupported("unregistered dependency type: " + original.ToDisplayString());
                if (original.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: { } artifact } &&
                    IsSymbolOnlyOwnerDefinition(original))
                {
                    if (artifact.Sha256 != binding.NativeArtifactSha256)
                        throw Unsupported("native dependency snapshot differs from semantic reference");
                    var identity = new AssemblyIdentity(artifact.Name, artifact.Version, artifact.Culture, artifact.PublicKeyToken, artifact.Flags);
                    var physicalParent = original is IUnionCaseTypeSymbol unionCase ? unionCase.MetadataContainingType : original.ContainingType;
                    if (physicalParent is not null)
                    {
                        _ = ImportExternalType(physicalParent);
                        imported = assembly.CreateNestedTypeReference(importedTypes[(INamedTypeSymbol)physicalParent.OriginalDefinition],
                            original.MetadataName, original.Arity, original.IsValueType);
                    }
                    else imported = original.TypeKind == TypeKind.Interface
                        ? assembly.CreateInterfaceReference(identity, binding.CoreLibrary, artifact.Sha256,
                            original.ContainingNamespace?.ToMetadataName() ?? "", original.MetadataName, original.Arity)
                        : original.TypeKind == TypeKind.Enum
                            ? assembly.CreateEnumReference(identity, binding.CoreLibrary, artifact.Sha256,
                                original.ContainingNamespace?.ToMetadataName() ?? "", original.MetadataName)
                        : original.IsValueType
                            ? assembly.CreateValueTypeReference(identity, binding.CoreLibrary, artifact.Sha256,
                                original.ContainingNamespace?.ToMetadataName() ?? "", original.MetadataName, original.Arity)
                            : assembly.CreateTypeReference(identity, binding.CoreLibrary, artifact.Sha256,
                                original.ContainingNamespace?.ToMetadataName() ?? "", original.MetadataName, original.Arity);
                }
                else
                {
                    if (original.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null })
                        throw Unsupported("native type requires a supported symbol-only emission contract");
                    var name = original.ToFullyQualifiedMetadataName();
                    var candidates = binding.Definition.MainModule.Types.Where(t => MatchesType(t, original)).Take(2).ToArray();
                    if (candidates.Length != 1 || candidates[0].GenericArity != original.Arity || candidates[0].IsValueType != original.IsValueType)
                        throw Unsupported("dependency type unavailable or ambiguous: " + name);
                    imported = assembly.ImportReference(candidates[0], binding.CoreLibrary);
                }
                importedTypes.Add(original, imported);
                if (IsSymbolOnlyReferenceDefinition(original))
                    foreach (var contract in original.Interfaces)
                    {
                        var target = ImportExternalType(contract).ImportedType!;
                        assembly.AddInterfaceConversion(imported, target);
                    }
                if (original.TypeKind == TypeKind.Interface && original.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null })
                {
                    // Seal a complete semantic contract, including accessors even when no body calls them.
                    var members = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);
                    foreach (var member in original.GetMembers())
                    {
                        if (member is IMethodSymbol method) members.Add(method);
                        else if (member is IPropertySymbol property)
                        {
                            if (property.GetMethod is { } getter) members.Add(getter);
                            if (property.SetMethod is { } setter) members.Add(setter);
                        }
                        else throw Unsupported("external interface member category");
                    }
                    foreach (var method in members) _ = Import(method);
                    assembly.CompleteInterfaceReference(imported);
                }
            }
            return imported.GenericArity == 0 ? imported : imported.MakeGenericInstance(type.TypeArguments
                .Select(t => NeoClrTypeMapper.Map(t, owned => nativeTypes[owned], ImportExternalType)).ToArray());
        }
        var functions = new NeoClrCallableDefinitionBuilder(assembly, resolveClass: type => nativeTypes[type], resolveExternal: ImportExternalType);
        foreach (var type in declaredTypes.Values)
        {
            var definition = type.Define(typeDefinitions);
            if (primitiveOwners.TryGetValue(type.Symbol, out var primitive)) definition.SetNativePrimitive(primitive);
            nativeTypes.Add(type.Symbol, definition);
            owners.Add(type.Symbol, new(assembly, definition, type => nativeTypes[type], ImportExternalType));
        }
        var extensionOwners = declaredTypes.Values.Where(t =>
            t.Symbol.OriginalDefinition is SourceNamedTypeSymbol { IsExtensionDeclaration: true }).ToArray();
        if (extensionOwners.Length != 0)
        {
            // Use the same bounded embedded marker representation as native union metadata.
            var marker = assembly.AddClass("System.Runtime.CompilerServices", "ExtensionAttribute", TypeVisibility.Internal)
                .AddConstructor(new MethodSignature(PrimitiveType.Void, []));
            marker.GetILGenerator().Return();
            foreach (var extension in extensionOwners)
                nativeTypes[extension.Symbol].AddCustomAttribute(new(marker.Definition, []));
        }
        if (unions.Count != 0)
            NeoClrUnionMetadata.Emit(assembly, unions, type => nativeTypes[type]);
        var nativeInterfaces = new Dictionary<INamedTypeSymbol, TypeBuilder>(SymbolEqualityComparer.Default);
        foreach (var contract in interfaces)
        {
            var symbol = contract.Symbol;
            var visibility = symbol.DeclaredAccessibility == Accessibility.Public ? TypeVisibility.Public : TypeVisibility.Internal;
            nativeInterfaces.Add(symbol, symbol.Arity == 0 ? assembly.AddInterface(contract.Namespace, contract.Name, visibility)
                : assembly.AddGenericInterface(contract.Namespace, symbol.Name, symbol.TypeParameters.Select(p => p.Name), visibility));
        }
        foreach (var pair in nativeInterfaces) nativeTypes.Add(pair.Key, pair.Value);
        var interfaceMethods = new Dictionary<IMethodSymbol, MetadataMethod>(SymbolEqualityComparer.Default);
        foreach (var contract in interfaces)
        {
            var definition = nativeInterfaces[contract.Symbol];
            foreach (var inherited in contract.BaseInterfaces)
            {
                if (!nativeInterfaces.TryGetValue((INamedTypeSymbol)inherited.OriginalDefinition, out var parent))
                {
                    definition.AddBaseInterface(ImportExternalType(inherited).ImportedType!);
                    continue;
                }
                if (inherited.Arity == 0) definition.AddBaseInterface(parent);
                else definition.AddBaseInterface(parent.MakeGenericInstance(inherited.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType)).ToArray()));
            }
            var contractMethods = new Dictionary<IMethodSymbol, MetadataMethod>(SymbolEqualityComparer.Default);
            foreach (var method in contract.Methods)
                contractMethods.Add(method.Symbol, definition.AddInterfaceMethod(method.Symbol.MetadataName, new MethodSignature(
                    NeoClrTypeMapper.Map(method.Signature.ReturnType, type => nativeTypes[type], ImportExternalType),
                    method.Signature.ParameterTypes.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType)), outParameters: method.Signature.OutParameters.IsDefault ? [] : method.Signature.OutParameters), method.Symbol.IsStatic));
            foreach (var pair in contractMethods) interfaceMethods.Add(pair.Key, pair.Value);
            foreach (var property in contract.Properties)
                definition.AddProperty(property.Symbol.MetadataName, NeoClrTypeMapper.Map(property.Type, type => nativeTypes[type], ImportExternalType),
                    property.Symbol.GetMethod is { } get ? contractMethods[get] : null,
                    property.Symbol.SetMethod is { } set ? contractMethods[set] : null);
        }
        foreach (var type in declaredTypes.Values)
            foreach (var contract in type.Symbol.Interfaces)
            {
                if (!nativeInterfaces.TryGetValue((INamedTypeSymbol)contract.OriginalDefinition, out var definition))
                {
                    nativeTypes[type.Symbol].AddInterfaceImplementation(ImportExternalType(contract).ImportedType!);
                    continue;
                }
                if (contract.Arity == 0) nativeTypes[type.Symbol].AddInterfaceImplementation(definition);
                else nativeTypes[type.Symbol].AddInterfaceImplementation(definition.MakeGenericInstance(
                    contract.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, owner => nativeTypes[owner], ImportExternalType)).ToArray()));
            }
        foreach (var type in declaredTypes.Values.Where(t => !t.IsExtensionContainer))
            foreach (var parameter in type.Symbol.TypeParameters)
            {
                foreach (var bound in parameter.ConstraintTypes)
                    nativeTypes[type.Symbol].AddBaseTypeConstraint(parameter.Ordinal, nativeTypes[(INamedTypeSymbol)bound]);
                var flags = TypeParameterConstraints.None;
                if (parameter.ConstraintKind.HasFlag(TypeParameterConstraintKind.ReferenceType)) flags |= TypeParameterConstraints.ReferenceType;
                if (parameter.ConstraintKind.HasFlag(TypeParameterConstraintKind.ValueType)) flags |= TypeParameterConstraints.ValueType | TypeParameterConstraints.DefaultConstructor;
                if (parameter.ConstraintKind.HasFlag(TypeParameterConstraintKind.Constructor)) flags |= TypeParameterConstraints.DefaultConstructor;
                nativeTypes[type.Symbol].SetSpecialConstraints(parameter.Ordinal, flags);
            }
        var fields = new Dictionary<IFieldSymbol, FieldBuilder>(SymbolEqualityComparer.Default);
        var importedFields = new Dictionary<IFieldSymbol, NeoClrFieldReference>(SymbolEqualityComparer.Default);
        foreach (var field in storageFields)
        {
            if (primitiveFields.ContainsKey(field)) continue;
            CallableSignature.TryType(field.Type, false, out var fieldType, NeoClrCapabilities.Shared);
            var storageType = NeoClrTypeMapper.Map(fieldType, type => nativeTypes[type], ImportExternalType);
            fields.Add(field, nativeTypes[field.ContainingType!].AddField(field.MetadataName, storageType, field.DeclaredAccessibility switch
            {
                Accessibility.Public => FieldVisibility.Public,
                Accessibility.Internal => FieldVisibility.Internal,
                Accessibility.Private => FieldVisibility.Private,
                _ => throw Unsupported("unsupported field visibility")
            }, isReadOnly: field.IsReadOnly));
        }
        if (compilation.Options.RuntimeIterationContract is { ArrayShapeTypeName: { } arrayName } arrayContract &&
            arrayContract.AssemblyName == options.Identity.Name)
        {
            var arrayShape = declaredTypes.Keys.SingleOrDefault(t => t.ToFullyQualifiedMetadataName() == arrayName);
            if (arrayShape is null || !nativeTypes.TryGetValue(arrayShape, out var arrayDefinition))
                throw Unsupported("configured array backing declaration is absent from output");
            assembly.SetArrayBacking(arrayDefinition);
        }
        var closureFields = new Dictionary<IMethodSymbol, FieldBuilder[]>(SymbolEqualityComparer.Default);
        var closureConstructors = new Dictionary<IMethodSymbol, MetadataMethod>(SymbolEqualityComparer.Default);
        var methods = new List<(SourceCallablePlan Plan, MetadataMethod Method, LinearMethodBody Body)>();
        foreach (var (plan, body) in prepared)
        {
            diagnosticSyntax = plan.Syntax;
            MetadataMethod definition;
            if (closureCaptures.TryGetValue(plan.Symbol, out var captures))
            {
                var frame = assembly.AddClass("", "$closure$" + closureFields.Count, TypeVisibility.Internal);
                var captureFields = captures.Select((capture, index) => frame.AddField("capture" + index,
                    NeoClrTypeMapper.Map(capture.Type, type => nativeTypes[type], ImportExternalType), FieldVisibility.Private)).ToArray();
                var constructor = frame.AddConstructor(new MethodSignature(PrimitiveType.Void, captureFields.Select(field => field.FieldType)));
                var constructorIl = constructor.GetILGenerator();
                for (int i = 0; i < captureFields.Length; i++)
                {
                    constructorIl.LoadArgument(0);
                    constructorIl.LoadArgument(i + 1);
                    constructorIl.StoreField(captureFields[i]);
                }
                constructorIl.Return();
                var signature = plan.Signature;
                definition = frame.AddInstanceMethod("Invoke", new MethodSignature(
                    NeoClrTypeMapper.Map(signature.ReturnType, type => nativeTypes[type], ImportExternalType),
                    signature.ParameterTypes.Select(type => NeoClrTypeMapper.Map(type, owner => nativeTypes[owner], ImportExternalType))));
                closureFields.Add(plan.Symbol, captureFields);
                closureConstructors.Add(plan.Symbol, constructor);
            }
            else
            {
                var owner = plan.IsAssemblyFunction ? functions : owners[plan.TypeOwner!];
                definition = plan.Define(owner);
            }
            for (var i = 0; i < plan.Symbol.Parameters.Length; i++)
                definition.SetParameterName(i, plan.Symbol.Parameters[i].Name);
            methods.Add((plan, definition, body));
        }
        var definedMethods = methods.ToDictionary(m => m.Plan.Symbol, m => m.Method, (IEqualityComparer<IMethodSymbol>)SymbolEqualityComparer.Default);
        foreach (var property in properties)
        {
            CallableSignature.TryType(property.Type, false, out var propertyType, NeoClrCapabilities.Shared);
            var valueType = NeoClrTypeMapper.Map(propertyType, type => nativeTypes[type], ImportExternalType);
            nativeTypes[property.ContainingType!].AddProperty(property.MetadataName, valueType,
                property.GetMethod is null ? null : definedMethods[property.GetMethod], property.SetMethod is null ? null : definedMethods[property.SetMethod]);
        }
        var references = new CallableReferenceTable<NeoClrCallableReference>(target =>
        {
            if (target.ContainingType is { Arity: > 0 } owner &&
                owner.OriginalDefinition is not SourceNamedTypeSymbol { IsExtensionDeclaration: true })
            {
                if (!definedMethods.TryGetValue(target.OriginalDefinition ?? target, out var definition))
                    return NeoClrCallableReference.Create(Import(target.OriginalDefinition ?? target).MakeConstructedReference(
                        owner.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType)),
                        target.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType))));
                return NeoClrCallableReference.Create(definition.MakeConstructedReference(
                    owner.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType)),
                    target.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType))));
            }
            if (target.IsGenericMethod)
            {
                if (definedMethods.TryGetValue(target.OriginalDefinition ?? target, out var definition))
                    return NeoClrCallableReference.Create(definition.MakeGenericInstance(target.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType)).ToArray()));
                var arguments = target.TypeArguments.Select(t =>
                    NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType)).ToArray();
                return NeoClrCallableReference.Create(Import(target.OriginalDefinition ?? target).MakeGenericInstance(arguments));
            }
            // Nested member lookup may expose a substituted method even under a
            // nongeneric owner. Reuse its declaration identity before resolving imports.
            if (definedMethods.TryGetValue(target.OriginalDefinition ?? target, out var declared))
                return NeoClrCallableReference.Create(declared);
            var systemFunction = ImportSystem(target);
            return systemFunction is not null
                ? NeoClrCallableReference.Create(systemFunction)
                : NeoClrCallableReference.Create(Import(target));
        });
        foreach (var declaration in methods)
            if (!declaration.Plan.Symbol.IsGenericMethod && declaration.Plan.Symbol.ContainingType?.Arity is not > 0)
                references.Declare(declaration.Plan.Symbol, NeoClrCallableReference.Create(declaration.Method));
        if (compilation.Options.OutputKind == OutputKind.ConsoleApplication)
        {
            var entry = compilation.GetEntryPoint() ?? throw Unsupported("entry point unavailable");
            assembly.EntryPoint = methods.SingleOrDefault(m => SymbolEqualityComparer.Default.Equals(m.Plan.Symbol, entry)).Method
                ?? throw Unsupported("entry must be a declared Int32/Unit function or static method");
        }
        var unitAdapters = new Dictionary<int, (MethodBuilder Constructor, MethodBuilder Invoke)>();
        (MethodBuilder Constructor, MethodBuilder Invoke) UnitAdapter(int arity, SignatureType unit)
        {
            if (unitAdapters.TryGetValue(arity, out var existing)) return existing;
            var owner = arity == 0 ? assembly.AddClass("Raven.Generated", "UnitCallback", TypeVisibility.Internal)
                : assembly.AddGenericClass("Raven.Generated", "UnitCallback", Enumerable.Range(0, arity).Select(i => "Argument" + i), TypeVisibility.Internal);
            var parameters = Enumerable.Range(0, arity).Select(SignatureType.TypeParameter).ToArray();
            var input = SignatureType.Function(new MethodSignature(PrimitiveType.Void, parameters));
            var callback = owner.AddField("callback", input, FieldVisibility.Private);
            var constructor = owner.AddConstructor(new MethodSignature(PrimitiveType.Void, [input]));
            var body = constructor.GetILGenerator(); body.LoadArgument(0); body.LoadArgument(1); body.StoreField(callback); body.Return();
            var invoke = owner.AddInstanceMethod("Invoke", new MethodSignature(unit, parameters));
            body = invoke.GetILGenerator(); body.LoadArgument(0); body.LoadField(callback);
            for (int i = 0; i < arity; i++) body.LoadArgument(i + 1);
            body.InvokeFunction(input); body.LoadDefault(unit); body.Return();
            return unitAdapters[arity] = (constructor, invoke);
        }
        foreach (var current in methods)
        {
            diagnosticSyntax = current.Plan.Syntax;
            current.Body.Emit(new NeoClrLinearMethodBuilder(current.Method.GetILGenerator(), (instruction, output) =>
            {
                diagnosticSyntax = instruction.Syntax;
                if (instruction.Kind == LinearInstructionKind.FunctionBind)
                {
                    var symbol = instruction.Method!;
                    if (!definedMethods.TryGetValue(symbol.OriginalDefinition, out var target) && !interfaceMethods.TryGetValue(symbol.OriginalDefinition, out target))
                        throw Unsupported("Function binding requires an owned target");
                    if (closureConstructors.TryGetValue(symbol, out var constructor)) output.NewObject(constructor);
                    var shape = NeoClrTypeMapper.Map(instruction.Type!, type => nativeTypes[type], ImportExternalType);
                    void Bind(SignatureType bindingShape)
                    {
                        if (symbol.ContainingType is { Arity: > 0 } owner)
                            output.BindFunction(bindingShape, target.MakeConstructedReference(owner.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType))));
                        else output.BindFunction(bindingShape, target);
                    }
                    if (target.Signature.ReturnType.Primitive == PrimitiveType.Void && !shape.FunctionSignature!.NoResult)
                    {
                        var unit = NeoClrTypeMapper.Map(compilation.UnitTypeSymbol, type => nativeTypes[type], ImportExternalType);
                        if (shape.FunctionSignature.ReturnType != unit) throw Unsupported("callback result requires an explicit conversion");
                        var parameters = shape.FunctionSignature.ParameterTypes;
                        Bind(SignatureType.Function(new MethodSignature(PrimitiveType.Void, parameters)));
                        var adapter = UnitAdapter(parameters.Count, unit);
                        if (parameters.Count == 0) { output.NewObject(adapter.Constructor); output.BindFunction(shape, adapter.Invoke); }
                        else { output.NewObject(adapter.Constructor.MakeConstructedReference(parameters)); output.BindFunction(shape, adapter.Invoke.MakeConstructedReference(parameters)); }
                    }
                    else Bind(shape);
                }
                else if (instruction.Kind == LinearInstructionKind.LoadCapture)
                {
                    output.LoadArgument(0);
                    output.LoadField(closureFields[current.Plan.Symbol][instruction.Integer]);
                }
                else if (instruction.Kind == LinearInstructionKind.InterfaceCall && interfaceMethods.TryGetValue(instruction.Method!.OriginalDefinition ?? instruction.Method, out var contract))
                {
                    if (instruction.Method.ContainingType is { Arity: > 0 } owner)
                        output.CallVirtual(contract.MakeConstructedReference(owner.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType))));
                    else output.CallVirtual(contract);
                }
                else if (instruction.Kind == LinearInstructionKind.Call && IsCheckedReservation(instruction.Method!))
                    output.ReserveArray(NeoClrTypeMapper.Map(instruction.Method!.TypeArguments[0], type => nativeTypes[type], ImportExternalType));
                else references.Resolve(instruction.Method!).EmitCall(output);
            }, field =>
            {
                if (primitiveFields.TryGetValue(field, out var primitive)) return new NeoClrFieldReference(null, IntrinsicStorage: primitive);
                if (fields.TryGetValue(field, out var definition)) return new NeoClrFieldReference(definition);
                if (field is SubstitutedFieldSymbol substituted && field.ContainingType is { Arity: > 0 } owner && fields.TryGetValue(substituted.OriginalField, out definition))
                    return new NeoClrFieldReference(definition, definition.MakeConstructedReference(owner.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType)).ToArray()));
                var originalField = field is SubstitutedFieldSymbol importedSubstitution ? importedSubstitution.OriginalField : field;
                if (originalField is IInstanceFieldLayoutSymbol layout && originalField.ContainingType is { } declaring &&
                    IsSymbolOnlyReferenceDefinition(declaring) && originalField.DeclaredAccessibility == Accessibility.Public &&
                    !originalField.IsStatic && IsSymbolOnlyType(originalField.Type, false))
                {
                    if (importedFields.TryGetValue(field, out var cached)) return cached;
                    _ = ImportExternalType(declaring);
                    var reference = assembly.CreateFieldReference(importedTypes[declaring], originalField.MetadataName,
                        MapSymbolOnlyType(originalField.Type), layout.InstanceStorageOrdinal, originalField.IsReadOnly);
                    cached = declaring.Arity == 0 ? new NeoClrFieldReference(null, Import: reference)
                        : new NeoClrFieldReference(null, ImportedConstruction: reference.MakeConstructedReference(
                            field.ContainingType!.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType)).ToArray()));
                    importedFields.Add(field, cached);
                    return cached;
                }
                if (field.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null })
                    throw Unsupported("native field requires a supported symbol-only emission contract and layout");
                throw Unsupported("undeclared instance field");
            },
                type => nativeTypes.TryGetValue(type, out var definition) ? definition : throw Unsupported("undeclared class local: " + type.ToDisplayString() + " (" + type.GetType().Name + ")"), ImportExternalType));
        }
        return metadataAssembly
            ? compilation.Options.OutputKind == OutputKind.DynamicallyLinkedLibrary
                ? NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer.WriteLibraryBinary(assembly)
                : NeoCLR.Metadata.Experimental.RuntimeAssemblyContainer.WriteBinary(assembly)
            : assembly.WriteNativeAssembly();

        bool IsCheckedReservation(IMethodSymbol method)
        {
            if (options.BootstrapReference is null ||
                !SymbolEqualityComparer.Default.Equals(method.ContainingAssembly, compilation.GetAssemblyOrModuleSymbol(options.BootstrapReference)) ||
                method.ContainingType?.ToFullyQualifiedMetadataName() != "System.Runtime.CompilerServices.CheckedStorage" || method.Name != "Reserve")
                return false;
            var definition = method.OriginalDefinition ?? method;
            if (definition.ContainingType?.IsStatic != true || !definition.IsStatic || definition.IsVirtual || definition.DeclaredAccessibility != Accessibility.Public ||
                definition.TypeParameters.Length != 1 || method.TypeArguments.Length != 1 ||
                definition.TypeParameters[0].ConstraintKind != TypeParameterConstraintKind.None || !definition.TypeParameters[0].ConstraintTypes.IsEmpty ||
                definition.Parameters.Length != 1 || definition.Parameters[0].RefKind != RefKind.None || definition.Parameters[0].Type.SpecialType != SpecialType.System_Int32 ||
                definition.ReturnType is not IArrayTypeSymbol { Rank: 1, IsFixedArray: false } array ||
                !SymbolEqualityComparer.Default.Equals(array.ElementType, definition.TypeParameters[0]))
                throw Unsupported("invalid checked-storage reservation contract");
            return true;
        }

        static IEnumerable<MemberDeclarationSyntax> Flatten(IEnumerable<MemberDeclarationSyntax> members)
        {
            foreach (var member in members)
            {
                if (member is BaseNamespaceDeclarationSyntax ns)
                {
                    foreach (var child in Flatten(ns.Members)) yield return child;
                }
                else
                {
                    yield return member;
                    if (member is TypeDeclarationSyntax type && type is ClassDeclarationSyntax or StructDeclarationSyntax)
                        foreach (var nested in Flatten(type.Members.Where(m => m is ClassDeclarationSyntax or StructDeclarationSyntax)))
                            yield return nested;
                }
            }
        }

        bool IsConsoleCall(BoundInvocationExpression call)
        {
            if (options.ConsoleReference is null) return false;
            var method = call.Method;
            if (call.Receiver is not (null or BoundTypeExpression) || !method.IsStatic || method.IsGenericMethod || method.Name != "WriteLine" ||
                method.ContainingType?.ToFullyQualifiedMetadataName() != "System.Console" ||
                !SymbolEqualityComparer.Default.Equals(method.ContainingAssembly, compilation.GetAssemblyOrModuleSymbol(options.ConsoleReference)) ||
                method.Parameters.Length != 1 || method.Parameters[0].Type.GetNonNullableType().SpecialType != SpecialType.System_String ||
                method.Parameters[0].RefKind != RefKind.None || method.ReturnType.SpecialType is not (SpecialType.System_Void or SpecialType.System_Unit))
                return false;
            return true;
        }

        NativeFunctionDefinition? ImportSystem(IMethodSymbol symbol)
        {
            if (options.SystemSymbols is not { } system ||
                !SymbolEqualityComparer.Default.Equals(symbol.ContainingAssembly, compilation.GetAssemblyOrModuleSymbol(system.Reference))) return null;
            var name = symbol.ContainingType?.ToFullyQualifiedMetadataName() + "." + symbol.MetadataName;
            var matches = system.Functions.Where(f => f.Name == name && f.TryGetStaticInt32Signature(out var count) &&
                count == symbol.Parameters.Length && ReturnsValue(symbol) && symbol.Parameters.All(p => p.Type.SpecialType == SpecialType.System_Int32)).Take(2).ToArray();
            if (matches.Length != 1) throw Unsupported("System callable absent from explicit native selection");
            return matches[0];
        }
        static bool MatchesType(NeoCLR.Metadata.Experimental.Model.TypeDefinition definition, INamedTypeSymbol symbol)
        {
            var original = (INamedTypeSymbol)symbol.OriginalDefinition;
            var parent = original is PEUnionCaseSymbol unionCase ? unionCase.MetadataContainingType : original.ContainingType;
            return definition.Name == original.MetadataName && (parent is null
                ? definition.DeclaringType is null && (definition.Namespace.Length == 0 ? definition.Name : definition.Namespace + "." + definition.Name) == original.ToFullyQualifiedMetadataName()
                : definition.DeclaringType is { } declaring && MatchesType(declaring, parent));
        }
        ImportedMethodReference Import(IMethodSymbol symbol)
        {
            var binding = dependencies.SingleOrDefault(d => SymbolEqualityComparer.Default.Equals(d.Symbol, symbol.ContainingAssembly)).Dependency
                ?? throw Unsupported("unregistered dependency: " + symbol.ContainingAssembly?.Name + " for " + symbol.ContainingType?.ToDisplayString() + "." + symbol.ToDisplayString());
            // This bounded profile uses only compiler symbols and host artifact values.
            // Nominal identities use the same symbol-only root-class authoring path.
            if (symbol.IsStatic && symbol.ContainingType is null &&
                symbol.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: { } artifact } &&
                CallableSignature.TryCreate(symbol, out var callable, NeoClrCapabilities.Shared) &&
                IsSymbolOnlyType(symbol.ReturnType, true) && symbol.Parameters.All(p => p.RefKind == RefKind.None && IsSymbolOnlyType(p.Type, false)))
            {
                if (artifact.Sha256 != binding.NativeArtifactSha256)
                    throw Unsupported("native dependency snapshot differs from semantic reference");
                var identity = new AssemblyIdentity(artifact.Name, artifact.Version, artifact.Culture, artifact.PublicKeyToken, artifact.Flags);
                var contract = new MethodSignature(MapSymbolOnlyType(symbol.ReturnType, result: true),
                    symbol.Parameters.Select(p => MapSymbolOnlyType(p.Type)), callable.GenericParameterNames);
                return assembly.CreateFunctionReference(identity, binding.CoreLibrary, artifact.Sha256,
                    symbol.ContainingNamespace?.ToMetadataName() ?? "", symbol.MetadataName, contract);
            }
            if (symbol.ContainingType is { } owner && IsSymbolOnlyOwnerDefinition((INamedTypeSymbol)owner.OriginalDefinition) &&
                (!symbol.IsOverride || owner.IsValueType) && (owner.TypeKind == TypeKind.Interface ? symbol.IsAbstract && symbol.IsVirtual : !symbol.IsAbstract && (!symbol.IsVirtual || owner.IsValueType)) &&
                symbol.DeclaredAccessibility == Accessibility.Public && (symbol.IsStatic || symbol.Arity == 0) &&
                CallableSignature.TryCreate(symbol, out var memberSignature, NeoClrCapabilities.Shared) &&
                IsSymbolOnlyType(symbol.ReturnType, true) && symbol.Parameters.All(p => p.RefKind is RefKind.None or RefKind.Ref or RefKind.Out && IsSymbolOnlyType(p.Type, false)))
            {
                _ = ImportExternalType(owner);
                var declaration = importedTypes[(INamedTypeSymbol)owner.OriginalDefinition];
                var contract = new MethodSignature(MapSymbolOnlyType(symbol.ReturnType, result: true),
                    symbol.Parameters.Select(p => p.RefKind == RefKind.None ? MapSymbolOnlyType(p.Type) : SignatureType.ByReference(MapSymbolOnlyType(p.Type))),
                    memberSignature.GenericParameterNames, memberSignature.OutParameters.IsDefault ? [] : memberSignature.OutParameters);
                return assembly.CreateMethodReference(declaration, symbol.MetadataName, contract, symbol.IsStatic, isOverride: symbol.IsOverride, nativePrimitive: owner.SpecialType is not SpecialType.None && owner.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null }
                    ? NeoClrTypeMapper.Instance.Map(Enum.Parse<EmissionPrimitiveType>(owner.SpecialType.ToString()[7..])) : null);
            }
            // A native callable must carry a complete supported semantic contract.
            // Do not recover missing emission facts by reopening its reader definition.
            if (symbol.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null })
                throw Unsupported("native callable requires a supported symbol-only emission contract: " + symbol.ToDisplayString());
            var dependencyMetadata = binding.Definition;
            var types = dependencyMetadata.MainModule.Types.Where(t => symbol.ContainingType is { } owner && t.GenericArity == owner.Arity && MatchesType(t, owner)).Take(2).ToArray();
            if (types.Length != 1) throw Unsupported("dependency type unavailable or ambiguous");
            if (!CallableSignature.TryCreate(symbol, out var signature, NeoClrCapabilities.Shared))
                throw Unsupported("unsupported dependency method signature");
            var expected = new MethodSignature(
                NeoClrTypeMapper.Map(signature.ReturnType, type => nativeTypes[type], ImportExternalType),
                signature.ParameterTypes.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType)), signature.GenericParameterNames, signature.OutParameters.IsDefault ? [] : signature.OutParameters);
            var matches = new List<ImportedMethodReference>();
            var importFailures = new List<string>();
            foreach (var candidate in types[0].Methods.Where(m => m.Name == symbol.MetadataName && m.GenericArity == symbol.Arity && m.IsStatic == symbol.IsStatic))
            {
                ImportedMethodReference imported;
                try { imported = assembly.ImportReference(candidate, binding.CoreLibrary); }
                catch (InvalidDataException error) { if (importFailures.Count < 2) importFailures.Add(error.Message); continue; }
                if (imported.IsInterfaceMethod != (symbol.ContainingType?.TypeKind == TypeKind.Interface) ||
                    imported.RequiresVirtualDispatch != (!symbol.IsStatic && symbol.ContainingType?.IsValueType != true && symbol.IsVirtual) ||
                    imported.RequiresManagedReceiver != (!symbol.IsStatic && symbol.ContainingType?.IsValueType == true)) continue;
                var actual = imported.Signature;
                if (actual.GenericParameterNames.Count == expected.GenericParameterNames.Count && actual.ReturnType == expected.ReturnType &&
                    actual.ParameterTypes.SequenceEqual(expected.ParameterTypes) && actual.OutParameters.SequenceEqual(expected.OutParameters)) matches.Add(imported);
                if (matches.Count == 2) break;
            }
            if (matches.Count != 1) throw Unsupported("dependency method contract unavailable or ambiguous: " + symbol.ToDisplayString() + (importFailures.Count == 0 ? "" : " (" + string.Join("; ", importFailures) + ")"));
            return matches[0];
        }
        // Static containers may own references, but are never signature value types.
        static bool IsSymbolOnlyOwnerDefinition(INamedTypeSymbol original) =>
            IsSymbolOnlyReferenceDefinition(original) ||
            original.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null } &&
            original.TypeKind == TypeKind.Class && original.IsStatic && original.ContainingType is null &&
            original.DeclaredAccessibility == Accessibility.Public && original.Interfaces.IsEmpty &&
            original.TypeParameters.All(p => p.ConstraintKind == TypeParameterConstraintKind.None && p.ConstraintTypes.IsEmpty && p.Variance == VarianceKind.None);

        static bool IsSymbolOnlyReferenceDefinition(INamedTypeSymbol original, int depth = 0) =>
            depth < 32 &&
            original.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null } &&
            (original.TypeKind is TypeKind.Class or TypeKind.Interface or TypeKind.Struct or TypeKind.Enum) &&
            !original.IsStatic && (original.ContainingType is null ||
                (original is IUnionCaseTypeSymbol unionCase ? unionCase.MetadataContainingType : original.ContainingType) is { Arity: 0 } parent && IsSymbolOnlyOwnerDefinition(parent)) && original.DeclaredAccessibility == Accessibility.Public &&
            original.Interfaces.All(contract => IsSymbolOnlyReferenceDefinition((INamedTypeSymbol)contract.OriginalDefinition, depth + 1)) &&
            (original.BaseType is null || original.BaseType.SpecialType == (original.TypeKind == TypeKind.Enum ? SpecialType.System_Enum : original.IsValueType ? SpecialType.System_ValueType : SpecialType.System_Object)) &&
            original.TypeParameters.All(p => p.ConstraintKind == TypeParameterConstraintKind.None && p.ConstraintTypes.IsEmpty && p.Variance == VarianceKind.None);

        bool IsRuntimeUnitValue(ITypeSymbol type) => compilation.Options.RuntimeUnitContract is not null &&
            SymbolEqualityComparer.Default.Equals(type, compilation.GetSpecialType(SpecialType.System_Unit));

        // The selected core's erased carrier is independent of System.Object.
        // Same-named declarations from other assemblies remain ordinary nominal types.
        bool IsRuntimeErasedValue(ITypeSymbol type) =>
            type is INamedTypeSymbol { Name: "Value", TypeKind: TypeKind.Struct, Arity: 0, ContainingType: null } named &&
            named.ContainingNamespace?.ToMetadataName() == "System" &&
            SymbolEqualityComparer.Default.Equals(named.ContainingAssembly, compilation.GetSpecialType(SpecialType.System_Object).ContainingAssembly);

        bool IsSymbolOnlyType(ITypeSymbol type, bool result) =>
            RuntimeSelfTypes.IsSelf(compilation, type) ||
            type is INamedTypeSymbol { TypeKind: TypeKind.Delegate } functionType && CallableSignature.TryFunction(functionType, out _, NeoClrCapabilities.Shared) ||
            !result && IsRuntimeUnitValue(type) || IsRuntimeErasedValue(type) ||
            type is ITypeParameterSymbol ||
            type is IArrayTypeSymbol { Rank: 1, FixedLength: null, ElementType: not IArrayTypeSymbol } vector && IsSymbolOnlyType(vector.ElementType, false) ||
            type.SpecialType is SpecialType.System_SByte or SpecialType.System_Int16 or SpecialType.System_UInt16 or SpecialType.System_UInt32 or SpecialType.System_UInt64 or SpecialType.System_Byte or SpecialType.System_Int32 or SpecialType.System_Int64 or SpecialType.System_Single or SpecialType.System_Double or SpecialType.System_Boolean or SpecialType.System_String or SpecialType.System_Char ||
            result && type.SpecialType is SpecialType.System_Unit or SpecialType.System_Void ||
            type is INamedTypeSymbol named && IsSymbolOnlyReferenceDefinition((INamedTypeSymbol)named.OriginalDefinition) &&
            named.TypeArguments.All(argument => IsSymbolOnlyType(argument, false));

        SignatureType MapSymbolOnlyType(ITypeSymbol type, bool result = false) => type switch
        {
            _ when RuntimeSelfTypes.IsSelf(compilation, type) => SignatureType.Self,
            INamedTypeSymbol { TypeKind: TypeKind.Delegate } functionType when CallableSignature.TryFunction(functionType, out var shape, NeoClrCapabilities.Shared) =>
                SignatureType.Function(new MethodSignature(
                    NeoClrTypeMapper.Map(shape.ReturnType, owned => nativeTypes[owned], ImportExternalType),
                    shape.ParameterTypes.Select(t => NeoClrTypeMapper.Map(t, owned => nativeTypes[owned], ImportExternalType)))),
            INamedTypeSymbol when !result && IsRuntimeUnitValue(type) =>
                ImportExternalType(((UnitTypeSymbol)compilation.GetSpecialType(SpecialType.System_Unit)).RuntimeRepresentation!),
            ITypeParameterSymbol { DeclaringMethodParameterOwner: not null } parameter => SignatureType.MethodParameter(parameter.Ordinal),
            ITypeParameterSymbol parameter => SignatureType.TypeParameter(parameter.Ordinal),
            IArrayTypeSymbol array => SignatureType.ArrayOf(MapSymbolOnlyType(array.ElementType)),
            INamedTypeSymbol named when named.SpecialType == SpecialType.System_Char || IsRuntimeErasedValue(named) || named.SpecialType == SpecialType.None && IsSymbolOnlyReferenceDefinition((INamedTypeSymbol)named.OriginalDefinition) => ImportExternalType(named),
            _ => type.SpecialType switch
            {
                SpecialType.System_Int32 => PrimitiveType.Int32,
                SpecialType.System_Int64 => PrimitiveType.Int64,
                SpecialType.System_Single => PrimitiveType.Single,
                SpecialType.System_Double => PrimitiveType.Double,
                SpecialType.System_Byte => PrimitiveType.Byte,
                SpecialType.System_SByte => PrimitiveType.SByte,
                SpecialType.System_Int16 => PrimitiveType.Int16,
                SpecialType.System_UInt16 => PrimitiveType.UInt16,
                SpecialType.System_UInt32 => PrimitiveType.UInt32,
                SpecialType.System_UInt64 => PrimitiveType.UInt64,
                SpecialType.System_Boolean => PrimitiveType.Boolean,
                SpecialType.System_String => PrimitiveType.String,
                SpecialType.System_Unit or SpecialType.System_Void => PrimitiveType.Void,
                _ => throw new InvalidOperationException("unsupported symbol-only signature")
            }
        };

        UnsupportedInputException Unsupported(string detail) => new(detail, diagnosticSyntax.GetLocation());

        static bool ReturnsValue(IMethodSymbol method) => method.ReturnType.SpecialType == SpecialType.System_Int32;

        SourceCallablePlan GetPlan(IMethodSymbol method)
        {
            if (!SourceCallablePlan.TryCreate(method, out var plan, NeoClrCapabilities.Shared))
                throw Unsupported("callable declaration: " + method.ToDisplayString());
            return plan!;
        }
    }
}

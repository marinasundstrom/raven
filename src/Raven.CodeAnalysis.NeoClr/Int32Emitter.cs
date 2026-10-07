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
        var runtimeServices = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);
        var interfaces = new List<SourceInterfacePlan>();
        var flagsEnums = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);
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
                    var symbol = model.GetDeclaredSymbol(declaration) as IMethodSymbol ?? throw Unsupported("function symbol unavailable");
                    if (symbol.IsAsync && compilation.Options.MetadataImportOptions?.AsyncAssemblyName is null) throw Unsupported("explicit native async provider required");
                    if (symbol.IsExtern)
                    {
                        if (!NeoClrRuntimeServiceDeclaration.TryCreate(options.CoreLibrary, symbol, declaration, out var service))
                            throw Unsupported("runtime services require internal nongeneric bodyless functions in neoCLR.Runtime with the core MethodImpl(InternalCall) attribute");
                        runtimeServices.Add(symbol);
                        plans.Add(service!);
                        continue;
                    }
                    if ((declaration.Body is null && declaration.ExpressionBody is null) || declaration.AttributeLists.Count != 0 ||
                        declaration.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.AsyncKeyword or SyntaxKind.UnsafeKeyword)))
                        throw Unsupported("only top-level functions with block or expression bodies");
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
                        if (extensionMember is ConversionOperatorDeclarationSyntax conversion && (conversion.Body is not null || conversion.ExpressionBody is not null) &&
                            conversion.AttributeLists.Count == 0 && conversion.Modifiers.All(m => m.Kind is SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.StaticKeyword) &&
                            model.GetDeclaredSymbol(conversion) is IMethodSymbol { IsStatic: true, MethodKind: MethodKind.Conversion } conversionMethod)
                        {
                            plans.Add(GetPlan(conversionMethod));
                            continue;
                        }
                        if (extensionMember is MethodDeclarationSyntax asyncExtension &&
                            model.GetDeclaredSymbol(asyncExtension) is IMethodSymbol { IsAsync: true })
                            throw Unsupported("native async state-machine emission");
                        if (extensionMember is not MethodDeclarationSyntax method ||
                            (method.Body is null && method.ExpressionBody is null) || method.AttributeLists.Count != 0 ||
                            method.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword or SyntaxKind.StaticKeyword)) ||
                            model.GetDeclaredSymbol(method) is not IMethodSymbol { IsStatic: true } symbol ||
                            !method.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword) && (!symbol.IsExtensionMethod || symbol.Parameters.IsEmpty))
                            throw Unsupported("implemented static or instance extension methods");
                        plans.Add(GetPlan(symbol));
                    }
                }
                else if (member is EnumDeclarationSyntax enumeration)
                {
                    if (model.GetDeclaredSymbol(enumeration) is not INamedTypeSymbol enumSymbol ||
                        !SourceTypePlan.TryCreate(enumSymbol, out var enumPlan, NeoClrCapabilities.Shared))
                        throw Unsupported("top-level public/internal Int32 enum");
                    var attributes = enumSymbol.GetAttributes();
                    if (!attributes.IsEmpty)
                    {
                        if (attributes.Length != 1 || attributes[0] is not { AttributeClass: { } attributeType, ConstructorArguments.IsEmpty: true, NamedArguments.IsEmpty: true } ||
                            attributeType.ToFullyQualifiedMetadataName() != "System.FlagsAttribute" || !NeoClrBindingContract.MatchesCore(attributeType.ContainingAssembly, options.CoreLibrary))
                            throw Unsupported("only the configured core FlagsAttribute on native enums");
                        flagsEnums.Add(enumSymbol);
                    }
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
                    var typeSymbol = model.GetDeclaredSymbol(type) as INamedTypeSymbol ?? throw Unsupported("type symbol unavailable");
                    if (type.AttributeLists.Count != 0 || type.ParameterList is not null ||
                        type.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.StaticKeyword or SyntaxKind.PartialKeyword or SyntaxKind.OpenKeyword or SyntaxKind.SealedKeyword or SyntaxKind.AbstractKeyword)))
                        throw Unsupported("only public or internal static/root classes or value types without additional contracts");
                    if (!SourceTypePlan.TryCreate(typeSymbol, out var typePlan, NeoClrCapabilities.Shared))
                        throw Unsupported("supported public/internal static classes, root classes or unconstrained value types: " + typeSymbol.ToDisplayString());
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
                            if (propertySyntax.AttributeLists.Count != 0 ||
                                propertySyntax.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword or SyntaxKind.StaticKeyword)) ||
                                model.GetDeclaredSymbol(propertySyntax) is not SourcePropertySymbol property ||
                                property.IsStatic && (property.BackingField is not null || propertySyntax.Initializer is not null) ||
                                !CallableSignature.TryType(property.Type, false, out _, NeoClrCapabilities.Shared))
                                throw Unsupported("only supported instance properties/storage or implemented static properties without storage");
                            if (propertySyntax.AccessorList is { } accessorList && accessorList.Accessors.Any(a =>
                                a.Kind is not (SyntaxKind.GetAccessorDeclaration or SyntaxKind.SetAccessorDeclaration) ||
                                a.AttributeLists.Count != 0 || (a.Body is null && a.ExpressionBody is null && property.BackingField is null) ||
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
                                constructor.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword or SyntaxKind.ProtectedKeyword)))
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
                        if (typeMember is MethodDeclarationSyntax asyncMember &&
                            model.GetDeclaredSymbol(asyncMember) is IMethodSymbol { IsAsync: true } &&
                            compilation.Options.MetadataImportOptions?.AsyncAssemblyName is null)
                            throw Unsupported("explicit native async provider required");
                        if (typeMember is not MethodDeclarationSyntax method || (method.Body is null && method.ExpressionBody is null && !method.Modifiers.Any(m => m.Kind == SyntaxKind.AbstractKeyword)) || method.AttributeLists.Count != 0 ||
                            method.ExplicitInterfaceSpecifier is not null || method.ConstraintClauses.Count != 0 ||
                            method.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword or SyntaxKind.StaticKeyword or SyntaxKind.OverrideKeyword or SyntaxKind.AsyncKeyword or SyntaxKind.VirtualKeyword or SyntaxKind.AbstractKeyword or SyntaxKind.UnsafeKeyword)))
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
            if (primitive == PrimitiveType.RuntimeTypeHandle)
            {
                if (!symbol.IsValueType || symbol.Arity != 0 || symbol.ContainingType is not null || fieldsForType.Length != 0 ||
                    plans.Any(p => SymbolEqualityComparer.Default.Equals(p.Symbol.ContainingType, symbol) && p.Symbol.MethodKind is MethodKind.Constructor or MethodKind.StaticConstructor))
                    throw Unsupported("runtime type handle implementation requires an empty value declaration without constructors");
                primitiveOwners.Add(symbol, primitive);
                continue;
            }
            if (symbol.IsValueType != (primitive != PrimitiveType.String) || symbol.Arity != 0 || symbol.ContainingType is not null || fieldsForType.Length != 1 ||
                fieldsForType[0] is not { Name: "m_value", DeclaredAccessibility: Accessibility.Private, IsStatic: false, IsReadOnly: false } field ||
                field.Type.SpecialType.ToString() != "System_" + primitive ||
                primitive != PrimitiveType.String && plans.Any(p => SymbolEqualityComparer.Default.Equals(p.Symbol.ContainingType, symbol) && p.Symbol.MethodKind == MethodKind.Constructor))
                throw Unsupported("primitive implementation requires canonical storage; only String admits constructors: " + primitive);
            primitiveOwners.Add(symbol, primitive);
            primitiveFields.Add(field, primitive);
        }
        INamedTypeSymbol? graphemeOwner = null;
        IFieldSymbol? graphemeField = null;
        if (options.ImplementsGrapheme)
        {
            graphemeOwner = declaredTypes.Keys.SingleOrDefault(t => t.ToFullyQualifiedMetadataName() == "System.Char")
                ?? throw Unsupported("selected grapheme implementation is missing: System.Char");
            var storage = storageFields.Where(f => SymbolEqualityComparer.Default.Equals(f.ContainingType, graphemeOwner)).ToArray();
            if (!graphemeOwner.IsValueType || graphemeOwner.Arity != 0 || graphemeOwner.ContainingType is not null || storage.Length != 1 ||
                storage[0] is not { Name: "m_value", DeclaredAccessibility: Accessibility.Private, IsStatic: false, IsReadOnly: false } field ||
                field.Type.SpecialType != SpecialType.System_Char ||
                plans.Any(p => SymbolEqualityComparer.Default.Equals(p.Symbol.ContainingType, graphemeOwner) && p.Symbol.MethodKind == MethodKind.Constructor))
                throw Unsupported("grapheme implementation requires canonical Char storage and no explicit constructors");
            graphemeField = field;
        }
        foreach (var type in declaredTypes.Values.Where(t => !t.IsStatic && !primitiveOwners.ContainsKey(t.Symbol) && !SymbolEqualityComparer.Default.Equals(t.Symbol, graphemeOwner)))
            foreach (var constructor in type.Symbol.GetMembers().OfType<IMethodSymbol>().Where(m => m.MethodKind == MethodKind.Constructor))
                if (!plans.Any(p => SymbolEqualityComparer.Default.Equals(p.Symbol, constructor)))
                {
                    if (constructor.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax() is not (ClassDeclarationSyntax or StructDeclarationSyntax)) throw Unsupported("constructor unavailable");
                    plans.Add(GetPlan(constructor));
                }
        var prepared = new List<(SourceCallablePlan Plan, LinearMethodBody? Body)>();
        foreach (var plan in plans)
        {
            if (runtimeServices.Contains(plan.Symbol) || plan.Symbol.IsAbstract)
            {
                prepared.Add((plan, null));
                continue;
            }
            if (!plan.TryLowerBody(compilation, IsConsoleCall, out var body, out var failure, NeoClrCapabilities.Shared))
                throw new UnsupportedInputException(failure!.Detail, failure.Syntax.GetLocation());
            prepared.Add((plan, body!));
        }
        foreach (var machine in compilation.GetSynthesizedAsyncStateMachineTypes().ToArray())
        {
            if (!SourceTypePlan.TryCreate(machine, out var typePlan, NeoClrCapabilities.Shared))
                throw Unsupported("supported native heap state-machine declaration");
            declaredTypes.Add(machine, typePlan!);
            storageFields.AddRange(machine.GetMembers().OfType<IFieldSymbol>());
            var anchor = machine.AsyncMethod.DeclaringSyntaxReferences.Single().GetSyntax();
            var constructorBody = new BoundBlockStatement(machine.ConstructorFields.Select((field, index) =>
                (BoundStatement)new BoundAssignmentStatement(new BoundFieldAssignmentExpression(
                    new BoundSelfExpression(machine), field, new BoundParameterAccess(machine.Constructor.Parameters[index]),
                    compilation.GetSpecialType(SpecialType.System_Unit)))).ToArray());
            foreach (var (method, methodBody) in new[] {
                (machine.Constructor, constructorBody), (machine.MoveNextMethod, machine.MoveNextBody),
                (machine.SetStateMachineMethod, machine.SetStateMachineBody) })
            {
                if (methodBody is null || !CallableSignature.TryCreate(method, out var signature, NeoClrCapabilities.Shared))
                    throw Unsupported("synthesized async method signature/body");
                var plan = new SourceCallablePlan(method, anchor, anchor, machine, method.MetadataName, signature, PreparedBody: methodBody);
                if (!plan.TryLowerBody(compilation, IsConsoleCall, out var body, out var failure, NeoClrCapabilities.Shared))
                    throw new UnsupportedInputException(failure!.Detail, failure.Syntax.GetLocation());
                prepared.Add((plan, body));
            }
        }
        var closureCaptures = new Dictionary<IMethodSymbol, ITypeSymbol[]>(SymbolEqualityComparer.Default);
        var lambdaSymbols = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);
        for (var index = 0; index < prepared.Count; index++)
            foreach (var (function, syntax) in prepared[index].Body?.Functions ?? [])
            {
                var symbol = (IMethodSymbol)function.Symbol!;
                if (!lambdaSymbols.Add(symbol)) continue;
                if (!CallableSignature.TryCreate(symbol, out var signature, NeoClrCapabilities.Shared)) throw Unsupported("unsupported Function body signature");
                var captures = function.CapturedVariables.Select(capture => capture switch
                {
                    INamedTypeSymbol selfType => selfType,
                    ILocalSymbol local => local.Type,
                    IParameterSymbol parameter => parameter.Type,
                    _ => throw Unsupported("unsupported closure capture")
                }).ToArray();
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
            if (compilation.IsSourceObjectRoot(type)) return nativeTypes[type];
            if (type.SpecialType == SpecialType.System_Char && graphemeOwner is not null)
                return nativeTypes[graphemeOwner];
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
                if (original.TypeKind == TypeKind.Class && original.BaseType is { SpecialType: not SpecialType.System_Object } classBase)
                    assembly.DeclareClassBase(imported, ImportExternalType(classBase).ImportedType!);
                if (original.SpecialType == SpecialType.System_Char && original.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null })
                    assembly.SetNativeGrapheme(imported);
                if (HasNativePrimitiveStorage(original.SpecialType) && original.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null })
                    assembly.SetNativePrimitive(imported, NeoClrTypeMapper.Instance.Map(Enum.Parse<EmissionPrimitiveType>(original.SpecialType.ToString()[7..])));
                if (IsSymbolOnlyReferenceDefinition(original) || IsSymbolOnlyOwnerDefinition(original) && original.IsValueType)
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
        void DefineClass(SourceTypePlan type)
        {
            if (nativeTypes.ContainsKey(type.Symbol)) return;
            if (type.ClassBase is { } parent) DefineClass(declaredTypes[parent]);
            if (type.MetadataOwner is { } container) DefineClass(declaredTypes[container]);
            var definition = type.Define(typeDefinitions);
            if (primitiveOwners.TryGetValue(type.Symbol, out var primitive)) definition.SetNativePrimitive(primitive);
            if (SymbolEqualityComparer.Default.Equals(type.Symbol, graphemeOwner)) definition.SetNativeGrapheme();
            if (flagsEnums.Contains(type.Symbol)) definition.SetEnumFlags();
            if (!type.IsValueType && !type.IsStatic && !type.IsClosedHierarchy && type.Symbol.IsClosed)
                definition.SetSealedClass();
            nativeTypes.Add(type.Symbol, definition);
            owners.Add(type.Symbol, new(assembly, definition, type => nativeTypes[type], ImportExternalType));
        }
        foreach (var type in declaredTypes.Values) DefineClass(type);
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
        var nativeInterfaces = new Dictionary<INamedTypeSymbol, TypeBuilder>(SymbolEqualityComparer.Default);
        foreach (var contract in interfaces)
        {
            var symbol = contract.Symbol;
            var visibility = symbol.DeclaredAccessibility == Accessibility.Public ? TypeVisibility.Public : TypeVisibility.Internal;
            nativeInterfaces.Add(symbol, symbol.IsSealedHierarchy ? assembly.AddClosedInterface(contract.Namespace, contract.Name, visibility)
                : symbol.Arity == 0 ? assembly.AddInterface(contract.Namespace, contract.Name, visibility)
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
            foreach (var pair in contractMethods)
            {
                pair.Value.SetNullableAnnotation(-1, NullableAnnotationEmitter.Create(pair.Key.ReturnType));
                for (var i = 0; i < pair.Key.Parameters.Length; i++)
                {
                    pair.Value.SetNullableAnnotation(i, NullableAnnotationEmitter.Create(pair.Key.Parameters[i].Type));
                    pair.Value.SetParameterName(i, pair.Key.Parameters[i].Name);
                    if (pair.Key.Parameters[i].IsVarParams) pair.Value.SetParameterArray(i);
                }
                interfaceMethods.Add(pair.Key, pair.Value);
            }
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
            if (primitiveFields.ContainsKey(field) || SymbolEqualityComparer.Default.Equals(field, graphemeField)) continue;
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
        var methods = new List<(SourceCallablePlan Plan, MetadataMethod Method, LinearMethodBody? Body)>();
        foreach (var (plan, body) in prepared)
        {
            diagnosticSyntax = plan.Syntax;
            MetadataMethod definition;
            if (closureCaptures.TryGetValue(plan.Symbol, out var captures))
            {
                var frameName = "$closure$" + closureFields.Count;
                var frame = plan.Symbol.ContainingType is { } lexicalOwner && nativeTypes.TryGetValue(lexicalOwner, out var nativeOwner)
                    ? nativeOwner.AddNestedClass(frameName, TypeVisibility.Internal)
                    : assembly.AddClass("", frameName, TypeVisibility.Internal);
                var captureFields = captures.Select((capture, index) => frame.AddField("capture" + index,
                    NeoClrTypeMapper.Map(capture, type => nativeTypes[type], ImportExternalType), FieldVisibility.Private)).ToArray();
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
            if (runtimeServices.Contains(plan.Symbol)) definition.SetInternalCall();
            definition.SetNullableAnnotation(-1, NullableAnnotationEmitter.Create(plan.Symbol.ReturnType));
            for (var i = 0; i < plan.Symbol.Parameters.Length; i++)
            {
                definition.SetNullableAnnotation(i, NullableAnnotationEmitter.Create(plan.Symbol.Parameters[i].Type));
                definition.SetParameterName(i, plan.Symbol.Parameters[i].Name);
                if (plan.Symbol.Parameters[i].IsVarParams) definition.SetParameterArray(i);
            }
            methods.Add((plan, definition, body));
        }
        var definedMethods = methods.ToDictionary(m => m.Plan.Symbol, m => m.Method, (IEqualityComparer<IMethodSymbol>)SymbolEqualityComparer.Default);
        if (unions.Count != 0)
        {
            // Source definitions own their marker identity and base construction. Resolve
            // only output symbols/builders; emission never reopens imported metadata.
            var sourceMarker = declaredTypes.Keys.SingleOrDefault(type =>
                type.ToFullyQualifiedMetadataName() == "System.Runtime.CompilerServices.UnionAttribute");
            MetadataMethod? markerConstructor = null;
            if (sourceMarker is not null)
            {
                var constructor = sourceMarker.Constructors.SingleOrDefault(method => !method.IsStatic &&
                    method.Parameters.IsEmpty && method.DeclaredAccessibility == Accessibility.Public);
                if (constructor is null || !definedMethods.TryGetValue(constructor, out markerConstructor))
                    throw Unsupported("source UnionAttribute requires a public parameterless constructor");
            }
            NeoClrUnionMetadata.Emit(assembly, unions, type => nativeTypes[type], markerConstructor);
        }
        foreach (var (plan, definition, _) in methods)
            foreach (var implementation in plan.Symbol.ExplicitInterfaceImplementations)
            {
                var contract = implementation.ContainingType!;
                if (contract.OriginalDefinition.DeclaringSyntaxReferences.IsEmpty)
                    definition.AddExplicitInterfaceImplementation(ImportExternalType(contract).ImportedType!, implementation.MetadataName);
                else
                    definition.AddExplicitInterfaceImplementation(nativeTypes[(INamedTypeSymbol)contract.OriginalDefinition], implementation.MetadataName,
                        contract.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType)).ToArray());
            }
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
                foreach (var argument in target.TypeArguments.OfType<INamedTypeSymbol>().Where(t => t.SpecialType is not SpecialType.None && t.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null }))
                    _ = ImportExternalType(argument);
                if (definedMethods.TryGetValue(target.OriginalDefinition ?? target, out var definition))
                    return NeoClrCallableReference.Create(definition.MakeGenericInstance(target.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType)).ToArray()));
                var arguments = target.TypeArguments.Select(t =>
                    NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType)).ToArray();
                return NeoClrCallableReference.Create(Import(target.OriginalDefinition ?? target).MakeGenericInstance(arguments));
            }
            // Nested member lookup may expose a substituted method even under a
            // nongeneric owner. Reuse its declaration identity before resolving imports.
            if (definedMethods.TryGetValue(target.OriginalDefinition ?? target, out var declared))
                return NeoClrCallableReference.Create(declared, target.IsVirtual && target.ContainingType?.IsValueType == false);
            if (options.BootstrapReference is { } bootstrap && target.ContainingType is { } bootstrapOwner &&
                SymbolEqualityComparer.Default.Equals(target.ContainingAssembly, compilation.GetAssemblyOrModuleSymbol(bootstrap)))
            {
                if (compilation.Options.MetadataImportOptions?.PrimitiveAssemblies.ContainsKey(bootstrapOwner.SpecialType) == true &&
                    compilation.GetSpecialType(bootstrapOwner.SpecialType) is INamedTypeSymbol provider &&
                    !SymbolEqualityComparer.Default.Equals(provider.ContainingAssembly, bootstrapOwner.ContainingAssembly))
                {
                    var members = provider.GetMembers().OfType<IMethodSymbol>().Where(m =>
                        m.MetadataName == target.MetadataName && m.IsStatic == target.IsStatic && m.Arity == target.Arity &&
                        SymbolEqualityComparer.Default.Equals(m.ReturnType, target.ReturnType) &&
                        m.Parameters.Length == target.Parameters.Length && m.Parameters.Zip(target.Parameters).All(p =>
                            p.First.RefKind == p.Second.RefKind && SymbolEqualityComparer.Default.Equals(p.First.Type, p.Second.Type))).ToArray();
                    if (members.Length != 1) throw Unsupported("selected native primitive member is missing or ambiguous: " + target);
                    return NeoClrCallableReference.Create(Import(members[0]));
                }
                var implementation = primitiveOwners.Keys.Concat(graphemeOwner is null ? [] : new[] { graphemeOwner }).SingleOrDefault(t =>
                    t.ToFullyQualifiedMetadataName() == bootstrapOwner.ToFullyQualifiedMetadataName());
                if (implementation is not null)
                {
                    var candidates = definedMethods.Where(pair =>
                        SymbolEqualityComparer.Default.Equals(pair.Key.ContainingType, implementation) &&
                        pair.Key.MetadataName == target.MetadataName && pair.Key.IsStatic == target.IsStatic &&
                        pair.Key.Arity == target.Arity &&
                        SymbolEqualityComparer.Default.Equals(pair.Key.ReturnType, target.ReturnType) &&
                        pair.Key.Parameters.Length == target.Parameters.Length &&
                        pair.Key.Parameters.Zip(target.Parameters).All(p => p.First.RefKind == p.Second.RefKind &&
                            SymbolEqualityComparer.Default.Equals(p.First.Type, p.Second.Type))).ToArray();
                    if (candidates.Length != 1) throw Unsupported("selected primitive source member is missing or ambiguous: " + target);
                    return NeoClrCallableReference.Create(candidates[0].Value);
                }
            }
            var systemFunction = ImportSystem(target);
            return systemFunction is not null
                ? NeoClrCallableReference.Create(systemFunction)
                : NeoClrCallableReference.Create(Import(target));
        });
        foreach (var declaration in methods)
            if (!declaration.Plan.Symbol.IsGenericMethod && declaration.Plan.Symbol.ContainingType?.Arity is not > 0)
                references.Declare(declaration.Plan.Symbol, NeoClrCallableReference.Create(declaration.Method, declaration.Plan.Symbol.IsVirtual && declaration.Plan.Symbol.ContainingType?.IsValueType == false));
        if (compilation.Options.OutputKind == OutputKind.ConsoleApplication)
        {
            var entry = compilation.GetEntryPoint() ?? throw Unsupported("entry point unavailable");
            var implementation = methods.SingleOrDefault(m => SymbolEqualityComparer.Default.Equals(m.Plan.Symbol, entry)).Method
                ?? throw Unsupported("entry must be a declared Int32/Unit function or static method");
            if (entry.ReturnType is INamedTypeSymbol { SpecialType: SpecialType.System_Threading_Tasks_Task_T, TypeArguments.Length: 1 } task)
            {
                var result = task.TypeArguments[0];
                if (result.SpecialType is not (SpecialType.System_Int32 or SpecialType.System_Unit or SpecialType.System_Void))
                    throw Unsupported("native async entry requires Task<int> or Task<unit>");
                var getResult = task.GetMembers("GetResult").OfType<IMethodSymbol>().SingleOrDefault(m =>
                    !m.IsStatic && m.Arity == 0 && m.Parameters.IsEmpty && m.DeclaredAccessibility == Accessibility.Public &&
                    SymbolEqualityComparer.Default.Equals(m.ReturnType, result))
                    ?? throw Unsupported("native async entry GetResult contract unavailable");
                var drain = compilation.GetTypeByMetadataName("System.Runtime.CompilerServices.RuntimeServices")?
                    .GetMembers("DrainEntryTasks").OfType<IMethodSymbol>().SingleOrDefault(m =>
                        m.IsStatic && m.Arity == 0 && m.Parameters.IsEmpty && m.DeclaredAccessibility == Accessibility.Public &&
                        m.ReturnType.SpecialType is SpecialType.System_Unit or SpecialType.System_Void)
                    ?? throw Unsupported("native async entry requires explicit DrainEntryTasks runtime binding");
                var adapter = assembly.AddFunction("Raven.Generated", "<AsyncEntry>",
                    new MethodSignature(PrimitiveType.Int32, implementation.Signature.ParameterTypes), MethodVisibility.Internal);
                var body = adapter.GetILGenerator();
                for (var i = 0; i < entry.Parameters.Length; i++) body.LoadArgument(i);
                body.Call(implementation);
                // The runtime drains registered work while retaining the caller frame
                // and its task value, then resumes here to observe completion/failure.
                references.Resolve(drain).EmitCall(body);
                references.Resolve(getResult).EmitCall(body);
                if (result.SpecialType != SpecialType.System_Int32)
                {
                    body.Emit(OpCode.Pop);
                    body.LoadConstant(0);
                }
                body.Return();
                assembly.EntryPoint = adapter;
            }
            else assembly.EntryPoint = implementation;
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
            if (current.Body is null) continue;
            diagnosticSyntax = current.Plan.Syntax;
            current.Body.Emit(new NeoClrLinearMethodBuilder(current.Method.GetILGenerator(), (instruction, output) =>
            {
                diagnosticSyntax = instruction.Syntax;
                if (instruction.Kind is LinearInstructionKind.BaseConstructorCall or LinearInstructionKind.DirectInstanceCall)
                    output.Call(definedMethods[instruction.Method!.OriginalDefinition]);
                else if (instruction.Kind == LinearInstructionKind.FunctionBind)
                {
                    var symbol = instruction.Method!;
                    if (!definedMethods.TryGetValue(symbol.OriginalDefinition, out var target) && !interfaceMethods.TryGetValue(symbol.OriginalDefinition, out target))
                        throw Unsupported("Function binding requires an owned target");
                    if (closureConstructors.TryGetValue(symbol, out var constructor)) output.NewObject(constructor);
                    var shape = NeoClrTypeMapper.Map(instruction.Type!, type => nativeTypes[type], ImportExternalType);
                    void Bind(SignatureType bindingShape)
                    {
                        if (symbol.ContainingType is { Arity: > 0 } owner)
                            output.BindFunction(bindingShape, target.MakeConstructedReference(owner.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType)), symbol.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType))));
                        else if (symbol.IsGenericMethod) output.BindFunction(bindingShape, target.MakeGenericInstance(symbol.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType)).ToArray()));
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
                else if (instruction.Kind == LinearInstructionKind.ConstrainedCall)
                {
                    var implementing = NeoClrTypeMapper.Map(instruction.Type!, type => nativeTypes[type], ImportExternalType);
                    if (interfaceMethods.TryGetValue(instruction.Method!.OriginalDefinition ?? instruction.Method, out var ownedContract))
                        output.CallConstrained(implementing, ownedContract);
                    else if (instruction.Method!.ContainingType is { Arity: > 0 } constrainedOwner)
                        output.CallConstrained(implementing, Import(instruction.Method.OriginalDefinition ?? instruction.Method).MakeConstructedReference(
                            constrainedOwner.TypeArguments.Select(t => NeoClrTypeMapper.Map(RuntimeSelfTypes.Substitute(compilation, t, instruction.Type!), type => nativeTypes[type], ImportExternalType))));
                    else output.CallConstrained(implementing, Import(instruction.Method!));
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
                else if (instruction.Kind == LinearInstructionKind.Call && IsTypeHandleIntrinsic(instruction.Method!))
                    output.LoadTypeToken(NeoClrTypeMapper.Map(instruction.Method!.TypeArguments[0], type => nativeTypes[type], ImportExternalType));
                else if (instruction.Kind == LinearInstructionKind.Call && IsCheckedReservation(instruction.Method!))
                    output.ReserveArray(NeoClrTypeMapper.Map(instruction.Method!.TypeArguments[0], type => nativeTypes[type], ImportExternalType));
                else references.Resolve(instruction.Method!).EmitCall(output);
            }, field =>
            {
                if (SymbolEqualityComparer.Default.Equals(field, graphemeField)) return new NeoClrFieldReference(null, GraphemeStorage: nativeTypes[graphemeOwner!]);
                if (primitiveFields.TryGetValue(field, out var primitive)) return new NeoClrFieldReference(null, IntrinsicStorage: primitive, StringConstructorStorage: current.Plan.Symbol.MethodKind == MethodKind.Constructor);
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

        bool IsTypeHandleIntrinsic(IMethodSymbol method)
        {
            if (options.BootstrapReference is null ||
                !SymbolEqualityComparer.Default.Equals(method.ContainingAssembly, compilation.GetAssemblyOrModuleSymbol(options.BootstrapReference)) ||
                method.ContainingType?.ToFullyQualifiedMetadataName() != "System.Runtime.CompilerServices.RuntimeServices" || method.Name != "TypeHandle")
                return false;
            var definition = method.OriginalDefinition;
            if (definition.ContainingType?.IsStatic != true || !definition.IsStatic || definition.IsVirtual ||
                definition.DeclaredAccessibility != Accessibility.Public || definition.TypeParameters.Length != 1 ||
                method.TypeArguments.Length != 1 || !definition.Parameters.IsEmpty ||
                definition.TypeParameters[0].ConstraintKind != TypeParameterConstraintKind.None || !definition.TypeParameters[0].ConstraintTypes.IsEmpty ||
                definition.ReturnType.SpecialType != SpecialType.System_RuntimeTypeHandle ||
                method.TypeArguments[0] is INamedTypeSymbol { IsUnboundGenericType: true })
                throw Unsupported("invalid bootstrap type-handle contract");
            return true;
        }

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
                (owner.TypeKind == TypeKind.Interface ? symbol.IsAbstract && symbol.IsVirtual : !symbol.IsAbstract && (!symbol.IsVirtual || owner.IsValueType || symbol.IsOverride)) &&
                symbol.DeclaredAccessibility == Accessibility.Public && (symbol.IsStatic || symbol.Arity == 0) &&
                CallableSignature.TryCreate(symbol, out var memberSignature, NeoClrCapabilities.Shared) &&
                IsSymbolOnlyType(symbol.ReturnType, true) && symbol.Parameters.All(p => p.RefKind is RefKind.None or RefKind.Ref or RefKind.Out && IsSymbolOnlyType(p.Type, false)))
            {
                _ = ImportExternalType(owner);
                var declaration = importedTypes[(INamedTypeSymbol)owner.OriginalDefinition];
                var contract = new MethodSignature(MapSymbolOnlyType(symbol.ReturnType, result: true),
                    symbol.Parameters.Select(p => p.RefKind == RefKind.None ? MapSymbolOnlyType(p.Type) : SignatureType.ByReference(MapSymbolOnlyType(p.Type))),
                    memberSignature.GenericParameterNames, memberSignature.OutParameters.IsDefault ? [] : memberSignature.OutParameters);
                return assembly.CreateMethodReference(declaration, symbol.MetadataName, contract, symbol.IsStatic, isOverride: symbol.IsOverride, nativePrimitive: HasNativePrimitiveStorage(owner.SpecialType) && owner.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null }
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
        static bool HasNativePrimitiveStorage(SpecialType type) => type is SpecialType.System_SByte or SpecialType.System_Byte or
            SpecialType.System_Int16 or SpecialType.System_UInt16 or SpecialType.System_Int32 or SpecialType.System_UInt32 or
            SpecialType.System_Int64 or SpecialType.System_UInt64 or SpecialType.System_IntPtr or SpecialType.System_UIntPtr or SpecialType.System_Single or SpecialType.System_Double or
            SpecialType.System_String or SpecialType.System_Boolean;
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
            (original.BaseType is null || original.BaseType.SpecialType == (original.TypeKind == TypeKind.Enum ? SpecialType.System_Enum : original.IsValueType ? SpecialType.System_ValueType : SpecialType.System_Object) ||
                original.TypeKind == TypeKind.Class && original.Arity == 0 && original.BaseType is { Arity: 0, ContainingType: null } parentClass &&
                SymbolEqualityComparer.Default.Equals(original.ContainingAssembly, parentClass.ContainingAssembly) && IsSymbolOnlyReferenceDefinition(parentClass, depth + 1)) &&
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
            type.GetNullableAbiProjection() == NullableAbiProjection.AnnotatedUnderlyingType ? IsSymbolOnlyType(type.GetNonNullableType(), result) :
            type is IPointerTypeSymbol pointer && CallableSignature.IsSupportedPointer(pointer) ||
            RuntimeSelfTypes.IsSelf(compilation, type) ||
            type is INamedTypeSymbol { TypeKind: TypeKind.Delegate } functionType && CallableSignature.TryFunction(functionType, out _, NeoClrCapabilities.Shared) ||
            !result && IsRuntimeUnitValue(type) || IsRuntimeErasedValue(type) ||
            type is ITypeParameterSymbol ||
            type is IArrayTypeSymbol { Rank: 1, FixedLength: null, ElementType: not IArrayTypeSymbol } vector && IsSymbolOnlyType(vector.ElementType, false) ||
            type.SpecialType is SpecialType.System_Object or SpecialType.System_RuntimeTypeHandle or SpecialType.System_SByte or SpecialType.System_Int16 or SpecialType.System_UInt16 or SpecialType.System_UInt32 or SpecialType.System_UInt64 or SpecialType.System_IntPtr or SpecialType.System_UIntPtr or SpecialType.System_Byte or SpecialType.System_Int32 or SpecialType.System_Int64 or SpecialType.System_Single or SpecialType.System_Double or SpecialType.System_Boolean or SpecialType.System_String or SpecialType.System_Char ||
            result && type.SpecialType is SpecialType.System_Unit or SpecialType.System_Void ||
            type is INamedTypeSymbol named && IsSymbolOnlyReferenceDefinition((INamedTypeSymbol)named.OriginalDefinition) &&
            named.TypeArguments.All(argument => IsSymbolOnlyType(argument, false));

        SignatureType MapSymbolOnlyType(ITypeSymbol type, bool result = false) => type switch
        {
            _ when type.GetNullableAbiProjection() == NullableAbiProjection.AnnotatedUnderlyingType => MapSymbolOnlyType(type.GetNonNullableType(), result),
            IPointerTypeSymbol pointer => NeoClrTypeMapper.Map(pointer, owned => nativeTypes[owned], ImportExternalType),
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
            INamedTypeSymbol named when named.SpecialType is SpecialType.System_Char or SpecialType.System_Object || IsRuntimeErasedValue(named) || named.SpecialType is (SpecialType.None or SpecialType.System_Threading_Tasks_Task_T or SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder_T or SpecialType.System_Runtime_CompilerServices_IAsyncStateMachine) && IsSymbolOnlyReferenceDefinition((INamedTypeSymbol)named.OriginalDefinition) => ImportExternalType(named),
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
                SpecialType.System_IntPtr => PrimitiveType.IntPtr,
                SpecialType.System_UIntPtr => PrimitiveType.UIntPtr,
                SpecialType.System_RuntimeTypeHandle => PrimitiveType.RuntimeTypeHandle,
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

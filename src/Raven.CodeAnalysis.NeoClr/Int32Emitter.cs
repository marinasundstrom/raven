using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using MetadataMethod = NeoCLR.Metadata.Experimental.Model.MethodBuilder;

namespace Raven.CodeAnalysis.NeoClr;

// Native declaration collection and reference resolution. Body emission consumes the same
// lowered bound trees as the .NET backend through the shared portable instruction plan.
internal static class Int32Emitter
{
    internal static byte[] Emit(Compilation compilation, NeoClrEmitOptions options,
        IReadOnlyList<(IAssemblySymbol Symbol, NeoClrMetadataDependency Dependency)> dependencies)
    {
        var nativeResolver = new NativeAssemblyResolver(dependencies.Select(d => d.Dependency.Definition));
        SyntaxNode diagnosticSyntax = compilation.SyntaxTrees[0].GetRoot();
        var plans = new List<SourceCallablePlan>();
        var interfaces = new List<SourceInterfacePlan>();
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
                else if (member is InterfaceDeclarationSyntax interfaceSyntax)
                {
                    if (model.GetDeclaredSymbol(interfaceSyntax) is not INamedTypeSymbol interfaceSymbol ||
                        !SourceInterfacePlan.TryCreate(interfaceSymbol, NeoClrCapabilities.Shared, out var interfacePlan))
                        throw Unsupported("only invariant owned interfaces with public abstract instance method contracts");
                    interfaces.Add(interfacePlan!);
                }
                else if (member is ClassDeclarationSyntax type)
                {
                    if (type.AttributeLists.Count != 0 || type.ParameterList is not null ||
                        type.PermitsClause is not null ||
                        type.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.StaticKeyword or SyntaxKind.PartialKeyword or SyntaxKind.OpenKeyword)))
                        throw Unsupported("only public or internal static or root classes without additional contracts");
                    var typeSymbol = model.GetDeclaredSymbol(type) as INamedTypeSymbol ?? throw Unsupported("type symbol unavailable");
                    if (!SourceTypePlan.TryCreate(typeSymbol, out var typePlan, NeoClrCapabilities.Shared))
                        throw Unsupported("only public or internal unconstrained static or root classes");
                    // Partial declarations share one semantic identity and one metadata definition.
                    // Still validate every part and collect all of its members.
                    declaredTypes.TryAdd(typeSymbol, typePlan!);
                    foreach (var typeMember in type.Members)
                    {
                        diagnosticSyntax = typeMember;
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
                                !CallableSignature.TryType(indexer.Type, false, out _))
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
                                !CallableSignature.TryType(property.Type, false, out _))
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
                        if (typeMember is not MethodDeclarationSyntax method || (method.Body is null && method.ExpressionBody is null) || method.AttributeLists.Count != 0 ||
                            method.ExplicitInterfaceSpecifier is not null || method.ConstraintClauses.Count != 0 ||
                            method.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword or SyntaxKind.StaticKeyword)))
                            throw Unsupported("only ordinary primitive methods, explicit constructors and auto-properties");
                        var symbol = model.GetDeclaredSymbol(method) as IMethodSymbol ?? throw Unsupported("method symbol unavailable");

                        var plan = GetPlan(symbol);
                        plans.Add(plan);
                    }
                }
                else throw Unsupported("only top-level functions and supported source class declarations");
            }
        }
        foreach (var type in declaredTypes.Values.Where(t => !t.IsStatic))
            foreach (var constructor in type.Symbol.GetMembers().OfType<IMethodSymbol>().Where(m => m.MethodKind == MethodKind.Constructor))
                if (!plans.Any(p => SymbolEqualityComparer.Default.Equals(p.Symbol, constructor)))
                {
                    if (constructor.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax() is not ClassDeclarationSyntax) throw Unsupported("constructor unavailable");
                    plans.Add(GetPlan(constructor));
                }
        var prepared = new List<(SourceCallablePlan Plan, LinearMethodBody Body)>();
        foreach (var plan in plans)
        {
            if (!plan.TryLowerBody(compilation, IsConsoleCall, out var body, out var failure, NeoClrCapabilities.Shared))
                throw new UnsupportedInputException(failure!.Detail, failure.Syntax.GetLocation());
            prepared.Add((plan, body!));
        }
        var lambdaSymbols = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);
        for (var index = 0; index < prepared.Count; index++)
            foreach (var (function, syntax) in prepared[index].Body.Functions)
            {
                var symbol = (IMethodSymbol)function.Symbol!;
                if (!lambdaSymbols.Add(symbol)) continue;
                if (!CallableSignature.TryCreate(symbol, out var signature, NeoClrCapabilities.Shared)) throw Unsupported("unsupported Function body signature");
                signature = signature with { IsInstance = false };
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
        var typeDefinitions = new NeoClrTypeDefinitionBuilder(assembly);
        var nativeTypes = new Dictionary<INamedTypeSymbol, TypeBuilder>(SymbolEqualityComparer.Default);
        var importedTypes = new Dictionary<INamedTypeSymbol, ImportedTypeReference>(SymbolEqualityComparer.Default);
        SignatureType ImportExternalType(INamedTypeSymbol type)
        {
            var original = (INamedTypeSymbol)type.OriginalDefinition;
            if (!importedTypes.TryGetValue(original, out var imported))
            {
                var binding = dependencies.SingleOrDefault(d => SymbolEqualityComparer.Default.Equals(d.Symbol, original.ContainingAssembly)).Dependency
                    ?? throw Unsupported("unregistered dependency type: " + original.ToDisplayString());
                var name = original.ToFullyQualifiedMetadataName();
                var candidates = binding.Definition.MainModule.Types.Where(t => MatchesType(t, original)).Take(2).ToArray();
                if (candidates.Length != 1 || candidates[0].GenericArity != original.Arity || candidates[0].IsValueType != original.IsValueType)
                    throw Unsupported("dependency type unavailable or ambiguous: " + name);
                imported = assembly.ImportReference(candidates[0], binding.CoreLibrary);
                importedTypes.Add(original, imported);
            }
            return imported.GenericArity == 0 ? imported : imported.MakeGenericInstance(type.TypeArguments
                .Select(t => NeoClrTypeMapper.Map(t, owned => nativeTypes[owned], ImportExternalType)).ToArray());
        }
        var functions = new NeoClrCallableDefinitionBuilder(assembly, resolveClass: type => nativeTypes[type], resolveExternal: ImportExternalType);
        foreach (var type in declaredTypes.Values)
        {
            var definition = type.Define(typeDefinitions);
            nativeTypes.Add(type.Symbol, definition);
            owners.Add(type.Symbol, new(assembly, definition, type => nativeTypes[type], ImportExternalType));
        }
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
                if (!nativeInterfaces.TryGetValue((INamedTypeSymbol)inherited.OriginalDefinition, out var parent)) throw Unsupported("inherited interface must be an emitted declaration");
                if (inherited.Arity == 0) definition.AddBaseInterface(parent);
                else definition.AddBaseInterface(parent.MakeGenericInstance(inherited.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType)).ToArray()));
            }
            var contractMethods = new Dictionary<IMethodSymbol, MetadataMethod>(SymbolEqualityComparer.Default);
            foreach (var method in contract.Methods)
                contractMethods.Add(method.Symbol, definition.AddInterfaceMethod(method.Symbol.MetadataName, new MethodSignature(
                    NeoClrTypeMapper.Map(method.Signature.ReturnType, type => nativeTypes[type], ImportExternalType),
                    method.Signature.ParameterTypes.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType)), outParameters: method.Signature.OutParameters.IsDefault ? [] : method.Signature.OutParameters)));
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
                    throw Unsupported("interface implementation must be emitted");
                if (contract.Arity == 0) nativeTypes[type.Symbol].AddInterfaceImplementation(definition);
                else nativeTypes[type.Symbol].AddInterfaceImplementation(definition.MakeGenericInstance(
                    contract.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, owner => nativeTypes[owner], ImportExternalType)).ToArray()));
            }
        foreach (var type in declaredTypes.Values)
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
        var methods = new List<(SourceCallablePlan Plan, MetadataMethod Method, LinearMethodBody Body)>();
        foreach (var (plan, body) in prepared)
        {
            diagnosticSyntax = plan.Syntax;
            var owner = plan.IsAssemblyFunction ? functions : owners[plan.TypeOwner!];
            methods.Add((plan, plan.Define(owner), body));
        }
        var definedMethods = methods.ToDictionary(m => m.Plan.Symbol, m => m.Method, (IEqualityComparer<IMethodSymbol>)SymbolEqualityComparer.Default);
        foreach (var property in properties)
        {
            CallableSignature.TryType(property.Type, false, out var propertyType);
            var valueType = NeoClrTypeMapper.Map(propertyType, type => nativeTypes[type], ImportExternalType);
            nativeTypes[property.ContainingType!].AddProperty(property.MetadataName, valueType,
                property.GetMethod is null ? null : definedMethods[property.GetMethod], property.SetMethod is null ? null : definedMethods[property.SetMethod]);
        }
        var references = new CallableReferenceTable<NeoClrCallableReference>(target =>
        {
            if (target.ContainingType is { Arity: > 0 } owner)
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
        foreach (var current in methods)
        {
            diagnosticSyntax = current.Plan.Syntax;
            current.Body.Emit(new NeoClrLinearMethodBuilder(current.Method, (instruction, output) =>
            {
                diagnosticSyntax = instruction.Syntax;
                if (instruction.Kind == LinearInstructionKind.FunctionBind)
                {
                    if (!definedMethods.TryGetValue(instruction.Method!, out var target)) throw Unsupported("Function binding requires an owned static target");
                    output.BindFunction(NeoClrTypeMapper.Map(instruction.Type!, type => nativeTypes[type], ImportExternalType), target);
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
                if (fields.TryGetValue(field, out var definition)) return new NeoClrFieldReference(definition);
                if (field is SubstitutedFieldSymbol substituted && field.ContainingType is { Arity: > 0 } owner && fields.TryGetValue(substituted.OriginalField, out definition))
                    return new NeoClrFieldReference(definition, definition.MakeConstructedReference(owner.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], ImportExternalType)).ToArray()));
                if (field is NativeFieldSymbol nativeField)
                {
                    if (importedFields.TryGetValue(field, out var imported)) return imported;
                    var binding = dependencies.SingleOrDefault(d => SymbolEqualityComparer.Default.Equals(d.Symbol, field.ContainingAssembly)).Dependency
                        ?? throw Unsupported("unregistered dependency field");
                    if (!ReferenceEquals(nativeField.Definition.Module.Assembly, binding.Definition))
                        throw Unsupported("native field snapshot differs from semantic reference");
                    imported = new NeoClrFieldReference(null, Import: assembly.ImportReference(nativeField.Definition, binding.CoreLibrary, nativeResolver));
                    importedFields.Add(field, imported);
                    return imported;
                }
                throw Unsupported("undeclared instance field");
            },
                type => nativeTypes.TryGetValue(type, out var definition) ? definition : throw Unsupported("undeclared class local"), ImportExternalType));
        }
        return assembly.WriteNativeAssembly();

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
                else yield return member;
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
                ?? throw Unsupported("unregistered dependency: " + symbol.ContainingAssembly?.Name);
            var dependencyMetadata = binding.Definition;
            if (symbol is NativeMethodSymbol native)
            {
                if (!ReferenceEquals(native.Definition.Module.Assembly, dependencyMetadata))
                    throw Unsupported("native dependency snapshot differs from semantic reference");
                return assembly.ImportReference(native.Definition, binding.CoreLibrary, nativeResolver);
            }
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

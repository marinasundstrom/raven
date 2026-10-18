using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen;
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
        var bodyCapabilities = NeoClrCapabilities.Shared;
        SyntaxNode diagnosticSyntax = compilation.SyntaxTrees[0].GetRoot();
        var plans = new List<SourceCallablePlan>();
        var emittedAttributes = new HashSet<(SyntaxTree Tree, Raven.CodeAnalysis.Text.TextSpan Span)>();
        var payloadHelpers = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);
        var storageHelpers = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);
        var runtimeServices = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);
        var foreignImports = new Dictionary<IMethodSymbol, NativeImportDefinition>(SymbolEqualityComparer.Default);
        var interfaces = new List<SourceInterfacePlan>();
        var flagsEnums = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);
        var unions = new List<SourceUnionDeclarationPlan>();
        var properties = new List<SourcePropertySymbol>();
        var attributeProperties = new List<(IPropertySymbol Symbol, PropertyDefinition Definition)>();
        var storageFields = new List<IFieldSymbol>();
        var literalFields = new List<IFieldSymbol>();
        var assemblyConstants = new List<IFieldSymbol>();
        var declaredTypes = new Dictionary<INamedTypeSymbol, SourceTypePlan>(SymbolEqualityComparer.Default);
        // Collect all declarations before emitting any body, so calls do not depend on file order.
        foreach (var tree in compilation.SyntaxTrees)
        {
            var model = compilation.GetSemanticModel(tree);
            var root = (CompilationUnitSyntax)tree.GetRoot();
            diagnosticSyntax = root;
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
                        if (compilation.Options.MetadataImportOptions?.UseSourceCore == true &&
                            NeoClrStorageDeclaration.TryCreate(symbol, declaration, out var storageHelper))
                        {
                            storageHelpers.Add(symbol);
                            plans.Add(storageHelper!);
                            continue;
                        }
                        if (NeoClrPayloadDeclaration.TryCreate(options, symbol, declaration, out var payloadHelper))
                        {
                            payloadHelpers.Add(symbol);
                            plans.Add(payloadHelper!);
                            continue;
                        }
                        if (NeoClrForeignDeclaration.TryCreate(options, symbol, declaration, out var foreign, out var import))
                        {
                            foreignImports.Add(symbol, import!);
                            plans.Add(foreign!);
                            continue;
                        }
                        if (!NeoClrRuntimeServiceDeclaration.TryCreate(options.CoreLibrary, symbol, declaration, out var service))
                            throw Unsupported("extern requires a supported NativeImport attribute or an internal neoCLR.Runtime MethodImpl(InternalCall) service");
                        runtimeServices.Add(symbol);
                        plans.Add(service!);
                        continue;
                    }
                    if ((declaration.Body is null && declaration.ExpressionBody is null) ||
                        declaration.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.AsyncKeyword or SyntaxKind.UnsafeKeyword)))
                        throw Unsupported("only top-level functions with block or expression bodies");
                    var plan = GetPlan(symbol);
                    plans.Add(plan);
                }
                else if (member is ConstDeclarationSyntax constant)
                {
                    if (constant.AttributeLists.Count != 0 || constant.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword)))
                        throw Unsupported("public/internal assembly-level Double constants without attributes");
                    foreach (var variable in constant.Declaration.Declarators)
                    {
                        if (model.GetDeclaredSymbol(variable) is not IFieldSymbol { IsConst: true, Type.SpecialType: SpecialType.System_Double } field ||
                            field.GetConstantValue() is not double value || !double.IsFinite(value))
                            throw Unsupported("finite assembly-level Double constants");
                        assemblyConstants.Add(field);
                    }
                }
                else if (member is ExtensionDeclarationSyntax extension)
                {
                    if (extension.AttributeLists.Count != 0 || extension.ConstraintClauses.Count != 0 ||
                        model.GetDeclaredSymbol(extension) is not INamedTypeSymbol extensionSymbol ||
                        !SourceTypePlan.TryCreate(extensionSymbol, out var extensionPlan, bodyCapabilities))
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
                        if (extensionMember is IndexerDeclarationSyntax extensionIndexer &&
                            extensionIndexer.AttributeLists.Count == 0 &&
                            model.GetDeclaredSymbol(extensionIndexer) is IPropertySymbol { IsExtensionProperty: true, IsIndexer: true } indexer)
                        {
                            if (indexer.SetMethod is { MethodKind: MethodKind.InitOnly })
                                throw Unsupported("ordinary extension indexer setter");
                            if (indexer.GetMethod is { } getter) plans.Add(GetPlan(getter));
                            if (indexer.SetMethod is { } setter) plans.Add(GetPlan(setter));
                            continue;
                        }
                        if (extensionMember is MethodDeclarationSyntax asyncExtension &&
                            model.GetDeclaredSymbol(asyncExtension) is IMethodSymbol { IsAsync: true })
                            throw Unsupported("native async state-machine emission");
                        if (extensionMember is not MethodDeclarationSyntax method ||
                            (method.Body is null && method.ExpressionBody is null) ||
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
                        !SourceTypePlan.TryCreate(enumSymbol, out var enumPlan, bodyCapabilities))
                        throw Unsupported("top-level public/internal Int32 enum");
                    if (enumSymbol.GetAttributes().Any(IsFlagsAttribute)) flagsEnums.Add(enumSymbol);
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
                        if (!SourceTypePlan.TryCreate(unionType.Symbol, out var unionTypePlan, bodyCapabilities))
                            throw Unsupported("union type contract: " + unionType.Symbol.ToDisplayString());
                        declaredTypes.TryAdd(unionType.Symbol, unionTypePlan!);
                        foreach (var field in unionType.Fields)
                        {
                            if (field.IsStatic || field.IsConst || field.RefKind != RefKind.None ||
                                !CallableSignature.TryType(field.Type, false, out _, bodyCapabilities))
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
                            if (!SourceCallablePlan.TryCreate(method, out var unionCallable, bodyCapabilities, unionSyntax))
                                throw Unsupported("union callable contract: " + method.ToDisplayString());
                            plans.Add(unionCallable!);
                            if (!unionCallable!.TryLowerBody(compilation, IsConsoleCall, out _, out var unionFailure, bodyCapabilities))
                                throw new UnsupportedInputException("union body " + method.Name + ": " + unionFailure!.Detail, unionFailure.Syntax.GetLocation());
                        }
                    }
                    // The full declaration graph, including uncalled synthesized members,
                    // is emitted with the union attributes below.
                }
                else if (member is RecordDeclarationSyntax recordSyntax)
                {
                    if (model.GetDeclaredSymbol(recordSyntax) is not SourceNamedTypeSymbol recordSymbol ||
                        recordSyntax.Members.Count != 0 ||
                        !SourceTypePlan.TryCreate(recordSymbol, out var recordPlan, bodyCapabilities))
                        throw Unsupported("positional records without additional members");
                    declaredTypes.TryAdd(recordSymbol, recordPlan!);
                    foreach (var field in recordSymbol.GetMembers().OfType<IFieldSymbol>())
                    {
                        if (field.IsStatic || !CallableSignature.TryType(field.Type, false, out _, bodyCapabilities))
                            throw Unsupported("record field contract: " + field.ToDisplayString());
                        storageFields.Add(field);
                    }
                    foreach (var property in recordSymbol.GetMembers().OfType<SourcePropertySymbol>())
                    {
                        if (property.GetMethod is null || property.SetMethod?.MethodKind is not (MethodKind.InitOnly or MethodKind.PropertySet) || property.EmitAsFieldOnly)
                            throw Unsupported("positional val or var components");
                        properties.Add(property);
                    }
                    foreach (var method in recordSymbol.GetMembers().OfType<IMethodSymbol>())
                    {
                        // The positional-storage capability does not export record equality,
                        // hashing or formatting. Calls to those source helpers are rejected.
                        if (method.MethodKind is not (MethodKind.Constructor or MethodKind.PropertyGet or MethodKind.PropertySet or MethodKind.InitOnly) && method.Name != "Deconstruct") continue;
                        if (!SourceCallablePlan.TryCreate(method, out var recordCallable, bodyCapabilities, recordSyntax))
                            throw Unsupported("record callable contract: " + method.ToDisplayString());
                        plans.Add(recordCallable!);
                    }
                }
                else if (member is InterfaceDeclarationSyntax interfaceSyntax)
                {
                    if (model.GetDeclaredSymbol(interfaceSyntax) is not INamedTypeSymbol interfaceSymbol ||
                        !SourceInterfacePlan.TryCreate(interfaceSymbol, bodyCapabilities, out var interfacePlan))
                        throw Unsupported("only invariant owned interfaces with public abstract method contracts");
                    interfaces.Add(interfacePlan!);
                }
                else if (member is TypeDeclarationSyntax type && type is ClassDeclarationSyntax or StructDeclarationSyntax)
                {
                    var typeSymbol = model.GetDeclaredSymbol(type) as INamedTypeSymbol ?? throw Unsupported("type symbol unavailable");
                    if (type.ParameterList is not null ||
                        type.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.StaticKeyword or SyntaxKind.PartialKeyword or SyntaxKind.OpenKeyword or SyntaxKind.SealedKeyword or SyntaxKind.AbstractKeyword)))
                        throw Unsupported("only public or internal static/root classes or value types without additional contracts");
                    if (!SourceTypePlan.TryCreate(typeSymbol, out var typePlan, bodyCapabilities))
                        throw Unsupported("supported public/internal static classes, root classes or unconstrained value types: " + typeSymbol.ToDisplayString());
                    // Partial declarations share one semantic identity and one metadata definition.
                    // Still validate every part and collect all of its members.
                    declaredTypes.TryAdd(typeSymbol, typePlan!);
                    foreach (var typeMember in type.Members)
                    {
                        diagnosticSyntax = typeMember;
                        if (typeMember is ClassDeclarationSyntax or StructDeclarationSyntax) continue;
                        if (typeMember is ConstDeclarationSyntax classConstant)
                        {
                            if (classConstant.AttributeLists.Count != 0 || classConstant.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword)))
                                throw Unsupported("class constants with public/internal/private visibility and no attributes");
                            foreach (var variable in classConstant.Declaration.Declarators)
                            {
                                if (model.GetDeclaredSymbol(variable) is not IFieldSymbol { IsConst: true, Type.SpecialType: SpecialType.System_Int32 } literal || literal.GetConstantValue() is not int)
                                    throw Unsupported("Int32 class constants");
                                literalFields.Add(literal);
                            }
                            continue;
                        }
                        if (typeMember is FieldDeclarationSyntax fieldSyntax)
                        {
                            if (fieldSyntax.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword or SyntaxKind.ReadonlyKeyword or SyntaxKind.StaticKeyword)))
                                throw Unsupported("only public/internal/private fields with target-supported storage types");
                            foreach (var variable in fieldSyntax.Declaration.Declarators)
                            {
                                diagnosticSyntax = variable;
                                if (model.GetDeclaredSymbol(variable) is not IFieldSymbol { IsConst: false, RefKind: RefKind.None } field ||
                                    field.DeclaredAccessibility is not (Accessibility.Public or Accessibility.Internal or Accessibility.Private) ||
                                    !CallableSignature.TryType(field.Type, false, out _, bodyCapabilities))
                                    throw Unsupported("only public/internal/private fields with target-supported storage types");
                                storageFields.Add(field);
                            }
                            continue;
                        }
                        if (!typeSymbol.IsStatic && typeMember is IndexerDeclarationSyntax indexerSyntax)
                        {
                            if (indexerSyntax.ExplicitInterfaceSpecifier is not null || indexerSyntax.Initializer is not null ||
                                indexerSyntax.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword)) ||
                                model.GetDeclaredSymbol(indexerSyntax) is not SourcePropertySymbol { IsStatic: false, IsIndexer: true } indexer ||
                                !CallableSignature.TryType(indexer.Type, false, out _, bodyCapabilities))
                                throw Unsupported("only implemented root-class indexers with supported value types");
                            if (indexerSyntax.AccessorList is { } indexerAccessors && indexerAccessors.Accessors.Any(a =>
                                a.Kind is not (SyntaxKind.GetAccessorDeclaration or SyntaxKind.SetAccessorDeclaration) ||
                                (a.Body is null && a.ExpressionBody is null) ||
                                a.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword))))
                                throw Unsupported("only implemented indexer get/set accessors");
                            properties.Add(indexer);
                            if (indexer.GetMethod is { } indexGet) plans.Add(GetPlan(indexGet));
                            if (indexer.SetMethod is { } indexSet) plans.Add(GetPlan(indexSet));
                            continue;
                        }
                        if (typeMember is PropertyDeclarationSyntax propertySyntax)
                        {
                            if (propertySyntax.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword or SyntaxKind.StaticKeyword)) ||
                                model.GetDeclaredSymbol(propertySyntax) is not SourcePropertySymbol property ||
                                !CallableSignature.TryType(property.Type, false, out _, bodyCapabilities))
                                throw Unsupported("only supported properties/storage");
                            if (propertySyntax.AccessorList is { } accessorList && accessorList.Accessors.Any(a =>
                                a.Kind is not (SyntaxKind.GetAccessorDeclaration or SyntaxKind.SetAccessorDeclaration or SyntaxKind.InitAccessorDeclaration) ||
                                (a.Body is null && a.ExpressionBody is null && property.BackingField is null) ||
                                a.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword))))
                                throw Unsupported("only implemented get/set/init accessors without additional contracts");
                            if (property.BackingField is { } backingField) storageFields.Add(backingField);
                            if (property.EmitAsFieldOnly) continue;
                            properties.Add(property);
                            if (property.GetMethod is { } get) plans.Add(GetPlan(get));
                            if (property.SetMethod is { } set) plans.Add(GetPlan(set));
                            continue;
                        }
                        if (typeMember is ConstructorDeclarationSyntax constructor)
                        {
                            if ((constructor.Body is null && constructor.ExpressionBody is null) ||
                                constructor.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword or SyntaxKind.ProtectedKeyword or SyntaxKind.StaticKeyword)))
                                throw Unsupported("only explicit root constructors with a block or expression body");
                            plans.Add(GetPlan((IMethodSymbol)model.GetDeclaredSymbol(constructor)!));
                            continue;
                        }
                        if (typeMember is ConversionOperatorDeclarationSyntax conversion && (conversion.Body is not null || conversion.ExpressionBody is not null) &&
                            conversion.Modifiers.All(m => m.Kind is SyntaxKind.PublicKeyword or SyntaxKind.StaticKeyword) &&
                            model.GetDeclaredSymbol(conversion) is IMethodSymbol { IsStatic: true, MethodKind: MethodKind.Conversion } conversionMethod)
                        {
                            plans.Add(GetPlan(conversionMethod));
                            continue;
                        }
                        if (typeMember is OperatorDeclarationSyntax op && (op.Body is not null || op.ExpressionBody is not null) &&
                            op.Modifiers.All(m => m.Kind is SyntaxKind.PublicKeyword or SyntaxKind.StaticKeyword))
                        {
                            plans.Add(GetPlan((IMethodSymbol)model.GetDeclaredSymbol(op)!));
                            continue;
                        }
                        if (typeMember is MethodDeclarationSyntax asyncMember &&
                            model.GetDeclaredSymbol(asyncMember) is IMethodSymbol { IsAsync: true } &&
                            compilation.Options.MetadataImportOptions?.AsyncAssemblyName is null)
                            throw Unsupported("explicit native async provider required");
                        if (typeMember is MethodDeclarationSyntax externalMethod &&
                            model.GetDeclaredSymbol(externalMethod) is IMethodSymbol { IsExtern: true } externalSymbol)
                        {
                            if (!NeoClrForeignDeclaration.TryCreate(options, externalSymbol, externalMethod, out var foreign, out var import))
                                throw Unsupported("extern static method with a supported NativeImport attribute");
                            foreignImports.Add(externalSymbol, import!);
                            plans.Add(foreign!);
                            continue;
                        }
                        if (typeMember is not MethodDeclarationSyntax method || (method.Body is null && method.ExpressionBody is null && !method.Modifiers.Any(m => m.Kind == SyntaxKind.AbstractKeyword)) ||
                            method.ExplicitInterfaceSpecifier is not null ||
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
            if (primitive is PrimitiveType.RuntimeTypeHandle or PrimitiveType.Value)
            {
                if (!symbol.IsValueType || symbol.Arity != 0 || symbol.ContainingType is not null || fieldsForType.Length != 0 ||
                    plans.Any(p => SymbolEqualityComparer.Default.Equals(p.Symbol.ContainingType, symbol) && p.Symbol.MethodKind is MethodKind.Constructor or MethodKind.StaticConstructor))
                    throw Unsupported("runtime handle/carrier implementation requires an empty value declaration without constructors");
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
        // Unit ownership is already explicit in RuntimeUnitContract. Preserve its
        // nominal CLI declaration while using intrinsic unit storage in native signatures.
        if (compilation.Options.RuntimeUnitContract is not null &&
            compilation.GetSpecialType(SpecialType.System_Unit) is UnitTypeSymbol { RuntimeRepresentation: { } unitRepresentation } &&
            declaredTypes.Keys.SingleOrDefault(t => SymbolEqualityComparer.Default.Equals(t, unitRepresentation)) is { } unitOwner)
        {
            if (!unitOwner.IsValueType || unitOwner.Arity != 0 || unitOwner.ContainingType is not null ||
                storageFields.Any(f => SymbolEqualityComparer.Default.Equals(f.ContainingType, unitOwner)) ||
                plans.Any(p => SymbolEqualityComparer.Default.Equals(p.Symbol.ContainingType, unitOwner) &&
                    p.Symbol.MethodKind is MethodKind.Constructor or MethodKind.StaticConstructor))
                throw Unsupported("selected native unit requires an empty value declaration without constructors");
            primitiveOwners.Add(unitOwner, PrimitiveType.Void);
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
        foreach (var type in declaredTypes.Values)
            if (type.Symbol.StaticConstructor is { } initializer && !plans.Any(p => SymbolEqualityComparer.Default.Equals(p.Symbol, initializer)))
                plans.Add(GetPlan(initializer));
        var prepared = new List<(SourceCallablePlan Plan, LinearMethodBody? Body)>();
        foreach (var plan in plans)
        {
            if (storageHelpers.Contains(plan.Symbol) || payloadHelpers.Contains(plan.Symbol) || runtimeServices.Contains(plan.Symbol) || foreignImports.ContainsKey(plan.Symbol) || plan.Symbol.IsAbstract)
            {
                prepared.Add((plan, null));
                continue;
            }
            var loweredPlan = plan;
            if (options.RuntimeAsyncHelperTypeName is { } helper && plan.Symbol.IsAsync)
                loweredPlan = NativeRuntimeAsyncLowerer.Rewrite(compilation, plan, helper);
            if (!loweredPlan.TryLowerBody(compilation, IsConsoleCall, out var body, out var failure, bodyCapabilities))
                throw new UnsupportedInputException(failure!.Detail, failure.Syntax.GetLocation());
            prepared.Add((loweredPlan, body!));
        }
        foreach (var machine in compilation.GetSynthesizedAsyncStateMachineTypes().ToArray())
        {
            if (!SourceTypePlan.TryCreate(machine, out var typePlan, bodyCapabilities))
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
                if (methodBody is null || !CallableSignature.TryCreate(method, out var signature, bodyCapabilities))
                    throw Unsupported("synthesized async method signature/body");
                var plan = new SourceCallablePlan(method, anchor, anchor, machine, method.MetadataName, signature, PreparedBody: methodBody);
                if (!plan.TryLowerBody(compilation, IsConsoleCall, out var body, out var failure, bodyCapabilities))
                    throw new UnsupportedInputException(failure!.Detail, failure.Syntax.GetLocation());
                prepared.Add((plan, body));
            }
        }
        var closureCaptures = new Dictionary<IMethodSymbol, ITypeSymbol[]>(SymbolEqualityComparer.Default);
        var closureParameters = new Dictionary<IMethodSymbol, ITypeParameterSymbol[]>(SymbolEqualityComparer.Default);
        var lambdaSymbols = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);
        for (var index = 0; index < prepared.Count; index++)
            foreach (var (function, syntax) in prepared[index].Body?.Functions ?? [])
            {
                var symbol = (IMethodSymbol)function.Symbol!;
                if (!lambdaSymbols.Add(symbol)) continue;
                if (!CallableSignature.TryCreate(symbol, out var signature, bodyCapabilities)) throw Unsupported("unsupported Function body signature");
                var captures = function.CapturedVariables.Select(capture => capture switch
                {
                    INamedTypeSymbol selfType => selfType,
                    ILocalSymbol local => local.Type,
                    IParameterSymbol parameter => parameter.Type,
                    _ => throw Unsupported("unsupported closure capture")
                }).ToArray();
                var context = prepared[index].Plan.Symbol;
                var parameters = closureParameters.TryGetValue(context, out var inherited) ? inherited : (context.ContainingType is { } contextOwner ? NeoClrTypeMapper.OwnerParameters(contextOwner) : []).Concat(context.TypeParameters).ToArray();
                if (parameters.Length != 0) closureParameters.Add(symbol, parameters);
                if (captures.Length != 0 || parameters.Length != 0) closureCaptures.Add(symbol, captures);
                signature = signature with { IsInstance = captures.Length != 0 || parameters.Length != 0 };
                if (!LinearMethodBody.TryLower(symbol, compilation.GetSemanticModel(syntax.SyntaxTree), syntax, IsConsoleCall,
                    out var body, out var failure, bodyCapabilities, function, forceFunctionInstance: parameters.Length != 0))
                    throw new UnsupportedInputException(failure!.Detail, failure.Syntax.GetLocation());
                var plan = new SourceCallablePlan(symbol, syntax, syntax, null, "$function$" + lambdaSymbols.Count, signature, IsSynthesizedStatic: true);
                prepared.Add((plan, body!));
            }
        // Materialize definitions only after all source declarations and body capabilities pass.
        // Every definition exists before reference resolution or method-body emission.
        var assembly = new AssemblyBuilder(options.Identity, options.CoreLibrary);
        foreach (var tree in compilation.SyntaxTrees)
            foreach (var declaration in tree.GetRoot().DescendantNodes().OfType<BaseNamespaceDeclarationSyntax>())
                if (compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration) is INamespaceSymbol { IsModule: true } module)
                    assembly.DefineModule(module.ToMetadataName() ?? "");
        foreach (var constant in assemblyConstants)
            assembly.AddConstant(new AssemblyConstantDefinition(constant.ContainingNamespace?.ToMetadataName() ?? "", constant.Name,
                (double)constant.GetConstantValue()!, constant.DeclaredAccessibility == Accessibility.Public ? MethodVisibility.Public : MethodVisibility.Internal));
        foreach (var (_, binding) in dependencies)
            if (binding.NativeImplementation is { } implementation)
                assembly.BindNativeLibrary(binding.Definition, implementation, binding.CoreLibrary);
        var owners = new Dictionary<INamedTypeSymbol, NeoClrCallableDefinitionBuilder>(SymbolEqualityComparer.Default);
        var nativeTypes = new Dictionary<INamedTypeSymbol, TypeBuilder>(SymbolEqualityComparer.Default);
        var importedTypes = new Dictionary<INamedTypeSymbol, ImportedTypeReference>(SymbolEqualityComparer.Default);
        var compositionTypes = new Dictionary<ITypeSymbol, SignatureType>(SymbolEqualityComparer.Default);
        {
            foreach (var dependency in dependencies)
            {
                if (dependency.Symbol is not NativeAssemblySymbol nativeAssembly || !nativeAssembly.Reference.HasStructuralCompositions) continue;
                foreach (var definition in nativeAssembly.Reference.Definition.MainModule.Functions)
                {
                    if (!definition.TryGetSignature(out var raw) || !NeoClrMetadataReference.ContainsComposition(raw!.ReturnType)) continue;
                    var imported = assembly.ImportExperimentalCompositionReference(definition, dependency.Dependency.CoreLibrary,
                        new NativeCompositionResolver(options.Dependencies));
                    assembly.EnableExperimentalCompositionStorage(imported);
                    var symbol = ImportedFunctions(nativeAssembly.GlobalNamespace).OfType<NativeMethodSymbol>().Single(m => m.NativeMetadataToken == definition.MetadataToken);
                    RegisterComposition(symbol.ReturnType, imported.Signature.ReturnType);
                }
            }
        }
        static IEnumerable<IMethodSymbol> ImportedFunctions(INamespaceSymbol scope)
        {
            foreach (var method in scope.GetMembers().OfType<IMethodSymbol>()) yield return method;
            foreach (var child in scope.GetMembers().OfType<INamespaceSymbol>())
                foreach (var method in ImportedFunctions(child)) yield return method;
        }
        void RegisterComposition(ITypeSymbol symbol, SignatureType signature)
        {
            if (signature.Composition is not null)
            {
                if (compositionTypes.TryGetValue(symbol, out var previous) && previous != signature)
                    throw Unsupported("ambiguous normalized composition storage identity");
                compositionTypes[symbol] = signature;
            }
            else if (symbol is INamedTypeSymbol wrapper && signature.ImportedType is { } imported)
                for (var index = 0; index < wrapper.TypeArguments.Length; index++)
                    RegisterComposition(wrapper.TypeArguments[index], imported.TypeArguments[index]);
        }
        var definingClasses = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);
        NeoClrTypeDefinitionBuilder typeDefinitions = null!;
        typeDefinitions = new NeoClrTypeDefinitionBuilder(assembly, type => nativeTypes[type], type => ImportClassBase((INamedTypeSymbol)type),
            type => NeoClrTypeMapper.Map(type, owner =>
            {
                if (!nativeTypes.ContainsKey(owner) && declaredTypes.TryGetValue(owner, out var plan)) DefineClass(plan);
                return nativeTypes[owner];
            }, ImportExternalType));
        SignatureType ImportClassBase(INamedTypeSymbol type)
        {
            var reference = ImportExternalType(type);
            type = (INamedTypeSymbol)type.OriginalDefinition;
            var definition = importedTypes[type];
            if (type.BaseType is { SpecialType: not SpecialType.System_Object } parent) _ = ImportClassBase(parent);
            var members = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);
            foreach (var member in type.GetMembers())
            {
                if (member is IMethodSymbol method) members.Add(method);
                else if (member is IPropertySymbol property)
                {
                    if (property.GetMethod is { } getter) members.Add(getter);
                    if (property.SetMethod is { } setter) members.Add(setter);
                }
            }
            assembly.CompleteClassBaseReference(definition, members
                .Where(m => !m.IsStatic && m.MethodKind != MethodKind.Constructor && m.DeclaredAccessibility == Accessibility.Public)
                .Select(Import), type.GetMembers().OfType<IFieldSymbol>().Count(f => !f.IsStatic));
            return reference;
        }

        SignatureType ImportExternalType(ITypeSymbol symbol)
        {
            if (symbol is ITupleTypeSymbol tuple)
                return assembly.CreateExperimentalTuple(tuple.TupleElements.Select(e => NeoClrTypeMapper.Map(e.Type, owner => nativeTypes[owner], ImportExternalType)).ToArray(), tuple.TupleElements.Select(e => (string?)e.Name).ToArray());
            // A normalized nominal symbol cannot identify which original compound
            // contract belongs in synthesized storage. Preserve call-only transport,
            // but reject storage instead of silently erasing that descriptor.
            if (compositionTypes.ContainsKey(symbol) && symbol is not (IStructuralUnionTypeSymbol or IIntersectionTypeSymbol))
                throw Unsupported("normalized composition storage requires an unambiguous structural descriptor");
            if (symbol is IStructuralUnionTypeSymbol or IIntersectionTypeSymbol)
            {
                if (compositionTypes.TryGetValue(symbol, out var signature)) return signature;
                if (!compilation.UsesStructuralCompositions)
                    throw Unsupported("composition storage requires an exact imported contract or native structural authoring");
                var bounds = symbol is IStructuralUnionTypeSymbol union ? union.ConstituentTypes : ((IIntersectionTypeSymbol)symbol).ConstituentTypes;
                signature = assembly.CreateExperimentalReferenceComposition(
                    symbol is IStructuralUnionTypeSymbol ? ReferenceCompositionKind.Union : ReferenceCompositionKind.Intersection,
                    bounds.Select(bound => NeoClrTypeMapper.Map(bound, owner => nativeTypes[owner], ImportExternalType)).ToArray());
                compositionTypes[symbol] = signature;
                return signature;
            }
            var type = (INamedTypeSymbol)symbol;
            // The configured semantic marker is transport only; native metadata keeps Self.
            if (RuntimeSelfTypes.IsSelf(compilation, type)) return SignatureType.Self;
            // Imported callable substitutions can retain the bootstrap's inhabited
            // Void token. Value positions must use this compilation's explicit unit
            // owner; no-result calls are mapped before reaching this nominal path.
            if (type.SpecialType == SpecialType.System_Void &&
                compilation.Options.RuntimeUnitContract is not null &&
                compilation.ResolveRuntimeUnitType() is { } selectedUnit &&
                !SymbolEqualityComparer.Default.Equals(type, selectedUnit) &&
                SymbolEqualityComparer.Default.Equals(type.ContainingAssembly,
                    compilation.GetSpecialType(SpecialType.System_Void).ContainingAssembly))
                return NeoClrTypeMapper.Map(selectedUnit, owned => nativeTypes[owned], ImportExternalType);
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
                    if (original.TypeKind == TypeKind.Interface)
                        assembly.SetInterfaceVariance(imported, original.TypeParameters.Select(p => p.Variance switch
                        {
                            VarianceKind.Out => GenericVariance.Covariant,
                            VarianceKind.In => GenericVariance.Contravariant,
                            _ => GenericVariance.Invariant
                        }));
                }
                else
                {
                    if (original.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null })
                        throw Unsupported("native type requires a supported symbol-only emission contract: " + original.ToFullyQualifiedMetadataName());
                    var name = original.ToFullyQualifiedMetadataName();
                    var candidates = binding.Definition.MainModule.Types.Where(t => MatchesType(t, original)).Take(2).ToArray();
                    if (candidates.Length != 1 || candidates[0].GenericArity != original.Arity || candidates[0].IsValueType != original.IsValueType)
                        throw Unsupported("dependency type unavailable or ambiguous: " + name);
                    imported = assembly.ImportReference(candidates[0], binding.CoreLibrary);
                }
                importedTypes.Add(original, imported);
                if (original.TypeKind == TypeKind.Class && !original.IsStatic && original.ContainingType is null)
                    assembly.DeclareDefaultConstructor(imported, !original.IsAbstract && original.InstanceConstructors.Any(c => c.Parameters.IsEmpty && c.DeclaredAccessibility == Accessibility.Public));
                foreach (var parameter in original.TypeParameters)
                {
                    var flags = TypeParameterConstraints.None;
                    if (parameter.ConstraintKind.HasFlag(TypeParameterConstraintKind.ReferenceType)) flags |= TypeParameterConstraints.ReferenceType;
                    if (parameter.ConstraintKind.HasFlag(TypeParameterConstraintKind.ValueType)) flags |= TypeParameterConstraints.ValueType | TypeParameterConstraints.DefaultConstructor;
                    if (parameter.ConstraintKind.HasFlag(TypeParameterConstraintKind.Constructor)) flags |= TypeParameterConstraints.DefaultConstructor;
                    if (parameter.ConstraintKind.HasFlag(TypeParameterConstraintKind.FunctionType)) flags |= TypeParameterConstraints.FunctionType;
                    if (flags != TypeParameterConstraints.None) assembly.SetSpecialConstraints(imported, parameter.Ordinal, flags);
                    foreach (var bound in parameter.ConstraintTypes)
                        assembly.AddBaseTypeConstraint(imported, parameter.Ordinal, ImportExternalType((INamedTypeSymbol)bound).ImportedType!);
                }
                if (original.SpecialType == SpecialType.System_Object &&
                    original.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null })
                    assembly.SetNativeObjectRoot(imported);
                if (original.TypeKind == TypeKind.Class && original.BaseType is { SpecialType: not SpecialType.System_Object } classBase)
                    assembly.DeclareClassBase(imported, ImportExternalType(classBase).ImportedType!);
                if (IsRuntimeErasedValue(original) && original.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null })
                    assembly.SetNativePrimitive(imported, PrimitiveType.Value);
                if (original.SpecialType == SpecialType.System_Char && original.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null })
                    assembly.SetNativeGrapheme(imported);
                if (HasNativePrimitiveStorage(original.SpecialType) && original.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null })
                    assembly.SetNativePrimitive(imported, NeoClrTypeMapper.Instance.Map(Enum.Parse<EmissionPrimitiveType>(original.SpecialType.ToString()[7..])));
                if (compilation.Options.RuntimeUnitContract is not null &&
                    compilation.GetSpecialType(SpecialType.System_Unit) is UnitTypeSymbol { RuntimeRepresentation: { } importedUnit } &&
                    SymbolEqualityComparer.Default.Equals(original, importedUnit) &&
                    original.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null })
                    assembly.SetNativePrimitive(imported, PrimitiveType.Void);
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
        if (compilation.Options.MetadataImportOptions?.ObjectAssemblyName is not null)
        {
            var root = compilation.GetSpecialType(SpecialType.System_Object);
            _ = ImportExternalType(root);
            // Supply the full selected root contract, independently of which slots bodies call.
            foreach (var name in new[] { "ToString", "Equals", "GetHashCode" })
            {
                var slots = root.GetMembers(name).OfType<IMethodSymbol>().Where(m =>
                    !m.IsStatic && m.IsVirtual && !m.IsAbstract && !m.IsOverride && m.Arity == 0 &&
                    m.DeclaredAccessibility == Accessibility.Public).ToArray();
                if (slots.Length != 1) throw Unsupported("selected Object root requires one concrete virtual slot: " + name);
                _ = Import(slots[0]);
            }
            if (root.ContainingAssembly.GetTypeByMetadataName("System.Value") is INamedTypeSymbol value && IsRuntimeErasedValue(value))
                _ = ImportExternalType(value);
        }
        void AdmitExternalConstraint(INamedTypeSymbol type)
        {
            _ = ImportExternalType(type);
            var original = (INamedTypeSymbol)type.OriginalDefinition;
            var binding = dependencies.Single(d => SymbolEqualityComparer.Default.Equals(d.Symbol, original.ContainingAssembly)).Dependency;
            var snapshot = binding.Reference is NeoClrMetadataReference native ? native.Definition : binding.Definition;
            var definition = snapshot.MainModule.Types.Single(t => MatchesType(t, original));
            _ = assembly.ImportReference(definition, binding.CoreLibrary);
        }
        var functions = new NeoClrCallableDefinitionBuilder(assembly, resolveClass: type => nativeTypes[type], resolveExternal: ImportExternalType, structuralConstraints: true, admitExternalConstraint: AdmitExternalConstraint);
        void DefineClass(SourceTypePlan type)
        {
            if (nativeTypes.ContainsKey(type.Symbol)) return;
            if (!definingClasses.Add(type.Symbol)) throw Unsupported("cyclic class base definitions are unsupported");
            if (type.ClassBase is { } parent && declaredTypes.TryGetValue((INamedTypeSymbol)parent.OriginalDefinition, out var basePlan)) DefineClass(basePlan);
            if (type.IsEnum && type.Symbol.BaseType is { SpecialType: SpecialType.System_Enum } enumParent &&
                declaredTypes.TryGetValue(enumParent, out var enumPlan)) DefineClass(enumPlan);
            if (type.IsValueType && (primitiveOwners.ContainsKey(type.Symbol) || SymbolEqualityComparer.Default.Equals(type.Symbol, graphemeOwner)) &&
                type.Symbol.BaseType is { SpecialType: SpecialType.System_ValueType } valueParent &&
                declaredTypes.TryGetValue(valueParent, out var valuePlan)) DefineClass(valuePlan);
            if (type.MetadataOwner is { } container) DefineClass(declaredTypes[container]);
            var definition = type.Define(typeDefinitions);
            if (primitiveOwners.TryGetValue(type.Symbol, out var primitive)) definition.SetNativePrimitive(primitive);
            if (SymbolEqualityComparer.Default.Equals(type.Symbol, graphemeOwner)) definition.SetNativeGrapheme();
            if (flagsEnums.Contains(type.Symbol)) definition.SetEnumFlags();
            if (!type.IsValueType && !type.IsStatic && !type.IsClosedHierarchy && type.Symbol.IsClosed)
                definition.SetSealedClass();
            nativeTypes.Add(type.Symbol, definition);
            typeDefinitions.BindBase(type, definition);
            definingClasses.Remove(type.Symbol);
            var scope = type.Symbol.OriginalDefinition is SynthesizedAsyncStateMachineTypeSymbol ? NeoClrTypeMapper.OwnerParameters(type.Symbol) : [];
            owners.Add(type.Symbol, new(assembly, definition, type => nativeTypes[type],
                named => ScopedExternal(named, scope), p => ScopedParameter(p, scope), structuralConstraints: true, admitExternalConstraint: AdmitExternalConstraint));
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
            for (int i = 0; i < symbol.TypeParameters.Length; i++)
                nativeInterfaces[symbol].SetVariance(i, symbol.TypeParameters[i].Variance switch
                {
                    VarianceKind.Out => GenericVariance.Covariant,
                    VarianceKind.In => GenericVariance.Contravariant,
                    _ => GenericVariance.Invariant
                });
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
                    if (pair.Key.Parameters[i].IsVarParams) SetParameterArray(pair.Value, i);
                }
                interfaceMethods.Add(pair.Key, pair.Value);
            }
            foreach (var property in contract.Properties)
            {
                var emitted = definition.AddProperty(property.Symbol.MetadataName, NeoClrTypeMapper.Map(property.Type, type => nativeTypes[type], ImportExternalType),
                    property.Symbol.GetMethod is { } get ? contractMethods[get] : null,
                    property.Symbol.SetMethod is { } set ? contractMethods[set] : null);
                attributeProperties.Add((property.Symbol, emitted.Definition));
            }
        }
        foreach (var type in declaredTypes.Values)
            nativeTypes[type.Symbol].SetBeforeFieldInit(type.Symbol.StaticConstructor?.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax() is not ConstructorDeclarationSyntax);
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
            foreach (var parameter in NeoClrTypeMapper.OwnerParameters(type.Symbol))
            {
                var ordinal = Array.FindIndex(NeoClrTypeMapper.OwnerParameters(type.Symbol), p => SymbolEqualityComparer.Default.Equals(p, parameter));
                foreach (var bound in parameter.ConstraintTypes)
                    nativeTypes[type.Symbol].AddBaseTypeConstraint(ordinal, nativeTypes[(INamedTypeSymbol)bound]);
                var flags = TypeParameterConstraints.None;
                if (parameter.ConstraintKind.HasFlag(TypeParameterConstraintKind.ReferenceType)) flags |= TypeParameterConstraints.ReferenceType;
                if (parameter.ConstraintKind.HasFlag(TypeParameterConstraintKind.ValueType)) flags |= TypeParameterConstraints.ValueType | TypeParameterConstraints.DefaultConstructor;
                if (parameter.ConstraintKind.HasFlag(TypeParameterConstraintKind.Constructor)) flags |= TypeParameterConstraints.DefaultConstructor;
                if (parameter.ConstraintKind.HasFlag(TypeParameterConstraintKind.FunctionType)) flags |= TypeParameterConstraints.FunctionType;
                nativeTypes[type.Symbol].SetSpecialConstraints(ordinal, flags);
            }
        foreach (var literal in literalFields)
            nativeTypes[literal.ContainingType!].Definition.Fields.Add(new FieldDefinition(literal.MetadataName,
                (ushort)(0x8050 | (literal.DeclaredAccessibility switch { Accessibility.Public => 6, Accessibility.Internal => 3, _ => 1 })),
                PrimitiveType.Int32, (int)literal.GetConstantValue()!));
        var fields = new Dictionary<IFieldSymbol, FieldBuilder>(SymbolEqualityComparer.Default);
        var importedFields = new Dictionary<IFieldSymbol, NeoClrFieldReference>(SymbolEqualityComparer.Default);
        foreach (var field in storageFields)
        {
            if (primitiveFields.ContainsKey(field) || SymbolEqualityComparer.Default.Equals(field, graphemeField)) continue;
            CallableSignature.TryType(field.Type, false, out var fieldType, bodyCapabilities);
            var fieldScope = field.ContainingType is { OriginalDefinition: SynthesizedAsyncStateMachineTypeSymbol } fieldOwner ? NeoClrTypeMapper.OwnerParameters(fieldOwner) : [];
            var storageType = NeoClrTypeMapper.Map(fieldType, type => nativeTypes[type], named => ScopedExternal(named, fieldScope), p => ScopedParameter(p, fieldScope));
            fields.Add(field, nativeTypes[field.ContainingType!].AddField(field.MetadataName, storageType, field.DeclaredAccessibility switch
            {
                Accessibility.Public => FieldVisibility.Public,
                Accessibility.Internal => FieldVisibility.Internal,
                Accessibility.Private => FieldVisibility.Private,
                _ => throw Unsupported("unsupported field visibility")
            }, isReadOnly: field.IsReadOnly, isStatic: field.IsStatic));
        }
        if (compilation.Options.RuntimeIterationContract is { ArrayShapeTypeName: { } arrayName } arrayContract &&
            arrayContract.AssemblyName == options.Identity.Name)
        {
            var arrayShape = declaredTypes.Keys.SingleOrDefault(t => t.ToFullyQualifiedMetadataName() == arrayName);
            if (arrayShape is null || !nativeTypes.TryGetValue(arrayShape, out var arrayDefinition))
                throw Unsupported("configured array backing declaration is absent from output");
            assembly.SetArrayBacking(arrayDefinition);
        }
        // Captured method parameters become parameters of synthesized closure/state owners.
        // Declaration imports retain their own scope; only caller constructions are remapped.
        SignatureType ScopedParameter(ITypeParameterSymbol parameter, ITypeParameterSymbol[] scope)
        {
            var index = Array.FindIndex(scope, item => SymbolEqualityComparer.Default.Equals(item, parameter) ||
                item.DeclaringTypeParameterOwner is SynthesizedAsyncStateMachineTypeSymbol machine &&
                SymbolEqualityComparer.Default.Equals(machine.AsyncMethod.TypeParameters[item.Ordinal], parameter));
            return index >= 0 ? SignatureType.TypeParameter(index)
                : parameter.DeclaringMethodParameterOwner is not null ? SignatureType.MethodParameter(parameter.Ordinal)
                : SignatureType.TypeParameter(parameter.Ordinal);
        }
        SignatureType ScopedExternal(ITypeSymbol symbol, ITypeParameterSymbol[] scope)
        {
            if (symbol is IStructuralUnionTypeSymbol or IIntersectionTypeSymbol) return ImportExternalType(symbol);
            var type = (INamedTypeSymbol)symbol;
            var normal = ImportExternalType(type);
            if (scope.Length == 0 || type.Arity == 0 || normal.ImportedType is null) return normal;
            return importedTypes[(INamedTypeSymbol)type.OriginalDefinition].MakeGenericInstance(type.TypeArguments
                .Select(t => NeoClrTypeMapper.Map(t, owner => nativeTypes[owner], named => ScopedExternal(named, scope), p => ScopedParameter(p, scope))).ToArray());
        }
        ITypeParameterSymbol[] emissionScope = [];
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
                var scope = closureParameters.GetValueOrDefault(plan.Symbol) ?? [];
                var lexicalOwner = plan.Symbol.ContainingType;
                var nativeOwner = lexicalOwner is not null && nativeTypes.TryGetValue(lexicalOwner, out var ownerDefinition) ? ownerDefinition : null;
                var parameters = scope.Select((p, index) => "T" + index);
                var frame = scope.Length != 0
                    ? nativeOwner is not null ? nativeOwner.AddNestedGenericClass(frameName, parameters, TypeVisibility.Internal)
                        : assembly.AddGenericClass("Raven.Generated", frameName, parameters, TypeVisibility.Internal)
                    : nativeOwner is not null ? nativeOwner.AddNestedClass(frameName, TypeVisibility.Internal)
                        : assembly.AddClass("", frameName, TypeVisibility.Internal);
                var captureFields = captures.Select((capture, index) => frame.AddField("capture" + index,
                    NeoClrTypeMapper.Map(capture, type => nativeTypes[type], named => ScopedExternal(named, scope), p => ScopedParameter(p, scope)), FieldVisibility.Private)).ToArray();
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
                    NeoClrTypeMapper.Map(signature.ReturnType, type => nativeTypes[type], named => ScopedExternal(named, scope), p => ScopedParameter(p, scope)),
                    signature.ParameterTypes.Select(type => NeoClrTypeMapper.Map(type, owner => nativeTypes[owner], named => ScopedExternal(named, scope), p => ScopedParameter(p, scope)))));
                closureFields.Add(plan.Symbol, captureFields);
                closureConstructors.Add(plan.Symbol, constructor);
            }
            else
            {
                var owner = plan.IsAssemblyFunction ? functions : owners[plan.TypeOwner!];
                definition = plan.Define(owner);
            }
            if (payloadHelpers.Contains(plan.Symbol))
            {
                var il = definition.GetILGenerator();
                il.LoadArgument(0);
                il.Emit(plan.Symbol.Name == "IsValue" ? OpCode.ValueIs : OpCode.ValueUnpack, SignatureType.MethodParameter(0));
                il.Return();
            }
            if (storageHelpers.Contains(plan.Symbol))
            {
                var il = definition.GetILGenerator();
                il.LoadArgument(0);
                il.ReserveArray(SignatureType.MethodParameter(0));
                il.Return();
            }
            if (runtimeServices.Contains(plan.Symbol)) definition.SetInternalCall();
            if (foreignImports.TryGetValue(plan.Symbol, out var foreignImport)) definition.SetNativeImport(foreignImport);
            definition.SetNullableAnnotation(-1, NullableAnnotationEmitter.Create(plan.Symbol.ReturnType));
            for (var i = 0; i < plan.Symbol.Parameters.Length; i++)
            {
                definition.SetNullableAnnotation(i, NullableAnnotationEmitter.Create(plan.Symbol.Parameters[i].Type));
                definition.SetParameterName(i, plan.Symbol.Parameters[i].Name);
                if (plan.Symbol.Parameters[i].IsVarParams) SetParameterArray(definition, i);
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
            CallableSignature.TryType(property.Type, false, out var propertyType, bodyCapabilities);
            var valueType = NeoClrTypeMapper.Map(propertyType, type => nativeTypes[type], ImportExternalType);
            var emitted = nativeTypes[property.ContainingType!].AddProperty(property.MetadataName, valueType,
                property.GetMethod is null ? null : definedMethods[property.GetMethod], property.SetMethod is null ? null : definedMethods[property.SetMethod], isInitOnly: property.SetMethod?.MethodKind == MethodKind.InitOnly);
            attributeProperties.Add((property, emitted.Definition));
        }
        CallableReferenceTable<NeoClrCallableReference> CreateReferences(ITypeParameterSymbol[] scope)
        {
            var references = new CallableReferenceTable<NeoClrCallableReference>(target =>
            {
                if (target.ContainingType is { } owner && NeoClrTypeMapper.OwnerArguments(owner).Length > 0 &&
                    owner.OriginalDefinition is not SourceNamedTypeSymbol { IsExtensionDeclaration: true })
                {
                    if (!definedMethods.TryGetValue(target.OriginalDefinition ?? target, out var definition))
                        return NeoClrCallableReference.Create(Import(target.OriginalDefinition ?? target).MakeConstructedReference(
                            NeoClrTypeMapper.OwnerArguments(owner).Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], named => ScopedExternal(named, scope), p => ScopedParameter(p, scope))),
                            target.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], named => ScopedExternal(named, scope), p => ScopedParameter(p, scope)))));
                    return NeoClrCallableReference.Create(definition.MakeConstructedReference(
                        NeoClrTypeMapper.OwnerArguments(owner).Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], named => ScopedExternal(named, scope), p => ScopedParameter(p, scope))),
                        target.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], named => ScopedExternal(named, scope), p => ScopedParameter(p, scope)))), (target.IsVirtual || target.IsOverride) && owner.IsReferenceType);
                }
                if (target.IsGenericMethod)
                {
                    foreach (var argument in target.TypeArguments.OfType<INamedTypeSymbol>().Where(t => t.SpecialType is not SpecialType.None && t.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null }))
                        _ = ImportExternalType(argument);
                    if (definedMethods.TryGetValue(target.OriginalDefinition ?? target, out var definition))
                        return NeoClrCallableReference.Create(definition.MakeGenericInstance(target.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], named => ScopedExternal(named, scope), p => ScopedParameter(p, scope))).ToArray()));
                    var arguments = target.TypeArguments.Select(t =>
                        NeoClrTypeMapper.Map(t, type => nativeTypes[type], named => ScopedExternal(named, scope), p => ScopedParameter(p, scope))).ToArray();
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
                if (!declaration.Plan.Symbol.IsGenericMethod && (declaration.Plan.Symbol.ContainingType is not { } declarationOwner || NeoClrTypeMapper.OwnerArguments(declarationOwner).Length == 0))
                    references.Declare(declaration.Plan.Symbol, NeoClrCallableReference.Create(declaration.Method, declaration.Plan.Symbol.IsVirtual && declaration.Plan.Symbol.ContainingType?.IsValueType == false));
            return references;
        }
        var references = CreateReferences([]);
        var defaultReferences = references;
        var defaultImportedFields = importedFields;
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
                var drainOwner = compilation.Options.MetadataImportOptions?.UseNativeMetadata == true
                    ? "System.Runtime.CompilerServices.RuntimeHelpers" : "System.Runtime.CompilerServices.RuntimeServices";
                var drain = compilation.GetTypeByMetadataName(drainOwner)?
                    .GetMembers("DrainEntryTasks").OfType<IMethodSymbol>().SingleOrDefault(m =>
                        m.IsStatic && m.Arity == 0 && m.Parameters.IsEmpty && m.DeclaredAccessibility == Accessibility.Public &&
                        m.ReturnType.SpecialType is SpecialType.System_Unit or SpecialType.System_Void)
                    ?? throw Unsupported("native async entry requires explicit DrainEntryTasks runtime binding");
                if (compilation.Options.MetadataImportOptions?.UseNativeMetadata == true &&
                    drain.ContainingAssembly?.Name != compilation.Options.MetadataImportOptions.AsyncAssemblyName)
                    throw Unsupported("native entry drain must belong to the selected async runtime library");
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
            emissionScope = closureParameters.GetValueOrDefault(current.Plan.Symbol) ??
                (current.Plan.Symbol.ContainingType is { OriginalDefinition: SynthesizedAsyncStateMachineTypeSymbol } machineOwner
                    ? NeoClrTypeMapper.OwnerParameters(machineOwner) : []);
            // Cached constructed references are only valid within their generic scope.
            references = emissionScope.Length == 0 ? defaultReferences : CreateReferences(emissionScope);
            importedFields = emissionScope.Length == 0 ? defaultImportedFields : new(SymbolEqualityComparer.Default);
            diagnosticSyntax = current.Plan.Syntax;
            current.Body.Emit(new NeoClrLinearMethodBuilder(current.Method.GetILGenerator(), (instruction, output) =>
            {
                diagnosticSyntax = instruction.Syntax;
                if (instruction.Kind is LinearInstructionKind.BaseConstructorCall or LinearInstructionKind.DirectInstanceCall)
                {
                    if (definedMethods.TryGetValue(instruction.Method!.OriginalDefinition, out var directTarget))
                    {
                        var symbol = instruction.Method;
                        SignatureType MapDirectType(ITypeSymbol type) => NeoClrTypeMapper.Map(type, owner => nativeTypes[owner],
                            named => ScopedExternal(named, emissionScope), parameter => ScopedParameter(parameter, emissionScope));
                        if (symbol.ContainingType is { } directOwner && NeoClrTypeMapper.OwnerArguments(directOwner) is { Length: > 0 } arguments)
                            output.Call(directTarget.MakeConstructedReference(arguments.Select(MapDirectType), symbol.TypeArguments.Select(MapDirectType)));
                        else output.Call(directTarget);
                    }
                    else if (instruction.Method.ContainingType is { } importedOwner && NeoClrTypeMapper.OwnerArguments(importedOwner) is { Length: > 0 } importedArguments)
                    {
                        SignatureType MapImportedType(ITypeSymbol type) => NeoClrTypeMapper.Map(type, owner => nativeTypes[owner],
                            named => ScopedExternal(named, emissionScope), parameter => ScopedParameter(parameter, emissionScope));
                        output.Call(Import(instruction.Method.OriginalDefinition).MakeConstructedReference(
                            importedArguments.Select(MapImportedType), instruction.Method.TypeArguments.Select(MapImportedType)));
                    }
                    else output.Emit(OpCode.Call, Import(instruction.Method));
                }
                else if (instruction.Kind == LinearInstructionKind.FunctionBind)
                {
                    var symbol = instruction.Method!;
                    if (!definedMethods.TryGetValue(symbol.OriginalDefinition, out var target) && !interfaceMethods.TryGetValue(symbol.OriginalDefinition, out target))
                        throw Unsupported("Function binding requires an owned target");
                    var genericClosure = closureParameters.GetValueOrDefault(symbol) ?? [];
                    var closureArguments = genericClosure.Select(p => ScopedParameter(p, emissionScope)).ToArray();
                    if (closureConstructors.TryGetValue(symbol, out var constructor))
                    {
                        if (closureArguments.Length == 0) output.NewObject(constructor);
                        else output.NewObject(constructor.MakeConstructedReference(closureArguments));
                    }
                    var shape = NeoClrTypeMapper.Map(instruction.Type!, type => nativeTypes[type], named => ScopedExternal(named, emissionScope), p => ScopedParameter(p, emissionScope));
                    void Bind(SignatureType bindingShape)
                    {
                        if (closureArguments.Length != 0)
                            output.BindFunction(bindingShape, target.MakeConstructedReference(closureArguments));
                        else if (symbol.ContainingType is { } owner && NeoClrTypeMapper.OwnerArguments(owner).Length > 0)
                            output.BindFunction(bindingShape, target.MakeConstructedReference(NeoClrTypeMapper.OwnerArguments(owner).Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], named => ScopedExternal(named, emissionScope), p => ScopedParameter(p, emissionScope))), symbol.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], named => ScopedExternal(named, emissionScope), p => ScopedParameter(p, emissionScope)))));
                        else if (symbol.IsGenericMethod) output.BindFunction(bindingShape, target.MakeGenericInstance(symbol.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], named => ScopedExternal(named, emissionScope), p => ScopedParameter(p, emissionScope))).ToArray()));
                        else output.BindFunction(bindingShape, target);
                    }
                    if (target.Signature.ReturnType.Primitive == PrimitiveType.Void && !shape.FunctionSignature!.NoResult)
                    {
                        var unit = NeoClrTypeMapper.Map(compilation.UnitTypeSymbol, type => nativeTypes[type], named => ScopedExternal(named, emissionScope), p => ScopedParameter(p, emissionScope));
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
                    var implementing = NeoClrTypeMapper.Map(instruction.Type!, type => nativeTypes[type], named => ScopedExternal(named, emissionScope), p => ScopedParameter(p, emissionScope));
                    if (interfaceMethods.TryGetValue(instruction.Method!.OriginalDefinition ?? instruction.Method, out var ownedContract))
                        output.CallConstrained(implementing, ownedContract);
                    else if (instruction.Method!.ContainingType is { Arity: > 0 } constrainedOwner)
                        output.CallConstrained(implementing, Import(instruction.Method.OriginalDefinition ?? instruction.Method).MakeConstructedReference(
                            constrainedOwner.TypeArguments.Select(t => NeoClrTypeMapper.Map(RuntimeSelfTypes.Substitute(compilation, t, instruction.Type!), type => nativeTypes[type], named => ScopedExternal(named, emissionScope), p => ScopedParameter(p, emissionScope)))));
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
                        output.CallVirtual(contract.MakeConstructedReference(NeoClrTypeMapper.OwnerArguments(owner).Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], named => ScopedExternal(named, emissionScope), p => ScopedParameter(p, emissionScope)))));
                    else output.CallVirtual(contract);
                }
                else if (instruction.Kind == LinearInstructionKind.Call && IsTypeHandleIntrinsic(instruction.Method!))
                    output.LoadTypeToken(NeoClrTypeMapper.Map(instruction.Method!.TypeArguments[0], type => nativeTypes[type], named => ScopedExternal(named, emissionScope), p => ScopedParameter(p, emissionScope)));
                else if (instruction.Kind == LinearInstructionKind.Call && IsCheckedReservation(instruction.Method!))
                    output.ReserveArray(NeoClrTypeMapper.Map(instruction.Method!.TypeArguments[0], type => nativeTypes[type], named => ScopedExternal(named, emissionScope), p => ScopedParameter(p, emissionScope)));
                else references.Resolve(instruction.Method!).EmitCall(output);
            }, field =>
            {
                if (SymbolEqualityComparer.Default.Equals(field, graphemeField)) return new NeoClrFieldReference(null, GraphemeStorage: nativeTypes[graphemeOwner!]);
                if (primitiveFields.TryGetValue(field, out var primitive)) return new NeoClrFieldReference(null, IntrinsicStorage: primitive, StringConstructorStorage: current.Plan.Symbol.MethodKind == MethodKind.Constructor);
                if (fields.TryGetValue(field, out var definition))
                {
                    if (field.ContainingType is { } capturedOwner && NeoClrTypeMapper.OwnerArguments(capturedOwner).Length > 0)
                        return new NeoClrFieldReference(definition, definition.MakeConstructedReference(NeoClrTypeMapper.OwnerArguments(capturedOwner)
                            .Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], named => ScopedExternal(named, emissionScope), p => ScopedParameter(p, emissionScope))).ToArray()));
                    return new NeoClrFieldReference(definition);
                }
                if (field is SubstitutedFieldSymbol substituted && field.ContainingType is { } owner && NeoClrTypeMapper.OwnerArguments(owner).Length > 0 && fields.TryGetValue(substituted.OriginalField, out definition))
                    return new NeoClrFieldReference(definition, definition.MakeConstructedReference(NeoClrTypeMapper.OwnerArguments(owner).Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], named => ScopedExternal(named, emissionScope), p => ScopedParameter(p, emissionScope))).ToArray()));
                var originalField = field is SubstitutedFieldSymbol importedSubstitution ? importedSubstitution.OriginalField : field;
                if (originalField is IInstanceFieldLayoutSymbol layout && originalField.ContainingType is { } declaring &&
                    IsSymbolOnlyOwnerDefinition(declaring) && CanImportVisibility(originalField) &&
                    IsSymbolOnlyType(originalField.Type, false))
                {
                    if (importedFields.TryGetValue(field, out var cached)) return cached;
                    _ = ImportExternalType(declaring);
                    var reference = assembly.CreateFieldReference(importedTypes[declaring], originalField.MetadataName,
                        MapSymbolOnlyType(originalField.Type), layout.InstanceStorageOrdinal, originalField.IsReadOnly, isStatic: originalField.IsStatic);
                    cached = declaring.Arity == 0 ? new NeoClrFieldReference(null, Import: reference)
                        : new NeoClrFieldReference(null, ImportedConstruction: reference.MakeConstructedReference(
                            field.ContainingType!.TypeArguments.Select(t => NeoClrTypeMapper.Map(t, type => nativeTypes[type], named => ScopedExternal(named, emissionScope), p => ScopedParameter(p, emissionScope))).ToArray()));
                    importedFields.Add(field, cached);
                    return cached;
                }
                if (field.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null })
                    throw Unsupported("native field requires a supported symbol-only emission contract and layout");
                throw Unsupported("undeclared instance field");
            },
                type => nativeTypes.TryGetValue(type, out var definition) ? definition : throw Unsupported("undeclared class local: " + type.ToDisplayString() + " (" + type.GetType().Name + ")"), named => ScopedExternal(named, emissionScope), p => ScopedParameter(p, emissionScope),
                producer => current.Method.DeclareLocal(Import(producer).Signature.ReturnType)));
        }
        // Bind first, then publish user annotations against complete output declarations.
        // Synthesized markers (flags, unions, nullable and params) retain their own encoders.
        foreach (var method in foreignImports.Keys.OfType<SourceMethodSymbol>())
            foreach (var attribute in method.GetReturnTypeAttributes())
                if (attribute.ApplicationSyntaxReference?.GetSyntax() is AttributeSyntax syntax)
                    emittedAttributes.Add((syntax.SyntaxTree, syntax.Span));
        EmitAttributes(compilation.Assembly, assembly.Definition.CustomAttributes);
        foreach (var (symbol, definition) in nativeTypes)
            EmitAttributes(symbol, definition.Definition.CustomAttributes);
        foreach (var (symbol, definition) in fields)
            EmitAttributes(symbol, definition.Definition.CustomAttributes);
        foreach (var (symbol, definition) in attributeProperties)
            EmitAttributes(symbol, definition.CustomAttributes);
        foreach (var (symbol, definition) in definedMethods.Concat(interfaceMethods))
        {
            if (!foreignImports.ContainsKey(symbol) && symbol.GetReturnTypeAttributes().Select(a => a.ApplicationSyntaxReference?.GetSyntax()).OfType<AttributeSyntax>().FirstOrDefault() is { } returnAttribute)
                throw new UnsupportedInputException("native return-value attributes are not yet supported", returnAttribute.GetLocation());
            EmitAttributes(symbol, definition.Definition.CustomAttributes);
            for (var i = 0; i < symbol.Parameters.Length; i++)
                EmitAttributes(symbol.Parameters[i], definition.Definition.GetParameterCustomAttributes(i));
        }
        foreach (var tree in compilation.SyntaxTrees)
            foreach (var attribute in tree.GetRoot().DescendantNodes().OfType<AttributeSyntax>())
                if (!emittedAttributes.Contains((tree, attribute.Span)))
                    throw new UnsupportedInputException("native attribute target is not yet supported", attribute.GetLocation());
        if (metadataAssembly)
            return NeoCLR.Metadata.Experimental.NativeModuleContainer.WriteLibraryBinary(assembly);
        return assembly.WriteNativeAssembly();

        void SetParameterArray(MetadataMethod method, int position)
        {
            // Register the selected native declaration so SDK emission preserves its owner.
            // Legacy CLI core binding remains handled by the metadata adapter.
            if (compilation.GetTypeByMetadataName("System.ParamArrayAttribute") is { } marker &&
                !SymbolEqualityComparer.Default.Equals(marker.ContainingAssembly, compilation.Assembly) &&
                marker is NativeNamedTypeSymbol)
            {
                var constructor = marker.InstanceConstructors.SingleOrDefault(c => c.Parameters.IsEmpty &&
                    c.DeclaredAccessibility == Accessibility.Public);
                if (constructor is null) throw Unsupported("native parameter arrays require a public parameterless marker constructor");
                method.Definition.GetParameterCustomAttributes(position).Add(new CustomAttributeDefinition(Import(constructor), []));
            }
            method.SetParameterArray(position);
        }

        bool IsFlagsAttribute(AttributeData attribute) => attribute is
        { AttributeClass: { } owner, ConstructorArguments.IsEmpty: true, NamedArguments.IsEmpty: true } &&
            owner.ToFullyQualifiedMetadataName() == "System.FlagsAttribute" &&
            NeoClrBindingContract.MatchesCore(owner.ContainingAssembly, options.CoreLibrary);

        void EmitAttributes(ISymbol symbol, IList<CustomAttributeDefinition> destination)
        {
            foreach (var attribute in symbol.GetAttributes())
            {
                if (attribute.ApplicationSyntaxReference?.GetSyntax() is not AttributeSyntax syntax) continue;
                diagnosticSyntax = syntax;
                emittedAttributes.Add((diagnosticSyntax.SyntaxTree, diagnosticSyntax.Span));
                if (symbol is INamedTypeSymbol { TypeKind: TypeKind.Enum } && IsFlagsAttribute(attribute)) continue;
                if (symbol is IMethodSymbol method && runtimeServices.Contains(method) &&
                    attribute.AttributeClass.ToFullyQualifiedMetadataName() == "System.Runtime.CompilerServices.MethodImplAttribute") continue;
                if (symbol is IMethodSymbol foreignMethod && foreignImports.ContainsKey(foreignMethod) &&
                    attribute.AttributeClass.ToFullyQualifiedMetadataName() == NeoClrForeignDeclaration.AttributeName) continue;
                if (symbol is IParameterSymbol parameter && parameter.ContainingSymbol is IMethodSymbol foreignOwner && foreignImports.ContainsKey(foreignOwner) &&
                    attribute.AttributeClass.ToFullyQualifiedMetadataName() == "System.Runtime.InteropServices.MarshalAsAttribute") continue;
                if (attribute.AttributeClass.ToFullyQualifiedMetadataName() is NeoClrForeignDeclaration.AttributeName or "System.Runtime.InteropServices.MarshalAsAttribute")
                    throw Unsupported("NativeImport requires a supported extern declaration");
                var constructor = attribute.AttributeConstructor;
                var arguments = attribute.ConstructorArguments.Select(Constant).ToArray();
                var named = attribute.NamedArguments.Select(argument =>
                {
                    var candidates = attribute.AttributeClass.GetMembers(argument.Key).Where(member =>
                        member is IFieldSymbol { IsStatic: false } or IPropertySymbol { IsStatic: false }).ToArray();
                    if (candidates.Length != 1) throw Unsupported("native named attributes require an unambiguous declared instance member");
                    var value = Constant(argument.Value);
                    if (value.Type.Primitive is null) throw Unsupported("native named enum attribute arguments are not yet supported");
                    return new CustomAttributeNamedArgument(argument.Key, candidates[0] is IFieldSymbol, value);
                }).ToArray();
                destination.Add(definedMethods.TryGetValue(constructor, out var local)
                    ? new CustomAttributeDefinition(local.Definition, arguments, named)
                    : new CustomAttributeDefinition(Import(constructor), arguments, named));
            }
        }

        CustomAttributeArgument Constant(TypedConstant constant)
        {
            if (constant.Type is not { } type || constant.Kind is not
                (TypedConstantKind.Primitive or TypedConstantKind.Null or TypedConstantKind.Enum))
                throw Unsupported("native custom attributes support String, Int32, Boolean and Int32-backed enum constants");
            if (type.GetNullableAbiProjection() == NullableAbiProjection.AnnotatedUnderlyingType)
                type = type.GetNonNullableType();
            SignatureType signature;
            if (type.TypeKind == TypeKind.Enum && type is INamedTypeSymbol { EnumUnderlyingType.SpecialType: SpecialType.System_Int32 } enumType)
            {
                if (!CallableSignature.TryType(enumType, false, out var enumStorage, bodyCapabilities))
                    throw Unsupported("unsupported attribute enum signature");
                signature = NeoClrTypeMapper.Map(enumStorage, t => nativeTypes[t], ImportExternalType);
            }
            else signature = type.SpecialType switch
            {
                SpecialType.System_String => PrimitiveType.String,
                SpecialType.System_Int32 => PrimitiveType.Int32,
                SpecialType.System_Boolean => PrimitiveType.Boolean,
                _ => throw Unsupported("native custom attributes support String, Int32, Boolean and Int32-backed enum constants")
            };
            return new CustomAttributeArgument(signature, constant.Value);
        }

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
            if (symbol is NativeMethodSymbol tupleMethod &&
                symbol.IsStatic && symbol.ContainingType is null && binding.Reference is NeoClrMetadataReference { HasStructuralTuples: true } tupleReference)
            {
                if (binding.NativeArtifactSha256 != tupleReference.Artifact.Sha256)
                    throw Unsupported("tuple callable requires its exact semantic snapshot");
                var definition = tupleReference.Definition.MainModule.GetMethodDefinition(tupleMethod.NativeMetadataToken)
                    ?? throw Unsupported("native tuple callable definition unavailable");
                if (definition.TryGetSignature(out var tupleSignature) && tupleSignature!.ParameterTypes.Append(tupleSignature.ReturnType).Any(NeoClrMetadataReference.ContainsTuple))
                    return assembly.ImportExperimentalTupleReference(definition, binding.CoreLibrary, new NativeCompositionResolver(options.Dependencies));
            }
            if (symbol is NativeMethodSymbol nativeMethod &&
                symbol.IsStatic && symbol.ContainingType is null && binding.Reference is NeoClrMetadataReference nativeReference)
            {
                if (binding.NativeArtifactSha256 != nativeReference.Artifact.Sha256)
                    throw Unsupported("experimental callable requires its exact semantic snapshot");
                var definition = nativeReference.Definition.MainModule.GetMethodDefinition(nativeMethod.NativeMetadataToken)
                    ?? throw Unsupported("native callable definition unavailable");
                if (definition.SpecialConstraints.Count != 0)
                    return assembly.ImportReference(definition, binding.CoreLibrary, new NativeCompositionResolver(options.Dependencies));
                if (definition.TypeConstraints.Any(c => NeoClrMetadataReference.ContainsComposition(c.Bound)))
                    return assembly.ImportExperimentalConstrainedReference(definition, binding.CoreLibrary, new NativeCompositionResolver(options.Dependencies));
                if (definition.TryGetSignature(out var raw) && raw!.ParameterTypes.Append(raw.ReturnType).Any(NeoClrMetadataReference.ContainsComposition))
                    return assembly.ImportExperimentalCompositionReference(definition, binding.CoreLibrary, new NativeCompositionResolver(options.Dependencies));
            }
            // This bounded profile uses only compiler symbols and host artifact values.
            // Nominal identities use the same symbol-only root-class authoring path.
            if (symbol.IsStatic && symbol.ContainingType is null &&
                symbol.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: { } artifact } &&
                CallableSignature.TryCreate(symbol, out var callable, bodyCapabilities) &&
                symbol.TypeParameters.All(p => (p.ConstraintKind & ~TypeParameterConstraintKind.TypeConstraint) == 0 &&
                    p.ConstraintTypes.All(t => t is INamedTypeSymbol { TypeKind: TypeKind.Interface, Arity: 0 } bound && IsSymbolOnlyOwnerDefinition(bound))) &&
                IsSymbolOnlyType(symbol.ReturnType, true) && symbol.Parameters.All(p => p.RefKind == RefKind.None && IsSymbolOnlyType(p.Type, false)))
            {
                if (artifact.Sha256 != binding.NativeArtifactSha256)
                    throw Unsupported("native dependency snapshot differs from semantic reference");
                var identity = new AssemblyIdentity(artifact.Name, artifact.Version, artifact.Culture, artifact.PublicKeyToken, artifact.Flags);
                var contract = new MethodSignature(MapSymbolOnlyType(symbol.ReturnType, result: true),
                    symbol.Parameters.Select(p => MapSymbolOnlyType(p.Type)), callable.GenericParameterNames);
                return assembly.CreateFunctionReference(identity, binding.CoreLibrary, artifact.Sha256,
                    symbol.ContainingNamespace?.ToMetadataName() ?? "", symbol.MetadataName, contract,
                    interfaceBounds: symbol.TypeParameters.SelectMany(p => p.ConstraintTypes.Select(bound =>
                        new GenericMethodTypeConstraint(p.Ordinal, ImportExternalType((INamedTypeSymbol)bound)))).ToArray(),
                    isInternalCall: symbol.OriginalDefinition is NativeMethodSymbol { IsRuntimeInternalCall: true });
            }
            if (symbol.ContainingType is { } owner && IsSymbolOnlyOwnerDefinition((INamedTypeSymbol)owner.OriginalDefinition) &&
                (owner.TypeKind == TypeKind.Interface ? symbol.IsAbstract && symbol.IsVirtual : (!symbol.IsAbstract && (!symbol.IsVirtual || owner.IsValueType || symbol.IsOverride || owner.SpecialType == SpecialType.System_Object) || SourceTypePlan.IsSupportedExternalBase(owner, bodyCapabilities.AllowsGenericClassVirtualSlots))) &&
                (CanImportVisibility(symbol) || symbol.MethodKind == MethodKind.Constructor && symbol.DeclaredAccessibility == Accessibility.ProtectedAndProtected && SourceTypePlan.IsSupportedExternalBase(owner, bodyCapabilities.AllowsGenericExternalClassInheritance)) && (symbol.IsStatic || symbol.Arity == 0) &&
                CallableSignature.TryCreate(symbol, out var memberSignature, bodyCapabilities) &&
                symbol.TypeParameters.All(p => (p.ConstraintKind & ~TypeParameterConstraintKind.TypeConstraint) == 0 &&
                    p.ConstraintTypes.All(t => t is INamedTypeSymbol { TypeKind: TypeKind.Interface, Arity: 0 } bound && IsSymbolOnlyOwnerDefinition(bound))) &&
                IsSymbolOnlyType(symbol.ReturnType, true) && symbol.Parameters.All(p => p.RefKind is RefKind.None or RefKind.Ref or RefKind.Out && IsSymbolOnlyType(p.Type, false)))
            {
                _ = ImportExternalType(owner);
                var declaration = importedTypes[(INamedTypeSymbol)owner.OriginalDefinition];
                var contract = new MethodSignature(MapSymbolOnlyType(symbol.ReturnType, result: true),
                    symbol.Parameters.Select(p => p.RefKind == RefKind.None ? MapSymbolOnlyType(p.Type) : SignatureType.ByReference(MapSymbolOnlyType(p.Type))),
                    memberSignature.GenericParameterNames, memberSignature.OutParameters.IsDefault ? [] : memberSignature.OutParameters);
                if (owner.SpecialType == SpecialType.System_Object && symbol.IsVirtual && !symbol.IsOverride &&
                    owner.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null })
                    return assembly.CreateObjectSlotReference(symbol.MetadataName, contract);
                return assembly.CreateMethodReference(declaration, symbol.MetadataName, contract, symbol.IsStatic, isOverride: symbol.IsOverride, nativePrimitive: HasNativePrimitiveStorage(owner.SpecialType) && owner.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null }
                    ? NeoClrTypeMapper.Instance.Map(Enum.Parse<EmissionPrimitiveType>(owner.SpecialType.ToString()[7..])) : null, visibility: symbol.DeclaredAccessibility == Accessibility.ProtectedAndProtected ? MethodVisibility.Protected : MethodVisibility.Public,
                    isVirtual: owner.TypeKind == TypeKind.Class && (owner.Arity == 0 || bodyCapabilities.AllowsGenericClassVirtualSlots && (!symbol.IsAbstract || bodyCapabilities.AllowsAbstractGenericClasses)) && (symbol.IsVirtual || symbol.IsOverride || symbol.IsAbstract),
                    isAbstract: owner.TypeKind == TypeKind.Class && symbol.IsAbstract,
                    interfaceBounds: symbol.TypeParameters.SelectMany(p => p.ConstraintTypes.Select(bound =>
                        new GenericMethodTypeConstraint(p.Ordinal, ImportExternalType((INamedTypeSymbol)bound)))).ToArray());
            }
            // A native callable must carry a complete supported semantic contract.
            // Do not recover missing emission facts by reopening its reader definition.
            if (symbol.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null })
                throw Unsupported("native callable requires a supported symbol-only emission contract: " + symbol.ToDisplayString());
            var dependencyMetadata = binding.Definition;
            var types = dependencyMetadata.MainModule.Types.Where(t => symbol.ContainingType is { } owner && t.GenericArity == owner.Arity && MatchesType(t, owner)).Take(2).ToArray();
            if (types.Length != 1) throw Unsupported("dependency type unavailable or ambiguous");
            if (!CallableSignature.TryCreate(symbol, out var signature, bodyCapabilities))
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
        bool CanImportVisibility(ISymbol symbol) => symbol.DeclaredAccessibility == Accessibility.Public
            || symbol.DeclaredAccessibility == Accessibility.Internal && symbol.ContainingAssembly is { } declaring
                && FriendAssemblyAccess.IsGranted(declaring, compilation.Assembly);

        bool IsSymbolOnlyOwnerDefinition(INamedTypeSymbol original) =>
            IsSymbolOnlyReferenceDefinition(original) ||
            original.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null } &&
            original.TypeKind == TypeKind.Class && original.IsStatic && original.ContainingType is null &&
            CanImportVisibility(original) && original.Interfaces.IsEmpty &&
            original.TypeParameters.All(p => p.ConstraintKind == TypeParameterConstraintKind.None && p.ConstraintTypes.IsEmpty && (p.Variance == VarianceKind.None || original.TypeKind == TypeKind.Interface));

        bool IsSymbolOnlyReferenceDefinition(INamedTypeSymbol original, int depth = 0) =>
            depth < 32 &&
            original.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null } &&
            (original.TypeKind is TypeKind.Class or TypeKind.Interface or TypeKind.Struct or TypeKind.Enum) &&
            !original.IsStatic && (original.ContainingType is null ||
                (original is IUnionCaseTypeSymbol unionCase ? unionCase.MetadataContainingType : original.ContainingType) is { Arity: 0 } parent && IsSymbolOnlyOwnerDefinition(parent)) && CanImportVisibility(original) &&
            original.Interfaces.All(contract => IsSymbolOnlyReferenceDefinition((INamedTypeSymbol)contract.OriginalDefinition, depth + 1)) &&
            (original.BaseType is null || original.BaseType.SpecialType == (original.TypeKind == TypeKind.Enum ? SpecialType.System_Enum : original.IsValueType ? SpecialType.System_ValueType : SpecialType.System_Object) ||
                original.TypeKind == TypeKind.Class && original.BaseType is { ContainingType: null } parentClass &&
                IsSymbolOnlyReferenceDefinition((INamedTypeSymbol)parentClass.OriginalDefinition, depth + 1)) &&
            original.TypeParameters.All(p => (p.ConstraintKind == TypeParameterConstraintKind.None || original.TypeKind == TypeKind.Class && original.ContainingType is null &&
                (p.ConstraintKind & ~(TypeParameterConstraintKind.ReferenceType | TypeParameterConstraintKind.ValueType | TypeParameterConstraintKind.Constructor | TypeParameterConstraintKind.TypeConstraint | TypeParameterConstraintKind.FunctionType)) == 0) &&
                p.ConstraintTypes.All(bound => bound is INamedTypeSymbol { TypeKind: TypeKind.Class, Arity: 0, ContainingType: null } nominal && IsSymbolOnlyReferenceDefinition(nominal, depth + 1)) && (p.Variance == VarianceKind.None || original.TypeKind == TypeKind.Interface));

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
            type is INamedTypeSymbol { TypeKind: TypeKind.Delegate } functionType && CallableSignature.TryFunction(functionType, out _, bodyCapabilities) ||
            !result && IsRuntimeUnitValue(type) || IsRuntimeErasedValue(type) ||
            type is (IStructuralUnionTypeSymbol or IIntersectionTypeSymbol) ||
            type is ITupleTypeSymbol tuple && tuple.TupleElements.All(e => IsSymbolOnlyType(e.Type, false)) ||
            type is ITypeParameterSymbol ||
            type is IArrayTypeSymbol { Rank: 1, FixedLength: null, ElementType: not IArrayTypeSymbol } vector && IsSymbolOnlyType(vector.ElementType, false) ||
            type.SpecialType is SpecialType.System_Object or SpecialType.System_RuntimeTypeHandle or SpecialType.System_SByte or SpecialType.System_Int16 or SpecialType.System_UInt16 or SpecialType.System_UInt32 or SpecialType.System_UInt64 or SpecialType.System_IntPtr or SpecialType.System_UIntPtr or SpecialType.System_Byte or SpecialType.System_Int32 or SpecialType.System_Int64 or SpecialType.System_Single or SpecialType.System_Double or SpecialType.System_Boolean or SpecialType.System_String or SpecialType.System_Char ||
            result && type.SpecialType is SpecialType.System_Unit or SpecialType.System_Void ||
            type is INamedTypeSymbol named && IsSymbolOnlyReferenceDefinition((INamedTypeSymbol)named.OriginalDefinition) &&
            named.TypeArguments.All(argument => IsSymbolOnlyType(argument, false));

        SignatureType MapSymbolOnlyType(ITypeSymbol type, bool result = false) => type switch
        {
            IStructuralUnionTypeSymbol or IIntersectionTypeSymbol or ITupleTypeSymbol => ImportExternalType(type),
            _ when type.GetNullableAbiProjection() == NullableAbiProjection.AnnotatedUnderlyingType => MapSymbolOnlyType(type.GetNonNullableType(), result),
            IPointerTypeSymbol pointer => NeoClrTypeMapper.Map(pointer, owned => nativeTypes[owned], ImportExternalType),
            _ when RuntimeSelfTypes.IsSelf(compilation, type) => SignatureType.Self,
            INamedTypeSymbol { TypeKind: TypeKind.Delegate } functionType when CallableSignature.TryFunction(functionType, out var shape, bodyCapabilities) =>
                SignatureType.Function(new MethodSignature(
                    NeoClrTypeMapper.Map(shape.ReturnType, owned => nativeTypes[owned], ImportExternalType),
                    shape.ParameterTypes.Select(t => NeoClrTypeMapper.Map(t, owned => nativeTypes[owned], ImportExternalType)))),
            INamedTypeSymbol when !result && IsRuntimeUnitValue(type) =>
                NeoClrTypeMapper.Map(((UnitTypeSymbol)compilation.GetSpecialType(SpecialType.System_Unit)).RuntimeRepresentation!,
                    owned => nativeTypes[owned], ImportExternalType),
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
            if (method.MethodKind == MethodKind.StaticConstructor)
            {
                if (!CallableSignature.TryCreate(method, out var signature, bodyCapabilities))
                    throw Unsupported("type initializer signature");
                var syntax = method.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax() ?? throw Unsupported("type initializer declaration");
                var statements = FieldInitializationPlan.Create(compilation, method).Cast<BoundStatement>().ToList();
                if (syntax is ConstructorDeclarationSyntax constructor)
                {
                    var bodySyntax = (SyntaxNode?)constructor.Body ?? constructor.ExpressionBody;
                    if (bodySyntax is null || compilation.GetSemanticModel(syntax.SyntaxTree).GetBoundNode(bodySyntax, BoundTreeView.Original) is not BoundBlockStatement body)
                        throw Unsupported("type initializer body");
                    statements.Add(body);
                }
                var bodyPlan = new SourceCallablePlan(method, syntax, syntax, method.ContainingType, method.MetadataName, signature,
                    PreparedBody: Lowerer.LowerBlock(method, new BoundBlockStatement(statements.ToArray())));
                if (!bodyPlan.IsSupportedBy(bodyCapabilities)) throw Unsupported("type initializer contract");
                return bodyPlan;
            }
            if (!SourceCallablePlan.TryCreate(method, out var plan, bodyCapabilities))
                throw Unsupported("callable declaration: " + method.ToDisplayString());
            return plan!;
        }
    }
}

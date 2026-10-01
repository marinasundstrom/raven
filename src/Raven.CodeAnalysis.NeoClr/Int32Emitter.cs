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
        SyntaxNode diagnosticSyntax = compilation.SyntaxTrees[0].GetRoot();
        var plans = new List<SourceCallablePlan>();
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
                else if (member is ClassDeclarationSyntax type)
                {
                    if (type.AttributeLists.Count != 0 || type.TypeParameterList is not null || type.ParameterList is not null ||
                        type.BaseList is not null || type.ConstraintClauses.Count != 0 || type.PermitsClause is not null ||
                        type.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.StaticKeyword or SyntaxKind.PartialKeyword)))
                        throw Unsupported("only public or internal nongeneric static or root classes without additional contracts");
                    var typeSymbol = model.GetDeclaredSymbol(type) as INamedTypeSymbol ?? throw Unsupported("type symbol unavailable");
                    if (!SourceTypePlan.TryCreate(typeSymbol, out var typePlan, NeoClrCapabilities.Shared))
                        throw Unsupported("only public or internal nongeneric static or root classes");
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
                                throw Unsupported("only public/internal/private mutable primitive or owned root-class instance fields");
                            foreach (var variable in fieldSyntax.Declaration.Declarators)
                            {
                                diagnosticSyntax = variable;
                                if (model.GetDeclaredSymbol(variable) is not IFieldSymbol { IsStatic: false, IsReadOnly: false, IsConst: false, RefKind: RefKind.None } field ||
                                    field.DeclaredAccessibility is not (Accessibility.Public or Accessibility.Internal or Accessibility.Private) ||
                                    !CallableSignature.TryType(field.Type, false, out _))
                                    throw Unsupported("only public/internal/private mutable primitive or owned root-class instance fields");
                                storageFields.Add(field);
                            }
                            continue;
                        }
                        if (!typeSymbol.IsStatic && typeMember is PropertyDeclarationSyntax propertySyntax)
                        {
                            if (propertySyntax.AttributeLists.Count != 0 || propertySyntax.ExplicitInterfaceSpecifier is not null ||
                                propertySyntax.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword)) ||
                                model.GetDeclaredSymbol(propertySyntax) is not SourcePropertySymbol { IsStatic: false } property ||
                                property.BackingField is { IsReadOnly: true } ||
                                !CallableSignature.TryType(property.Type, false, out _))
                                throw Unsupported("only primitive or owned root-class instance properties and mutable private storage");
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
                            if ((constructor.Body is null && constructor.ExpressionBody is null) || constructor.Initializer is not null || constructor.AttributeLists.Count != 0 ||
                                constructor.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.InternalKeyword or SyntaxKind.PrivateKeyword)))
                                throw Unsupported("only explicit root constructors with a block or expression body and no chaining");
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
                else throw Unsupported("only top-level functions and public or internal static classes");
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
        // Materialize definitions only after all source declarations and body capabilities pass.
        // Every definition exists before reference resolution or method-body emission.
        var assembly = new AssemblyBuilder(options.Identity, options.CoreLibrary);
        var owners = new Dictionary<INamedTypeSymbol, NeoClrCallableDefinitionBuilder>(SymbolEqualityComparer.Default);
        var typeDefinitions = new NeoClrTypeDefinitionBuilder(assembly);
        var nativeTypes = new Dictionary<INamedTypeSymbol, TypeBuilder>(SymbolEqualityComparer.Default);
        var functions = new NeoClrCallableDefinitionBuilder(assembly, resolveClass: type => nativeTypes[type]);
        foreach (var type in declaredTypes.Values)
        {
            var definition = type.Define(typeDefinitions);
            nativeTypes.Add(type.Symbol, definition);
            owners.Add(type.Symbol, new(assembly, definition, type => nativeTypes[type]));
        }
        var fields = new Dictionary<IFieldSymbol, FieldBuilder>(SymbolEqualityComparer.Default);
        foreach (var field in storageFields)
        {
            CallableSignature.TryType(field.Type, false, out var fieldType);
            SignatureType storageType = fieldType.Class is { } fieldClass
                ? nativeTypes[fieldClass] : NeoClrTypeMapper.Instance.Map(fieldType.Primitive!.Value);
            fields.Add(field, nativeTypes[field.ContainingType!].AddField(field.MetadataName, storageType, field.DeclaredAccessibility switch
            {
                Accessibility.Public => FieldVisibility.Public,
                Accessibility.Internal => FieldVisibility.Internal,
                Accessibility.Private => FieldVisibility.Private,
                _ => throw Unsupported("unsupported field visibility")
            }));
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
            SignatureType valueType = propertyType.Class is { } propertyClass
                ? nativeTypes[propertyClass] : NeoClrTypeMapper.Instance.Map(propertyType.Primitive!.Value);
            nativeTypes[property.ContainingType!].AddProperty(property.MetadataName, valueType,
                property.GetMethod is null ? null : definedMethods[property.GetMethod], property.SetMethod is null ? null : definedMethods[property.SetMethod]);
        }
        var references = new CallableReferenceTable<NeoClrCallableReference>(target =>
        {
            var systemFunction = ImportSystem(target);
            return systemFunction is not null
                ? NeoClrCallableReference.Create(systemFunction)
                : NeoClrCallableReference.Create(Import(target));
        });
        foreach (var declaration in methods)
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
                if (instruction.Kind == LinearInstructionKind.NewObject)
                {
                    if (!definedMethods.TryGetValue(instruction.Method!, out var constructor)) throw Unsupported("only declared source constructors");
                    output.NewObject(constructor);
                }
                else references.Resolve(instruction.Method!).EmitCall(output);
            }, field => fields.TryGetValue(field, out var definition) ? definition : throw Unsupported("undeclared instance field"),
                type => nativeTypes.TryGetValue(type, out var definition) ? definition : throw Unsupported("undeclared class local")));
        }
        return assembly.WriteNativeAssembly();

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
        ImportedMethodReference Import(IMethodSymbol symbol)
        {
            if (!symbol.IsStatic) throw Unsupported("instance call");
            var binding = dependencies.SingleOrDefault(d => SymbolEqualityComparer.Default.Equals(d.Symbol, symbol.ContainingAssembly)).Dependency
                ?? throw Unsupported("unregistered dependency: " + symbol.ContainingAssembly?.Name);
            var dependencyMetadata = binding.Definition;
            var types = dependencyMetadata.MainModule.Types.Where(t => t.DeclaringType is null && t.GenericArity == 0 &&
                (t.Namespace.Length == 0 ? t.Name : t.Namespace + "." + t.Name) == symbol.ContainingType?.ToFullyQualifiedMetadataName()).Take(2).ToArray();
            if (types.Length != 1) throw Unsupported("dependency type unavailable or ambiguous");
            var definitions = types[0].Methods.Where(m => m.Name == symbol.MetadataName && MatchesSignature(m, symbol)).Take(2).ToArray();
            if (definitions.Length != 1) throw Unsupported("dependency method contract unavailable or ambiguous");
            return assembly.ImportReference(definitions[0], binding.CoreLibrary);
        }
        UnsupportedInputException Unsupported(string detail) => new(detail, diagnosticSyntax.GetLocation());

        static bool MatchesSignature(MethodDefinition method, IMethodSymbol symbol)
        {
            if (!method.TryGetStaticPrimitiveSignature(out var metadata) ||
                !PrimitiveCallableSignature.TryCreate(symbol, out var signature)) return false;
            var expected = NeoClrCallableDefinitionBuilder.ToMetadata(signature);
            return metadata!.ReturnType == expected.ReturnType &&
                metadata.ParameterTypes.SequenceEqual(expected.ParameterTypes);
        }

        static bool ReturnsValue(IMethodSymbol method) => method.ReturnType.SpecialType == SpecialType.System_Int32;

        SourceCallablePlan GetPlan(IMethodSymbol method)
        {
            if (!SourceCallablePlan.TryCreate(method, out var plan, NeoClrCapabilities.Shared))
                throw Unsupported("only nongeneric primitive or owned root-class parameters/results (Unit only as result): " + method.Name + " (" + string.Join(", ", method.Parameters.Select(p => $"{p.Type.SpecialType}, default={p.HasExplicitDefaultValue}, params={p.IsVarParams}, ref={p.RefKind}")) + ")");
            return plan!;
        }
    }
}

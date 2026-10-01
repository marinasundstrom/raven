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
        var declaredTypes = new List<SourceStaticTypePlan>();
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
                    if (member.Parent is not CompilationUnitSyntax)
                        throw Unsupported("namespace-scoped functions require native function namespace metadata");
                    if (declaration.Body is null || declaration.AttributeLists.Count != 0 || declaration.Modifiers.Count != 0)
                        throw Unsupported("only top-level block-bodied functions");
                    var symbol = model.GetDeclaredSymbol(declaration) as IMethodSymbol ?? throw Unsupported("function symbol unavailable");
                    if (compilation.Options.OutputKind == OutputKind.DynamicallyLinkedLibrary && symbol.DeclaredAccessibility != Accessibility.Public)
                        throw Unsupported("nonpublic library functions require visibility metadata");
                    var plan = GetPlan(symbol);
                    plans.Add(plan);
                }
                else if (member is ClassDeclarationSyntax type)
                {
                    if (type.AttributeLists.Count != 0 || type.TypeParameterList is not null || type.ParameterList is not null ||
                        type.BaseList is not null || type.ConstraintClauses.Count != 0 || type.PermitsClause is not null ||
                        type.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.StaticKeyword)))
                        throw Unsupported("only public nongeneric static classes without additional contracts");
                    var typeSymbol = model.GetDeclaredSymbol(type) as INamedTypeSymbol ?? throw Unsupported("type symbol unavailable");
                    if (!SourceStaticTypePlan.TryCreate(typeSymbol, out var typePlan))
                        throw Unsupported("only public nongeneric static classes");
                    declaredTypes.Add(typePlan!);
                    foreach (var typeMember in type.Members)
                    {
                        diagnosticSyntax = typeMember;
                        if (typeMember is not MethodDeclarationSyntax method || method.Body is null || method.AttributeLists.Count != 0 ||
                            method.ExplicitInterfaceSpecifier is not null || method.ConstraintClauses.Count != 0 ||
                            method.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.StaticKeyword)))
                            throw Unsupported("only public static block-bodied methods");
                        var symbol = model.GetDeclaredSymbol(method) as IMethodSymbol ?? throw Unsupported("method symbol unavailable");
                        if (!symbol.IsStatic || symbol.DeclaredAccessibility != Accessibility.Public) throw Unsupported("only public static methods");
                        var plan = GetPlan(symbol);
                        plans.Add(plan);
                    }
                }
                else throw Unsupported("only top-level functions and public static classes");
            }
        }
        // Materialize definitions only after collecting and validating source declarations.
        // Every definition exists before reference resolution or method-body emission.
        var assembly = new AssemblyBuilder(options.Identity, options.CoreLibrary);
        var functions = new NeoClrCallableDefinitionBuilder(assembly);
        var owners = new Dictionary<INamedTypeSymbol, NeoClrCallableDefinitionBuilder>(SymbolEqualityComparer.Default);
        var typeDefinitions = new NeoClrTypeDefinitionBuilder(assembly);
        foreach (var type in declaredTypes)
            owners.Add(type.Symbol, new(assembly, type.Define(typeDefinitions)));
        var methods = new List<(SourceCallablePlan Plan, MetadataMethod Method)>();
        foreach (var plan in plans)
        {
            diagnosticSyntax = plan.Syntax;
            var owner = plan.IsAssemblyFunction ? functions : owners[plan.TypeOwner!];
            methods.Add((plan, plan.Define(owner)));
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
            if (!current.Plan.TryLowerBody(compilation, IsConsoleCall, out var lowered, out var failure))
                throw new UnsupportedInputException(failure!.Detail, failure.Syntax.GetLocation());
            lowered!.Emit(new NeoClrLinearMethodBuilder(current.Method, (instruction, output) =>
            {
                diagnosticSyntax = instruction.Syntax;
                references.Resolve(instruction.Method!).EmitCall(output);
            }));
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
                method.Parameters[0].RefKind != RefKind.None || method.ReturnType.SpecialType is not (SpecialType.System_Void or SpecialType.System_Unit) ||
                call.Arguments.ToArray() is not [BoundLiteralExpression { Value: string }])
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
            if (!SourceCallablePlan.TryCreate(method, out var plan))
                throw Unsupported("only nongeneric Int32/Int64/Boolean parameters and Int32/Int64/Boolean/Unit results: " + method.Name + " (" + string.Join(", ", method.Parameters.Select(p => $"{p.Type.SpecialType}, default={p.HasExplicitDefaultValue}, params={p.IsVarParams}, ref={p.RefKind}")) + ")");
            return plan!;
        }
    }
}

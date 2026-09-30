using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using MetadataMethod = NeoCLR.Metadata.Experimental.Model.MethodBuilder;

namespace Raven.CodeAnalysis.NeoClr;

// Body/declaration lowering behind the explicitly selected native backend. It consumes public
// semantic operations; no bound nodes, reflection emit or source-token operator guessing.
internal static class Int32Emitter
{
    internal static byte[] Emit(Compilation compilation, NeoClrEmitOptions options,
        IReadOnlyList<(IAssemblySymbol Symbol, NeoClrMetadataDependency Dependency)> dependencies)
    {
        var assembly = new AssemblyBuilder(options.Identity, options.CoreLibrary);
        SyntaxNode diagnosticSyntax = compilation.SyntaxTrees[0].GetRoot();
        var methods = new List<(SemanticModel Model, IMethodSymbol Symbol, SyntaxNode Syntax, BlockStatementSyntax Body, MetadataMethod Method)>();
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
                    CheckSignature(symbol);
                    methods.Add((model, symbol, declaration, declaration.Body, assembly.AddFunction(symbol.Name, symbol.Parameters.Length, ReturnsValue(symbol))));
                }
                else if (member is ClassDeclarationSyntax type)
                {
                    if (type.AttributeLists.Count != 0 || type.TypeParameterList is not null || type.ParameterList is not null ||
                        type.BaseList is not null || type.ConstraintClauses.Count != 0 || type.PermitsClause is not null ||
                        type.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.StaticKeyword)))
                        throw Unsupported("only public nongeneric static classes without additional contracts");
                    var typeSymbol = model.GetDeclaredSymbol(type) as INamedTypeSymbol ?? throw Unsupported("type symbol unavailable");
                    if (!typeSymbol.IsStatic || typeSymbol.DeclaredAccessibility != Accessibility.Public || typeSymbol.Arity != 0)
                        throw Unsupported("only public nongeneric static classes");
                    var fullName = typeSymbol.ToFullyQualifiedMetadataName();
                    var typeNamespace = typeSymbol.ContainingNamespace.IsGlobalNamespace ? "" : fullName[..^(typeSymbol.MetadataName.Length + 1)];
                    var owner = assembly.AddType(typeNamespace, typeSymbol.MetadataName);
                    foreach (var typeMember in type.Members)
                    {
                        diagnosticSyntax = typeMember;
                        if (typeMember is not MethodDeclarationSyntax method || method.Body is null || method.AttributeLists.Count != 0 ||
                            method.ExplicitInterfaceSpecifier is not null || method.ConstraintClauses.Count != 0 ||
                            method.Modifiers.Any(m => m.Kind is not (SyntaxKind.PublicKeyword or SyntaxKind.StaticKeyword)))
                            throw Unsupported("only public static block-bodied methods");
                        var symbol = model.GetDeclaredSymbol(method) as IMethodSymbol ?? throw Unsupported("method symbol unavailable");
                        if (!symbol.IsStatic || symbol.DeclaredAccessibility != Accessibility.Public) throw Unsupported("only public static methods");
                        CheckSignature(symbol);
                        methods.Add((model, symbol, method, method.Body, owner.AddMethod(symbol.MetadataName, symbol.Parameters.Length, ReturnsValue(symbol))));
                    }
                }
                else throw Unsupported("only top-level functions and public static classes");
            }
        }
        if (compilation.Options.OutputKind == OutputKind.ConsoleApplication)
        {
            var entry = compilation.GetEntryPoint() ?? throw Unsupported("entry point unavailable");
            assembly.EntryPoint = methods.SingleOrDefault(m => SymbolEqualityComparer.Default.Equals(m.Symbol, entry)).Method
                ?? throw Unsupported("entry must be a declared Int32/Unit function or static method");
        }
        foreach (var current in methods)
        {
            diagnosticSyntax = current.Syntax;
            var body = current.Model.GetOperation(current.Body) as IBlockOperation ?? throw Unsupported("function operation body unavailable");
            if (!LinearMethodBody.TryLower(current.Symbol, body, IsConsoleCall, out var lowered, out var failure))
                throw new UnsupportedInputException(failure!.Detail, failure.Syntax.GetLocation());
            lowered!.Emit(new NeoClrLinearMethodBuilder(current.Method, (instruction, output) =>
            {
                diagnosticSyntax = instruction.Syntax;
                var target = instruction.Method!;
                var local = methods.SingleOrDefault(m => SymbolEqualityComparer.Default.Equals(m.Symbol, target)).Method;
                var systemFunction = local is null ? ImportSystem(target) : null;
                var imported = local is null && systemFunction is null ? Import(target) : null;
                if (local is not null) output.Emit(OpCode.Call, local);
                else if (systemFunction is not null) output.Emit(OpCode.Call, systemFunction);
                else output.Emit(OpCode.Call, imported!);
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

        bool IsConsoleCall(IInvocationOperation call)
        {
            if (options.ConsoleReference is null) return false;
            var method = call.TargetMethod;
            if (call.Instance is not null || !method.IsStatic || method.IsGenericMethod || method.Name != "WriteLine" ||
                method.ContainingType?.ToFullyQualifiedMetadataName() != "System.Console" ||
                !SymbolEqualityComparer.Default.Equals(method.ContainingAssembly, compilation.GetAssemblyOrModuleSymbol(options.ConsoleReference)) ||
                method.Parameters.Length != 1 || method.Parameters[0].Type.GetNonNullableType().SpecialType != SpecialType.System_String ||
                method.Parameters[0].RefKind != RefKind.None || method.ReturnType.SpecialType is not (SpecialType.System_Void or SpecialType.System_Unit) ||
                call.Arguments.Length != 1 || call.Arguments[0] is not IArgumentOperation { IsNamed: false, Value: ILiteralOperation { Value: string text } })
                return false;
            return true;
        }

        NativeFunctionDefinition? ImportSystem(IMethodSymbol symbol)
        {
            if (options.SystemSymbols is not { } system ||
                !SymbolEqualityComparer.Default.Equals(symbol.ContainingAssembly, compilation.GetAssemblyOrModuleSymbol(system.Reference))) return null;
            var name = symbol.ContainingType?.ToFullyQualifiedMetadataName() + "." + symbol.MetadataName;
            var matches = system.Functions.Where(f => f.Name == name && f.TryGetStaticInt32Signature(out var count) &&
                count == symbol.Parameters.Length && ReturnsValue(symbol)).Take(2).ToArray();
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
            var definitions = types[0].Methods.Where(m => m.Name == symbol.MetadataName && m.TryGetStaticInt32Signature(out var count, out var result) && count == symbol.Parameters.Length && result == ReturnsValue(symbol)).Take(2).ToArray();
            if (definitions.Length != 1) throw Unsupported("dependency method contract unavailable or ambiguous");
            return assembly.ImportReference(definitions[0], binding.CoreLibrary);
        }
        UnsupportedInputException Unsupported(string detail) => new(detail, diagnosticSyntax.GetLocation());

        static bool ReturnsValue(IMethodSymbol method) => method.ReturnType.SpecialType == SpecialType.System_Int32;

        void CheckSignature(IMethodSymbol method)
        {
            if (!LinearMethodBody.HasSupportedSignature(method))
                throw Unsupported("only nongeneric Int32 parameters and Int32/Unit results: " + method.Name + " (" + string.Join(", ", method.Parameters.Select(p => $"{p.Type.SpecialType}, default={p.HasExplicitDefaultValue}, params={p.IsVarParams}, ref={p.RefKind}")) + ")");
        }
    }
}

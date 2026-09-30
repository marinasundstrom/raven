using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using MetadataMethod = NeoCLR.Metadata.Experimental.Model.MethodBuilder;
using OperatorKind = Raven.CodeAnalysis.Operations.BinaryOperatorKind;

namespace Raven.CodeAnalysis.NeoClr;

// Experimental consumer, not an installed Compilation.Emit backend. It consumes public
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
            if (!ReturnsValue(entry)) throw Unsupported("entry must return Int32");
            assembly.EntryPoint = methods.SingleOrDefault(m => SymbolEqualityComparer.Default.Equals(m.Symbol, entry)).Method
                ?? throw Unsupported("entry must be a declared Int32 function or static method");
        }
        foreach (var current in methods)
        {
            diagnosticSyntax = current.Syntax;
            var body = current.Model.GetOperation(current.Body) as IBlockOperation ?? throw Unsupported("function operation body unavailable");
            foreach (var statement in body.Operations)
            {
                diagnosticSyntax = statement.Syntax;
                if (statement is IExpressionStatementOperation { Operation: IInvocationOperation call })
                {
                    if (EmitConsole(call, current.Method)) continue;
                    if (ReturnsValue(call.TargetMethod)) throw Unsupported("discarded value calls");
                    EmitValue(call, current.Symbol, current.Method);
                    continue;
                }
                if (statement is IReturnOperation { ReturnedValue: null } && !ReturnsValue(current.Symbol))
                {
                    current.Method.Return();
                    continue;
                }
                if (statement is not IReturnOperation { ReturnedValue: { } value }) throw Unsupported("only value-return statements");
                EmitValue(value, current.Symbol, current.Method);
                current.Method.Return();
            }
            if (!ReturnsValue(current.Symbol) && body.Operations.LastOrDefault() is not IReturnOperation)
                current.Method.Return();
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

        bool EmitConsole(IInvocationOperation call, MetadataMethod output)
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
            output.WriteConsoleLine(text);
            return true;
        }

        void EmitValue(IOperation operation, IMethodSymbol source, MetadataMethod output)
        {
            diagnosticSyntax = operation.Syntax;
            switch (operation)
            {
                case ILiteralOperation { Value: int value }:
                    output.LoadConstant(value); return;
                case IParameterReferenceOperation parameter:
                    var index = source.Parameters.IndexOf(parameter.Parameter, 0, source.Parameters.Length, SymbolEqualityComparer.Default);
                    if (index < 0) throw Unsupported("captured parameter");
                    output.LoadArgument(index); return;
                case IParenthesizedOperation parenthesized when parenthesized.Operand is { } operand:
                    EmitValue(operand, source, output); return;
                case IBinaryOperation binary when !binary.IsChecked && !binary.IsLifted && binary.OperatorMethod is null &&
                    binary.Type?.SpecialType == SpecialType.System_Int32 && binary.Left is not null && binary.Right is not null:
                    if (binary.OperatorKind is not (OperatorKind.Add or OperatorKind.Subtract or OperatorKind.Multiply)) throw Unsupported("binary operator " + binary.OperatorKind);
                    EmitValue(binary.Left, source, output); EmitValue(binary.Right, source, output);
                    if (binary.OperatorKind == OperatorKind.Add) output.Add();
                    else if (binary.OperatorKind == OperatorKind.Subtract) output.Subtract();
                    else output.Multiply();
                    return;
                case IInvocationOperation call when call.Instance is null:
                    CheckSignature(call.TargetMethod);
                    if (call.Arguments.Length != call.TargetMethod.Parameters.Length) throw Unsupported("optional/expanded arguments");
                    var local = methods.SingleOrDefault(m => SymbolEqualityComparer.Default.Equals(m.Symbol, call.TargetMethod)).Method;
                    var imported = local is null ? Import(call.TargetMethod) : null;
                    foreach (var argument in call.Arguments)
                    {
                        if (argument is IArgumentOperation { IsNamed: false, Value: { } argumentValue }) EmitValue(argumentValue, source, output);
                        else throw Unsupported("named or unavailable argument");
                    }
                    if (local is not null) output.Call(local);
                    else output.Call(imported!);
                    return;
                default: throw Unsupported("operation " + operation.Kind);
            }
        }
        ImportedMethodReference Import(IMethodSymbol symbol)
        {
            if (!symbol.IsStatic) throw Unsupported("instance call");
            var binding = dependencies.SingleOrDefault(d => SymbolEqualityComparer.Default.Equals(d.Symbol, symbol.ContainingAssembly)).Dependency
                ?? throw Unsupported("unregistered dependency");
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
            if (method.IsGenericMethod || method.IsExtensionMethod || method.IsAsync || method.ReturnType.SpecialType is not (SpecialType.System_Int32 or SpecialType.System_Unit or SpecialType.System_Void) ||
                method.Parameters.Any(p => p.Type.SpecialType != SpecialType.System_Int32 || p.RefKind != RefKind.None || p.HasExplicitDefaultValue || p.IsVarParams))
                throw Unsupported("only nongeneric Int32 parameters and Int32/Unit results: " + method.Name + " (" + string.Join(", ", method.Parameters.Select(p => $"{p.Type.SpecialType}, default={p.HasExplicitDefaultValue}, params={p.IsVarParams}, ref={p.RefKind}")) + ")");
        }
    }
}

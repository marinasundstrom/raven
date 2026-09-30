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
        var methods = new List<(SemanticModel Model, IMethodSymbol Symbol, FunctionStatementSyntax Syntax, MetadataMethod Method)>();
        // Collect all declarations before emitting any body, so calls do not depend on file order.
        foreach (var tree in compilation.SyntaxTrees)
        {
            var model = compilation.GetSemanticModel(tree);
            var root = (CompilationUnitSyntax)tree.GetRoot();
            diagnosticSyntax = root;
            if (root.AttributeLists.Count != 0 || root.Members.Any(member => member is not GlobalStatementSyntax { Statement: FunctionStatementSyntax }))
                throw Unsupported("only top-level function declarations");
            foreach (var declaration in root.DescendantNodes().OfType<FunctionStatementSyntax>())
            {
                diagnosticSyntax = declaration;
                if (declaration.Ancestors().OfType<FunctionStatementSyntax>().Any() || declaration.Body is null || declaration.AttributeLists.Count != 0 || declaration.Modifiers.Count != 0)
                    throw Unsupported("only top-level block-bodied functions");
                var symbol = model.GetDeclaredSymbol(declaration) as IMethodSymbol ?? throw Unsupported("function symbol unavailable");
                CheckSignature(symbol);
                methods.Add((model, symbol, declaration, assembly.AddFunction(symbol.Name, symbol.Parameters.Length)));
            }
        }
        var entry = compilation.GetEntryPoint() ?? throw Unsupported("entry point unavailable");
        assembly.EntryPoint = methods.SingleOrDefault(m => SymbolEqualityComparer.Default.Equals(m.Symbol, entry)).Method
            ?? throw Unsupported("entry must be a declared top-level Int32 function");
        foreach (var current in methods)
        {
            diagnosticSyntax = current.Syntax;
            var body = current.Model.GetOperation(current.Syntax.Body!) as IBlockOperation ?? throw Unsupported("function operation body unavailable");
            foreach (var statement in body.Operations)
            {
                diagnosticSyntax = statement.Syntax;
                if (statement is not IReturnOperation { ReturnedValue: { } value }) throw Unsupported("only value-return statements");
                EmitValue(value, current.Symbol, current.Method);
                current.Method.Return();
            }
        }
        return assembly.WriteNativeAssembly();

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
            var definitions = types[0].Methods.Where(m => m.Name == symbol.MetadataName && m.TryGetStaticInt32Signature(out var count, out var result) && count == symbol.Parameters.Length && result).Take(2).ToArray();
            if (definitions.Length != 1) throw Unsupported("dependency method contract unavailable or ambiguous");
            return assembly.ImportReference(definitions[0], binding.CoreLibrary);
        }
        UnsupportedInputException Unsupported(string detail) => new(detail, diagnosticSyntax.GetLocation());

        void CheckSignature(IMethodSymbol method)
        {
            if (method.IsGenericMethod || method.IsExtensionMethod || method.IsAsync || method.ReturnType.SpecialType != SpecialType.System_Int32 ||
                method.Parameters.Any(p => p.Type.SpecialType != SpecialType.System_Int32 || p.RefKind != RefKind.None || p.HasExplicitDefaultValue || p.IsVarParams))
                throw Unsupported("only nongeneric Int32 value signatures: " + method.Name + " (" + string.Join(", ", method.Parameters.Select(p => $"{p.Type.SpecialType}, default={p.HasExplicitDefaultValue}, params={p.IsVarParams}, ref={p.RefKind}")) + ")");
        }
    }
}

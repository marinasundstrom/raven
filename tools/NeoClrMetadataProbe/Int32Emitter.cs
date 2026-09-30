using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using MetadataAssembly = NeoCLR.Metadata.Experimental.Model.AssemblyDefinition;
using MetadataMethod = NeoCLR.Metadata.Experimental.Model.MethodBuilder;
using OperatorKind = Raven.CodeAnalysis.Operations.BinaryOperatorKind;

namespace NeoClrMetadataProbe;

// Experimental consumer, not an installed Compilation.Emit backend. It consumes public
// semantic operations; no bound nodes, reflection emit or source-token operator guessing.
internal static class Int32Emitter
{
    internal static byte[] Emit(Compilation compilation, SyntaxTree tree, AssemblyIdentity core,
        AssemblyBuilder dependency, MetadataAssembly dependencyMetadata)
    {
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (errors.Length != 0) throw new InvalidDataException(string.Join("\n", errors.Select(d => d.ToString())));
        var model = compilation.GetSemanticModel(tree);
        var assembly = new AssemblyBuilder(new(compilation.AssemblyName!, new Version(1, 0, 0, 0)), core);
        var root = (CompilationUnitSyntax)tree.GetRoot();
        if (root.AttributeLists.Count != 0 || root.Members.Any(member => member is not GlobalStatementSyntax { Statement: FunctionStatementSyntax }))
            throw Unsupported("only top-level function declarations");
        var declarations = root.DescendantNodes().OfType<FunctionStatementSyntax>().ToArray();
        var methods = new List<(IMethodSymbol Symbol, FunctionStatementSyntax Syntax, MetadataMethod Method)>();
        foreach (var declaration in declarations)
        {
            if (declaration.Ancestors().OfType<FunctionStatementSyntax>().Any() || declaration.Body is null || declaration.AttributeLists.Count != 0 || declaration.Modifiers.Count != 0)
                throw Unsupported("only top-level block-bodied functions");
            var symbol = model.GetDeclaredSymbol(declaration) as IMethodSymbol ?? throw Unsupported("function symbol unavailable");
            CheckSignature(symbol);
            methods.Add((symbol, declaration, assembly.AddFunction(symbol.Name, symbol.Parameters.Length)));
        }
        var entry = compilation.GetEntryPoint() ?? throw Unsupported("entry point unavailable");
        assembly.EntryPoint = methods.SingleOrDefault(m => SymbolEqualityComparer.Default.Equals(m.Symbol, entry)).Method
            ?? throw Unsupported("entry must be a declared top-level Int32 function");
        foreach (var current in methods)
        {
            var body = model.GetOperation(current.Syntax.Body!) as IBlockOperation ?? throw Unsupported("function operation body unavailable");
            foreach (var statement in body.Operations)
            {
                if (statement is not IReturnOperation { ReturnedValue: { } value }) throw Unsupported("only value-return statements");
                EmitValue(value, current.Symbol, current.Method);
                current.Method.Return();
            }
        }
        return assembly.WriteNativeAssembly();

        void EmitValue(IOperation operation, IMethodSymbol source, MetadataMethod output)
        {
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
                    var target = local ?? Import(call.TargetMethod);
                    foreach (var argument in call.Arguments)
                    {
                        if (argument is IArgumentOperation { IsNamed: false, Value: { } argumentValue }) EmitValue(argumentValue, source, output);
                        else throw Unsupported("named or unavailable argument");
                    }
                    output.Call(target); return;
                default: throw Unsupported("operation " + operation.Kind);
            }
        }
        MetadataMethod Import(IMethodSymbol symbol)
        {
            if (!symbol.IsStatic || symbol.ContainingAssembly?.Name != dependencyMetadata.Name) throw Unsupported("unregistered dependency");
            var type = dependencyMetadata.MainModule.Types.SingleOrDefault(t => (t.Namespace.Length == 0 ? t.Name : t.Namespace + "." + t.Name) == symbol.ContainingType?.ToFullyQualifiedMetadataName())
                ?? throw Unsupported("dependency type unavailable");
            var definition = type.Methods.SingleOrDefault(m => m.Name == symbol.MetadataName && m.TryGetStaticInt32Signature(out var count, out var result) && count == symbol.Parameters.Length && result)
                ?? throw Unsupported("dependency method contract unavailable");
            return dependency.Types.Single(t => t.Name == type.Name && t.Namespace == type.Namespace).Methods.Single(m => m.Name == definition.Name && m.ParameterCount == symbol.Parameters.Length && m.ReturnsValue);
        }
    }
    private static void CheckSignature(IMethodSymbol method)
    {
        if (method.IsGenericMethod || method.IsExtensionMethod || method.IsAsync || method.ReturnType.SpecialType != SpecialType.System_Int32 ||
            method.Parameters.Any(p => p.Type.SpecialType != SpecialType.System_Int32 || p.RefKind != RefKind.None || p.HasExplicitDefaultValue || p.IsVarParams))
            throw Unsupported("only nongeneric Int32 value signatures: " + method.Name + " (" + string.Join(", ", method.Parameters.Select(p => $"{p.Type.SpecialType}, default={p.HasExplicitDefaultValue}, params={p.IsVarParams}, ref={p.RefKind}")) + ")");
    }
    private static InvalidDataException Unsupported(string detail) => new("NEOMETA001: unsupported probe input: " + detail);
}

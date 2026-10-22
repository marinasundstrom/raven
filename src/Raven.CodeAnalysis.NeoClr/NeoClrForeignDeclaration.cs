using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.NeoClr;

// Native imports belong to the target contract. CLI extern/DllImport semantics stay separate.
internal static class NeoClrForeignDeclaration
{
    internal const string AttributeName = "System.Runtime.InteropServices.NativeImportAttribute";

    internal static bool TryCreate(NeoClrEmitOptions options, IMethodSymbol symbol, SyntaxNode syntax,
        out SourceCallablePlan? plan, out NativeImportDefinition? import)
    {
        plan = null;
        import = null;
        if (!symbol.IsExtern || !symbol.IsStatic || symbol.Arity != 0 || symbol.ContainingType?.Arity > 0 ||
            symbol.MethodKind != MethodKind.Ordinary || symbol.IsAsync || symbol.IsVirtual || symbol.IsAbstract ||
            symbol.Parameters.Any(p => p.RefKind != RefKind.None || p.IsVarParams)) return false;
        if (syntax is FunctionStatementSyntax { Body: not null } or FunctionStatementSyntax { ExpressionBody: not null } or
            MethodDeclarationSyntax { Body: not null } or MethodDeclarationSyntax { ExpressionBody: not null }) return false;
        var attributes = symbol.GetAttributes();
        if (attributes.Length != 1) return false;
        var attribute = attributes[0];
        if (attribute.AttributeClass.ToFullyQualifiedMetadataName() != AttributeName ||
            !(NeoClrBindingContract.MatchesCore(attribute.AttributeClass.ContainingAssembly, options.CoreLibrary) ||
              options.Dependencies.Any(d => d.Identity.Name == "System.Runtime" &&
                  NeoClrBindingContract.MatchesCore(attribute.AttributeClass.ContainingAssembly, d.Identity))) ||
            attribute.ConstructorArguments is not [{ Value: string library }] ||
            !CallableSignature.TryCreate(symbol, out var signature, NeoClrCapabilities.Shared)) return false;
        bool Trusted(AttributeData data) => data.AttributeClass.ToFullyQualifiedMetadataName() == "System.Runtime.InteropServices.MarshalAsAttribute" &&
            SymbolEqualityComparer.Default.Equals(data.AttributeClass.ContainingAssembly, attribute.AttributeClass.ContainingAssembly);
        string? Marshal(System.Collections.Immutable.ImmutableArray<AttributeData> values)
        {
            if (values.IsEmpty) return null;
            if (values.Length != 1 || !Trusted(values[0]) || values[0].NamedArguments.Length != 0 || values[0].ConstructorArguments is not [{ Value: int kind }])
                throw new ArgumentException("unsupported marshalling attribute");
            return kind switch { 0 => "Utf8", 1 => "Utf16", 2 => "Bool8", 3 => "Bool32", _ => throw new ArgumentException("unsupported native value representation") };
        }
        var entry = symbol.Name;
        var convention = "Cdecl";
        string? encoding = null;
        foreach (var argument in attribute.NamedArguments)
        {
            switch (argument.Key, argument.Value.Value)
            {
                case ("EntryPoint", string name) when name.Length > 0: entry = name; break;
                case ("CallingConvention", 0): break; // NativeCallingConvention.Cdecl
                case ("CallingConvention", 1): convention = "System"; break;
                case ("StringMarshalling", 0): encoding = null; break;
                case ("StringMarshalling", 1): encoding = "Utf8"; break;
                case ("StringMarshalling", 2): encoding = "Utf16"; break;
                default: return false;
            }
        }
        try
        {
            var parameters = new Dictionary<int, string>();
            for (var i = 0; i < symbol.Parameters.Length; i++)
                if (Marshal(symbol.Parameters[i].GetAttributes()) is { } conversion) parameters.Add(i, conversion);
            var result = symbol is SourceMethodSymbol source ? Marshal(source.GetReturnTypeAttributes()) : null;
            var callbacks = new Dictionary<int, NativeCallbackDefinition>();
            for (var i = 0; i < symbol.Parameters.Length; i++)
                if (symbol.Parameters[i].Type is INamedTypeSymbol callable && callable.GetDelegateInvokeMethod() is not null)
                    callbacks.Add(i, new NativeCallbackDefinition(convention));
            import = new(library, entry, encoding, convention, parameters, result, callbacks);
        }
        catch (ArgumentException) { return false; }
        var owner = syntax is FunctionStatementSyntax ? null : symbol.ContainingType;
        plan = new(symbol, syntax, null, owner, symbol.MetadataName, signature);
        if (!plan.IsSupportedBy(NeoClrCapabilities.Shared)) { plan = null; import = null; return false; }
        return true;
    }
}

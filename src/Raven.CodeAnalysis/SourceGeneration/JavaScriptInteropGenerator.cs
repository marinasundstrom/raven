using System.Text;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

/// <summary>
/// Generates the low-level browser WebAssembly bindings for Raven methods marked
/// with <c>System.Runtime.InteropServices.JavaScript.JSImportAttribute</c> or
/// <c>System.Runtime.InteropServices.JavaScript.JSExportAttribute</c>.
/// </summary>
public sealed class JavaScriptInteropGenerator : ISourceGenerator
{
    private static readonly DiagnosticDescriptor s_unsupportedDeclaration = DiagnosticDescriptor.Create(
        "RVNJS001",
        "Unsupported JavaScript import",
        "JavaScript imports currently support static partial methods returning unit with string and Action<string> parameters.",
        string.Empty,
        "JavaScript import '{0}' is not supported by the current Raven interop generator: {1}",
        "Interop",
        DiagnosticSeverity.Error);

    private static readonly DiagnosticDescriptor s_unsupportedExport = DiagnosticDescriptor.Create(
        "RVNJS002",
        "Unsupported JavaScript export",
        "JavaScript exports currently support static methods with bodies, string return values, and string parameters.",
        string.Empty,
        "JavaScript export '{0}' is not supported by the current Raven interop generator: {1}",
        "Interop",
        DiagnosticSeverity.Error);

    public void Initialize(GeneratorInitializationContext context)
    {
    }

    public void Execute(GeneratorExecutionContext context)
    {
        var exports = new List<ExportMethod>();

        foreach (var tree in context.Compilation.SyntaxTrees)
        {
            context.CancellationToken.ThrowIfCancellationRequested();
            var semanticModel = context.Compilation.GetSemanticModel(tree);

            foreach (var declaration in tree.GetRoot(context.CancellationToken)
                         .DescendantNodes()
                         .OfType<MethodDeclarationSyntax>())
            {
                if (semanticModel.GetDeclaredSymbol(declaration) is not IMethodSymbol method)
                {
                    continue;
                }

                if (TryGetJSImportSyntax(declaration, out var importAttribute) && HasJSImportAttribute(method))
                {
                    if (!TryGetJSImportArguments(importAttribute, out var functionName, out var moduleName))
                    {
                        context.ReportDiagnostic(Diagnostic.Create(
                            s_unsupportedDeclaration,
                            tree.GetLocation(declaration.Identifier.Span),
                            method.Name,
                            "the attribute requires constant function and module name strings"));
                    }
                    else if (!TryCreateImport(method, declaration, functionName, moduleName, out var source, out var reason))
                    {
                        context.ReportDiagnostic(Diagnostic.Create(
                            s_unsupportedDeclaration,
                            tree.GetLocation(declaration.Identifier.Span),
                            method.Name,
                            reason));
                    }
                    else
                    {
                        var containingTypeName = method.ContainingType!.ToDisplayString().Replace('.', '_');
                        context.AddSource($"JavaScriptInterop/{containingTypeName}_{method.Name}_{declaration.Span.Start}.g.rvn", source);
                    }
                }

                if (!TryGetJSExportSyntax(declaration, out _) || !HasJSExportAttribute(method))
                    continue;

                if (!TryCreateExport(method, declaration, out var export, out var exportReason))
                {
                    context.ReportDiagnostic(Diagnostic.Create(
                        s_unsupportedExport,
                        tree.GetLocation(declaration.Identifier.Span),
                        method.Name,
                        exportReason));
                    continue;
                }

                exports.Add(export);
            }
        }

        if (exports.Count > 0)
        {
            context.AddSource(
                "JavaScriptInterop/JSExports.g.rvn",
                RenderExports(context.Compilation.AssemblyName, exports));
        }
    }

    internal static bool HasCandidate(Compilation compilation)
        => compilation.SyntaxTrees.Any(static tree =>
            tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Any(HasInteropCandidate));

    private static bool HasInteropCandidate(MethodDeclarationSyntax declaration)
        => TryGetJSImportSyntax(declaration, out _) || TryGetJSExportSyntax(declaration, out _);

    private static bool TryGetJSImportSyntax(MethodDeclarationSyntax declaration, out AttributeSyntax attributeSyntax)
    {
        attributeSyntax = declaration.AttributeLists
            .SelectMany(static list => list.Attributes)
            .FirstOrDefault(static attribute =>
            {
                var name = attribute.Name.ToString();
                return name is "JSImport" or "JSImportAttribute" ||
                       name.EndsWith(".JSImport", StringComparison.Ordinal) ||
                       name.EndsWith(".JSImportAttribute", StringComparison.Ordinal);
            });
        return attributeSyntax is not null;
    }

    private static bool TryGetJSExportSyntax(MethodDeclarationSyntax declaration, out AttributeSyntax attributeSyntax)
    {
        attributeSyntax = declaration.AttributeLists
            .SelectMany(static list => list.Attributes)
            .FirstOrDefault(static attribute =>
            {
                var name = attribute.Name.ToString();
                return name is "JSExport" or "JSExportAttribute" ||
                       name.EndsWith(".JSExport", StringComparison.Ordinal) ||
                       name.EndsWith(".JSExportAttribute", StringComparison.Ordinal);
            });
        return attributeSyntax is not null;
    }

    private static bool TryGetJSImportArguments(
        AttributeSyntax attribute,
        out string functionName,
        out string moduleName)
    {
        if (attribute.ArgumentList?.Arguments is
            [
            { Expression: LiteralExpressionSyntax { Token.Value: string function } },
            { Expression: LiteralExpressionSyntax { Token.Value: string module } }
            ])
        {
            functionName = function;
            moduleName = module;
            return true;
        }

        functionName = string.Empty;
        moduleName = string.Empty;
        return false;
    }

    private static bool HasJSImportAttribute(IMethodSymbol method)
        => method.GetAttributes().Any(static attribute =>
            attribute.AttributeClass.ToFullyQualifiedMetadataName() ==
            "System.Runtime.InteropServices.JavaScript.JSImportAttribute");

    private static bool HasJSExportAttribute(IMethodSymbol method)
        => method.GetAttributes().Any(static attribute =>
            attribute.AttributeClass.ToFullyQualifiedMetadataName() ==
            "System.Runtime.InteropServices.JavaScript.JSExportAttribute");

    private static bool TryCreateImport(
        IMethodSymbol method,
        MethodDeclarationSyntax declaration,
        string functionName,
        string moduleName,
        out string source,
        out string reason)
    {
        source = string.Empty;
        reason = string.Empty;

        if (method.ContainingType is not { } containingType || containingType.ContainingType is not null)
        {
            reason = "the containing type must be a top-level class";
            return false;
        }

        if (containingType.TypeKind != TypeKind.Class || containingType.Arity != 0)
        {
            reason = "the containing type must be a non-generic class";
            return false;
        }

        if (!declaration.Modifiers.Any(static modifier => modifier.IsKind(SyntaxKind.StaticKeyword)) ||
            !declaration.Modifiers.Any(static modifier => modifier.IsKind(SyntaxKind.PartialKeyword)) ||
            declaration.Body is not null ||
            declaration.ExpressionBody is not null)
        {
            reason = "the declaration must be a static partial method without a body";
            return false;
        }

        if (method.IsGenericMethod || method.ReturnType.SpecialType is not (SpecialType.System_Unit or SpecialType.System_Void))
        {
            reason = "generic methods and non-unit return values are not supported";
            return false;
        }

        var parameters = new List<ImportParameter>(method.Parameters.Length);
        foreach (var parameter in method.Parameters)
        {
            if (parameter.RefKind != RefKind.None)
            {
                reason = "ref, in, and out parameters are not supported";
                return false;
            }

            if (parameter.Type.SpecialType == SpecialType.System_String)
            {
                parameters.Add(new ImportParameter(parameter.Name, "string", "JSMarshalerType.String", IsCallback: false));
                continue;
            }

            if (IsStringAction(parameter.Type))
            {
                parameters.Add(new ImportParameter(
                    parameter.Name,
                    "Action<string>",
                    "JSMarshalerType.Action(JSMarshalerType.String)",
                    IsCallback: true));
                continue;
            }

            reason = $"parameter '{parameter.Name}' has unsupported type '{parameter.Type.ToDisplayString()}'";
            return false;
        }

        source = RenderSource(method, containingType, functionName, moduleName, parameters);
        return true;
    }

    private static bool IsStringAction(ITypeSymbol type)
        => type is INamedTypeSymbol
        {
            Name: "Action",
            Arity: 1,
            ContainingNamespace: { } containingNamespace,
            TypeArguments: [{ SpecialType: SpecialType.System_String }]
        } && containingNamespace.ToDisplayString() == "System";

    private static string RenderSource(
        IMethodSymbol method,
        INamedTypeSymbol containingType,
        string functionName,
        string moduleName,
        IReadOnlyList<ImportParameter> parameters)
    {
        var builder = new StringBuilder();
        builder.AppendLine("import System.*");
        builder.AppendLine("import System.Runtime.InteropServices.JavaScript.*");
        builder.AppendLine();

        var namespaceName = containingType.ContainingNamespace is { IsGlobalNamespace: false } containingNamespace
            ? containingNamespace.ToDisplayString()
            : null;
        if (namespaceName is not null)
        {
            builder.Append("namespace ").Append(namespaceName).AppendLine(" {");
            builder.AppendLine();
        }

        var outerIndent = namespaceName is null ? string.Empty : "    ";
        var memberIndent = outerIndent + "    ";
        builder.Append(outerIndent).Append("partial class ").Append(containingType.Name).AppendLine(" {");

        foreach (var callback in parameters.Where(static parameter => parameter.IsCallback))
        {
            builder.Append(memberIndent).Append("private static func __ReadString_").Append(method.Name).Append('_')
                .Append(callback.Name).AppendLine("(ref argument: JSMarshalerArgument, out value: string) {");
            builder.Append(memberIndent).AppendLine("    value = \"\"");
            builder.Append(memberIndent).AppendLine("    argument.ToManaged(out value)");
            builder.Append(memberIndent).AppendLine("}");
            builder.AppendLine();
        }

        builder.Append(memberIndent).Append("static partial func ").Append(method.Name).Append('(')
            .Append(string.Join(", ", parameters.Select(static parameter => $"{parameter.Name}: {parameter.TypeName}")))
            .AppendLine(") {");
        builder.Append(memberIndent).AppendLine("    let signature = JSFunctionBinding.BindJSFunction(");
        builder.Append(memberIndent).Append("        \"").Append(EscapeString(functionName)).AppendLine("\",");
        builder.Append(memberIndent).Append("        \"").Append(EscapeString(moduleName)).AppendLine("\",");
        builder.Append(memberIndent).AppendLine("        [");
        builder.Append(memberIndent).Append("            JSMarshalerType.Discard");
        foreach (var parameter in parameters)
            builder.AppendLine(",").Append(memberIndent).Append("            ").Append(parameter.MarshalerType);
        builder.AppendLine();
        builder.Append(memberIndent).AppendLine("        ]");
        builder.Append(memberIndent).AppendLine("    )");
        builder.AppendLine();
        builder.Append(memberIndent).AppendLine("    var exceptionArgument = default(JSMarshalerArgument)");
        builder.Append(memberIndent).AppendLine("    var returnArgument = default(JSMarshalerArgument)");
        foreach (var parameter in parameters)
            builder.Append(memberIndent).Append("    var ").Append(parameter.Name).AppendLine("Argument = default(JSMarshalerArgument)");
        builder.AppendLine();
        builder.Append(memberIndent).AppendLine("    exceptionArgument.Initialize()");
        builder.Append(memberIndent).AppendLine("    returnArgument.Initialize()");
        foreach (var parameter in parameters)
        {
            builder.Append(memberIndent).Append("    ").Append(parameter.Name).Append("Argument.ToJS(").Append(parameter.Name);
            if (parameter.IsCallback)
                builder.Append(", __ReadString_").Append(method.Name).Append('_').Append(parameter.Name);
            builder.AppendLine(")");
        }

        builder.AppendLine();
        builder.Append(memberIndent).AppendLine("    JSFunctionBinding.InvokeJS(");
        builder.Append(memberIndent).AppendLine("        signature,");
        builder.Append(memberIndent).Append("        [exceptionArgument, returnArgument");
        foreach (var parameter in parameters)
            builder.Append(", ").Append(parameter.Name).Append("Argument");
        builder.AppendLine("]");
        builder.Append(memberIndent).AppendLine("    )");
        builder.Append(memberIndent).AppendLine("}");
        builder.Append(outerIndent).AppendLine("}");

        if (namespaceName is not null)
            builder.AppendLine("}");

        return builder.ToString();
    }

    private static bool TryCreateExport(
        IMethodSymbol method,
        MethodDeclarationSyntax declaration,
        out ExportMethod export,
        out string reason)
    {
        export = default;
        reason = string.Empty;

        if (method.ContainingType is not { } containingType || containingType.ContainingType is not null)
        {
            reason = "the containing type must be a top-level class";
            return false;
        }

        if (containingType.TypeKind != TypeKind.Class || containingType.Arity != 0)
        {
            reason = "the containing type must be a non-generic class";
            return false;
        }

        if (declaration.Parent is not ClassDeclarationSyntax containingDeclaration ||
            !containingDeclaration.Modifiers.Any(static modifier => modifier.IsKind(SyntaxKind.PartialKeyword)))
        {
            reason = "the containing class must be partial";
            return false;
        }

        if (!declaration.Modifiers.Any(static modifier => modifier.IsKind(SyntaxKind.StaticKeyword)) ||
            declaration.Body is null && declaration.ExpressionBody is null)
        {
            reason = "the declaration must be a static method with a body";
            return false;
        }

        if (method.IsGenericMethod || method.ReturnType.SpecialType != SpecialType.System_String)
        {
            reason = "generic methods and return values other than string are not supported";
            return false;
        }

        var parameters = new List<string>(method.Parameters.Length);
        foreach (var parameter in method.Parameters)
        {
            if (parameter.RefKind != RefKind.None || parameter.Type.SpecialType != SpecialType.System_String)
            {
                reason = $"parameter '{parameter.Name}' has unsupported type '{parameter.Type.ToDisplayString()}'";
                return false;
            }

            parameters.Add(parameter.Name);
        }

        var namespaceName = containingType.ContainingNamespace is { IsGlobalNamespace: false } containingNamespace
            ? containingNamespace.ToDisplayString()
            : null;
        var typeFullName = namespaceName is null
            ? containingType.Name
            : $"{namespaceName}.{containingType.Name}";
        var typesHash = ComputeTypesHash(method.Parameters.Length + 1);

        export = new ExportMethod(
            method.Name,
            containingType.Name,
            typeFullName,
            namespaceName,
            parameters,
            typesHash);
        return true;
    }

    private static int ComputeTypesHash(int stringTypeCount)
    {
        uint hash = 17;
        for (var index = 0; index < stringTypeCount; index++)
        {
            foreach (var character in "string")
                hash = unchecked(hash * 31 + character);
        }

        return (int)(hash & 0x7FFFFFFF);
    }

    private static string RenderExports(string assemblyName, IReadOnlyList<ExportMethod> exports)
    {
        var builder = new StringBuilder();
        builder.AppendLine("import System.*");
        builder.AppendLine("import System.Diagnostics.CodeAnalysis.*");
        builder.AppendLine("import System.Runtime.CompilerServices.*");
        builder.AppendLine("import System.Runtime.InteropServices.*");
        builder.AppendLine("import System.Runtime.InteropServices.JavaScript.*");
        builder.AppendLine();
        builder.AppendLine("namespace System.Runtime.InteropServices.JavaScript {");
        builder.AppendLine("    [CompilerGenerated]");
        builder.AppendLine("    class __GeneratedInitializer {");
        builder.AppendLine("        static field initialized: bool = false");
        builder.AppendLine();
        builder.AppendLine("        [ModuleInitializer, DynamicDependency(");
        builder.AppendLine("            DynamicallyAccessedMemberTypes.PublicMethods | DynamicallyAccessedMemberTypes.NonPublicMethods,");
        builder.AppendLine("            \"System.Runtime.InteropServices.JavaScript.__GeneratedInitializer\",");
        builder.Append("            \"").Append(EscapeString(assemblyName)).AppendLine("\"");
        builder.AppendLine("        )]");
        builder.AppendLine("        static func __TrimmingPreserve_() {}");
        builder.AppendLine();
        foreach (var export in exports)
        {
            builder.Append("        [DynamicDependency(\"").Append(export.WrapperName).Append("\", \"")
                .Append(EscapeString(export.TypeFullName)).Append("\", \"")
                .Append(EscapeString(assemblyName)).AppendLine("\")]");
        }

        builder.AppendLine("        static func __Register_() {");
        builder.AppendLine("            if initialized || RuntimeInformation.OSArchitecture != Architecture.Wasm {");
        builder.AppendLine("                return");
        builder.AppendLine("            }");
        builder.AppendLine("            initialized = true");
        foreach (var export in exports)
        {
            builder.AppendLine();
            builder.Append("            _ = JSFunctionBinding.BindManagedFunction(\"[")
                .Append(EscapeString(assemblyName)).Append(']')
                .Append(EscapeString(export.TypeFullName)).Append(':').Append(export.MethodName).AppendLine("\",");
            builder.Append("                ").Append(export.TypesHash).AppendLine(",");
            builder.Append("                [JSMarshalerType.String");
            foreach (var _ in export.Parameters)
                builder.Append(", JSMarshalerType.String");
            builder.AppendLine("]");
            builder.AppendLine("            )");
        }

        builder.AppendLine("        }");
        builder.AppendLine("    }");
        builder.AppendLine("}");

        foreach (var export in exports)
        {
            builder.AppendLine();
            if (export.NamespaceName is not null)
            {
                builder.Append("namespace ").Append(export.NamespaceName).AppendLine(" {");
                builder.AppendLine();
            }

            var outerIndent = export.NamespaceName is null ? string.Empty : "    ";
            var memberIndent = outerIndent + "    ";
            builder.Append(outerIndent).Append("partial class ").Append(export.TypeName).AppendLine(" {");
            builder.Append(memberIndent).Append("unsafe static func ").Append(export.WrapperName)
                .AppendLine("(__arguments_buffer: *JSMarshalerArgument) {");
            foreach (var parameter in export.Parameters)
                builder.Append(memberIndent).Append("    var ").Append(parameter).AppendLine(": string = \"\"");
            builder.Append(memberIndent).AppendLine("    try {");
            for (var index = 0; index < export.Parameters.Count; index++)
            {
                builder.Append(memberIndent).Append("        (__arguments_buffer + ").Append(index + 2)
                    .Append(")->ToManaged(out ").Append(export.Parameters[index]).AppendLine(")");
            }

            builder.Append(memberIndent).Append("        let result = ").Append(export.MethodName).Append('(')
                .Append(string.Join(", ", export.Parameters)).AppendLine(")");
            builder.Append(memberIndent).AppendLine("        (__arguments_buffer + 1)->ToJS(result)");
            builder.Append(memberIndent).AppendLine("    } catch (Exception exception) {");
            builder.Append(memberIndent).AppendLine("        __arguments_buffer->ToJS(exception)");
            builder.Append(memberIndent).AppendLine("    }");
            builder.Append(memberIndent).AppendLine("}");
            builder.Append(outerIndent).AppendLine("}");

            if (export.NamespaceName is not null)
                builder.AppendLine("}");
        }

        return builder.ToString();
    }

    private static string EscapeString(string value)
        => value.Replace("\\", "\\\\", StringComparison.Ordinal).Replace("\"", "\\\"", StringComparison.Ordinal);

    private readonly record struct ImportParameter(
        string Name,
        string TypeName,
        string MarshalerType,
        bool IsCallback);

    private readonly record struct ExportMethod(
        string MethodName,
        string TypeName,
        string TypeFullName,
        string? NamespaceName,
        IReadOnlyList<string> Parameters,
        int TypesHash)
    {
        public string WrapperName => $"__Wrapper_{MethodName}_{TypesHash}";
    }
}

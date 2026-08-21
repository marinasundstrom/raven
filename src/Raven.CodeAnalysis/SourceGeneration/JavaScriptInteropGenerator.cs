using System.Text;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

/// <summary>
/// Generates the low-level browser WebAssembly bindings for Raven methods marked
/// with <c>System.Runtime.InteropServices.JavaScript.JSImportAttribute</c>.
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

    public void Initialize(GeneratorInitializationContext context)
    {
    }

    public void Execute(GeneratorExecutionContext context)
    {
        foreach (var tree in context.Compilation.SyntaxTrees)
        {
            context.CancellationToken.ThrowIfCancellationRequested();
            var semanticModel = context.Compilation.GetSemanticModel(tree);

            foreach (var declaration in tree.GetRoot(context.CancellationToken)
                         .DescendantNodes()
                         .OfType<MethodDeclarationSyntax>())
            {
                if (!TryGetJSImportSyntax(declaration, out var importAttribute) ||
                    semanticModel.GetDeclaredSymbol(declaration) is not IMethodSymbol method ||
                    !HasJSImportAttribute(method))
                {
                    continue;
                }

                if (!TryGetJSImportArguments(importAttribute, out var functionName, out var moduleName))
                {
                    context.ReportDiagnostic(Diagnostic.Create(
                        s_unsupportedDeclaration,
                        tree.GetLocation(declaration.Identifier.Span),
                        method.Name,
                        "the attribute requires constant function and module name strings"));
                    continue;
                }

                if (!TryCreateImport(method, declaration, functionName, moduleName, out var source, out var reason))
                {
                    context.ReportDiagnostic(Diagnostic.Create(
                        s_unsupportedDeclaration,
                        tree.GetLocation(declaration.Identifier.Span),
                        method.Name,
                        reason));
                    continue;
                }

                var containingTypeName = method.ContainingType!.ToDisplayString().Replace('.', '_');
                context.AddSource($"JavaScriptInterop/{containingTypeName}_{method.Name}_{declaration.Span.Start}.g.rvn", source);
            }
        }
    }

    internal static bool HasCandidate(Compilation compilation)
        => compilation.SyntaxTrees.Any(static tree =>
            tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Any(HasJSImportCandidate));

    private static bool HasJSImportCandidate(MethodDeclarationSyntax declaration)
        => TryGetJSImportSyntax(declaration, out _);

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

    private static string EscapeString(string value)
        => value.Replace("\\", "\\\\", StringComparison.Ordinal).Replace("\"", "\\\"", StringComparison.Ordinal);

    private readonly record struct ImportParameter(
        string Name,
        string TypeName,
        string MarshalerType,
        bool IsCallback);
}

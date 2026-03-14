using System;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

internal static class MacroSemanticValidator
{
    private static readonly DiagnosticDescriptor s_unknownMacro = DiagnosticDescriptor.Create(
        "RAVM010",
        "Unknown macro",
        "",
        "",
        "Macro '{0}' could not be resolved. Add a matching Raven macro plugin reference.",
        "compiler",
        DiagnosticSeverity.Error,
        true);

    private static readonly DiagnosticDescriptor s_macroTargetNotSupported = DiagnosticDescriptor.Create(
        "RAVM011",
        "Invalid macro target",
        "",
        "",
        "Macro '{0}' is not valid on {1}.",
        "compiler",
        DiagnosticSeverity.Error,
        true);

    private static readonly DiagnosticDescriptor s_macroArgumentsNotSupported = DiagnosticDescriptor.Create(
        "RAVM012",
        "Macro arguments not supported",
        "",
        "",
        "Macro '{0}' does not accept arguments.",
        "compiler",
        DiagnosticSeverity.Error,
        true);

    public static void ValidateAttribute(
        Compilation compilation,
        AttributeSyntax attribute,
        SyntaxNode targetDeclaration,
        DiagnosticBag diagnostics)
    {
        _ = TryResolveAttachedMacro(compilation, attribute, targetDeclaration, diagnostics, out _);
    }

    public static bool TryResolveAttachedMacro(
        Compilation compilation,
        AttributeSyntax attribute,
        SyntaxNode targetDeclaration,
        DiagnosticBag? diagnostics,
        out LoadedAttachedMacro loaded)
    {
        if (attribute is null)
            throw new ArgumentNullException(nameof(attribute));

        if (targetDeclaration is null)
            throw new ArgumentNullException(nameof(targetDeclaration));

        if (!attribute.TryGetMacroName(out var macroName))
        {
            loaded = default;
            return false;
        }

        var registry = compilation.GetMacroRegistry();
        if (!registry.TryResolveAttachedMacro(macroName, out loaded))
        {
            diagnostics?.Report(Diagnostic.Create(s_unknownMacro, attribute.Name.GetLocation(), macroName));
            return false;
        }

        var actualTarget = GetTarget(targetDeclaration);
        if (actualTarget == MacroTarget.None || (loaded.Macro.Targets & actualTarget) == 0)
        {
            diagnostics?.Report(Diagnostic.Create(
                s_macroTargetNotSupported,
                attribute.Name.GetLocation(),
                macroName,
                DescribeTarget(targetDeclaration)));
            return false;
        }

        if (attribute.ArgumentList is { Arguments.Count: > 0 } && !loaded.Macro.AcceptsArguments)
        {
            diagnostics?.Report(Diagnostic.Create(
                s_macroArgumentsNotSupported,
                attribute.ArgumentList.GetLocation(),
                macroName));
            return false;
        }

        return true;
    }

    public static bool TryResolveFreestandingMacro(
        Compilation compilation,
        FreestandingMacroExpressionSyntax expression,
        DiagnosticBag? diagnostics,
        out LoadedFreestandingMacro loaded)
    {
        ArgumentNullException.ThrowIfNull(expression);

        if (!expression.TryGetMacroName(out var macroName))
        {
            loaded = default;
            return false;
        }

        var registry = compilation.GetMacroRegistry();
        if (!registry.TryResolveFreestandingMacro(macroName, out loaded))
        {
            diagnostics?.Report(Diagnostic.Create(s_unknownMacro, expression.Name.GetLocation(), macroName));
            return false;
        }

        if (expression.ArgumentList.Arguments.Count > 0 && !loaded.Macro.AcceptsArguments)
        {
            diagnostics?.Report(Diagnostic.Create(
                s_macroArgumentsNotSupported,
                expression.ArgumentList.GetLocation(),
                macroName));
            return false;
        }

        return true;
    }

    private static MacroTarget GetTarget(SyntaxNode targetDeclaration)
        => targetDeclaration switch
        {
            BaseTypeDeclarationSyntax => MacroTarget.Type,
            MethodDeclarationSyntax or FunctionStatementSyntax => MacroTarget.Method,
            PropertyDeclarationSyntax or IndexerDeclarationSyntax => MacroTarget.Property,
            FieldDeclarationSyntax or ConstDeclarationSyntax => MacroTarget.Field,
            EventDeclarationSyntax => MacroTarget.Event,
            ParameterSyntax => MacroTarget.Parameter,
            AccessorDeclarationSyntax => MacroTarget.Accessor,
            ConstructorDeclarationSyntax or ParameterlessConstructorDeclarationSyntax => MacroTarget.Constructor,
            _ => MacroTarget.None
        };

    private static string DescribeTarget(SyntaxNode targetDeclaration)
        => targetDeclaration switch
        {
            BaseTypeDeclarationSyntax => "type declarations",
            MethodDeclarationSyntax or FunctionStatementSyntax => "methods",
            PropertyDeclarationSyntax or IndexerDeclarationSyntax => "properties",
            FieldDeclarationSyntax or ConstDeclarationSyntax => "fields",
            EventDeclarationSyntax => "events",
            ParameterSyntax => "parameters",
            AccessorDeclarationSyntax => "accessors",
            ConstructorDeclarationSyntax or ParameterlessConstructorDeclarationSyntax => "constructors",
            _ => "this declaration"
        };
}

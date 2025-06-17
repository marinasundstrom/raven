using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace TypeUnionAnalyzer;

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public class TypeUnionParameterAnalyzer : DiagnosticAnalyzer
{
    private static readonly DiagnosticDescriptor Rule = new DiagnosticDescriptor(
        id: "TU001",
        title: "TypeUnion usage detected",
        messageFormat: "{0} {1}",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Info,
        isEnabledByDefault: true
    );

    private static readonly DiagnosticDescriptor IncompatibleTypeRule = new DiagnosticDescriptor(
        id: "TU002",
        title: "Type does not match TypeUnion",
        messageFormat: "Value must be compatible with one of types {1}",
        category: "TypeChecking",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true
    );

    private static readonly DiagnosticDescriptor IncompatibleReturnTypeRule = new DiagnosticDescriptor(
        id: "TU003",
        title: "Return value does not match TypeUnion",
        messageFormat: "Return value must be compatible with one of types {1}",
        category: "TypeChecking",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true
    );

    private static readonly DiagnosticDescriptor InvalidPatternRule = new DiagnosticDescriptor(
        id: "TU004",
        title: "Pattern does not match TypeUnion",
        messageFormat: "Type '{1}' is not allowed by {0}",
        category: "TypeChecking",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true
    );

    private static readonly DiagnosticDescriptor MustBeObjectTypeRule = new DiagnosticDescriptor(
        id: "TU005",
        title: "TypeUnion must be declared with object type",
        messageFormat: "{0} must be declared with type 'object' when using [TypeUnion]",
        category: "TypeChecking",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true
    );

    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics =>
        [Rule, IncompatibleTypeRule, IncompatibleReturnTypeRule, InvalidPatternRule, MustBeObjectTypeRule];

    public override void Initialize(AnalysisContext context)
    {
        context.EnableConcurrentExecution();
        context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
        context.RegisterSyntaxNodeAction(AnalyzeParameter, SyntaxKind.Parameter);
        context.RegisterSyntaxNodeAction(AnalyzeMethodReturnType, SyntaxKind.MethodDeclaration);
        context.RegisterSyntaxNodeAction(AnalyzeInvocation, SyntaxKind.InvocationExpression);
        context.RegisterSyntaxNodeAction(AnalyzeMemberAccess, SyntaxKind.SimpleMemberAccessExpression);
        context.RegisterSyntaxNodeAction(AnalyzeInvocationExpressionForTU001, SyntaxKind.InvocationExpression);
        context.RegisterSyntaxNodeAction(AnalyzeReturnStatement, SyntaxKind.ReturnStatement);
        context.RegisterSyntaxNodeAction(AnalyzeIsPattern, SyntaxKind.IsPatternExpression);
        context.RegisterSyntaxNodeAction(AnalyzeSwitchStatement, SyntaxKind.SwitchStatement);
        context.RegisterSyntaxNodeAction(AnalyzeSwitchExpression, SyntaxKind.SwitchExpression);
        context.RegisterSyntaxNodeAction(AnalyzeFieldDeclaration, SyntaxKind.FieldDeclaration);
        context.RegisterSyntaxNodeAction(AnalyzePropertyDeclaration, SyntaxKind.PropertyDeclaration);
        context.RegisterSyntaxNodeAction(AnalyzeAssignmentExpression, SyntaxKind.SimpleAssignmentExpression);
    }

    private void AnalyzeParameter(SyntaxNodeAnalysisContext context)
    {
        var parameterSyntax = (ParameterSyntax)context.Node;

        var parameterSymbol = context.SemanticModel.GetDeclaredSymbol(parameterSyntax);
        if (parameterSymbol == null)
            return;

        foreach (var attributeData in parameterSymbol.GetAttributes())
        {
            if (IsTypeUnionAttribute(attributeData))
            {
                if (IsNotUnionCompatibleType(parameterSymbol.Type, context.Compilation))
                {
                    var diag = Diagnostic.Create(
                        MustBeObjectTypeRule,
                        parameterSyntax.Type?.GetLocation() ?? parameterSyntax.GetLocation(),
                        $"Parameter '{parameterSymbol.Name}'"
                    );
                    context.ReportDiagnostic(diag);
                }

                var typeArgs = attributeData.ConstructorArguments.FirstOrDefault();
                if (typeArgs.Kind == TypedConstantKind.Array && typeArgs.Values is { Length: > 0 })
                {
                    string typeList = FormatTypeList(typeArgs);

                    var diagnostic = Diagnostic.Create(
                        Rule,
                        parameterSyntax.Identifier.GetLocation(),
                        $"Parameter \'{parameterSymbol.Name}\' expects",
                        typeList
                    );

                    context.ReportDiagnostic(diagnostic);
                }
            }
        }
    }

    private void AnalyzeMethodReturnType(SyntaxNodeAnalysisContext context)
    {
        var methodSyntax = (MethodDeclarationSyntax)context.Node;
        var methodSymbol = context.SemanticModel.GetDeclaredSymbol(methodSyntax);
        if (methodSymbol == null)
            return;

        foreach (var attribute in methodSymbol.GetReturnTypeAttributes())
        {
            if (IsTypeUnionAttribute(attribute))
            {
                if (IsNotUnionCompatibleType(methodSymbol.ReturnType, context.Compilation))
                {
                    var diag = Diagnostic.Create(
                        MustBeObjectTypeRule,
                        methodSyntax.ReturnType.GetLocation(),
                        $"Return type of method '{methodSymbol.Name}'"
                    );
                    context.ReportDiagnostic(diag);
                }

                var typeArgs = attribute.ConstructorArguments.FirstOrDefault();
                if (typeArgs.Kind == TypedConstantKind.Array && typeArgs.Values is { Length: > 0 })
                {
                    string typeList = FormatTypeList(typeArgs);

                    var diagnostic = Diagnostic.Create(
                        Rule,
                        methodSyntax.ReturnType.GetLocation(),
                        "Method returns value of types",
                        typeList
                    );

                    context.ReportDiagnostic(diagnostic);
                }
            }
        }
    }

    private void AnalyzeInvocation(SyntaxNodeAnalysisContext context)
    {
        var invocation = (InvocationExpressionSyntax)context.Node;
        var symbolInfo = context.SemanticModel.GetSymbolInfo(invocation);
        if (symbolInfo.Symbol is not IMethodSymbol methodSymbol)
            return;

        var args = invocation.ArgumentList.Arguments;

        for (int i = 0; i < args.Count && i < methodSymbol.Parameters.Length; i++)
        {
            var parameter = methodSymbol.Parameters[i];
            var argExpr = args[i].Expression;

            var unionAttr = parameter.GetAttributes()
                .FirstOrDefault(a => a.AttributeClass?.Name == "TypeUnionAttribute");

            if (unionAttr == null)
                continue;

            var allowedTypes = unionAttr.ConstructorArguments.FirstOrDefault();
            if (allowedTypes.Kind != TypedConstantKind.Array)
                continue;

            var argType = context.SemanticModel.GetTypeInfo(argExpr).Type;
            if (argType == null)
                continue;

            bool isCompatible = allowedTypes.Values.Any(tc =>
            {
                var unionType = tc.Value as ITypeSymbol;
                return unionType != null &&
                       context.SemanticModel.Compilation.ClassifyConversion(argType, unionType).IsImplicit;
            });

            if (!isCompatible)
            {
                var diagnostic = Diagnostic.Create(
                    IncompatibleTypeRule,
                    argExpr.GetLocation(),
                    parameter.Name,
                    FormatTypeList(allowedTypes),
                    argType.ToDisplayString()
                );

                context.ReportDiagnostic(diagnostic);
            }
        }
    }

    private void AnalyzeMemberAccess(SyntaxNodeAnalysisContext context)
    {
        var memberAccess = (MemberAccessExpressionSyntax)context.Node;
        var symbol = context.SemanticModel.GetSymbolInfo(memberAccess).Symbol;

        if (symbol is IPropertySymbol or IFieldSymbol)
        {
            ReportTU001IfTypeUnionFound(symbol, memberAccess.Name.GetLocation(), context);
        }
    }

    private void ReportTU001IfTypeUnionFound(ISymbol symbol, Location location, SyntaxNodeAnalysisContext context)
    {
        // Check return type (for methods)
        if (symbol is IMethodSymbol methodSymbol)
        {
            foreach (var attr in methodSymbol.GetReturnTypeAttributes())
            {
                if (IsTypeUnion(attr, out var typeArgs))
                {
                    var typeList = FormatTypeList(typeArgs);
                    var diagnostic = Diagnostic.Create(
                        Rule,
                        location,
                        "Returns value of type",
                        typeList
                    );
                    context.ReportDiagnostic(diagnostic);
                    break;
                }
            }
        }

        // Check symbol itself (e.g., field/property/parameter attributes)
        foreach (var attr in symbol.GetAttributes())
        {
            if (IsTypeUnion(attr, out var typeArgs))
            {
                var typeList = FormatTypeList(typeArgs);
                var diagnostic = Diagnostic.Create(
                    Rule,
                    location,
                    $"{(symbol is IPropertySymbol ? "Property" : "Field")} \'{symbol.Name}\' is",
                    typeList
                );
                context.ReportDiagnostic(diagnostic);
                break;
            }
        }
    }

    private void AnalyzeInvocationExpressionForTU001(SyntaxNodeAnalysisContext context)
    {
        var invocation = (InvocationExpressionSyntax)context.Node;
        var symbol = context.SemanticModel.GetSymbolInfo(invocation).Symbol;

        if (symbol is IMethodSymbol methodSymbol)
        {
            // TU001 for return value
            foreach (var attr in methodSymbol.GetReturnTypeAttributes())
            {
                if (IsTypeUnion(attr, out var unionTypes))
                {
                    var typeList = FormatTypeList(unionTypes);
                    var diagnostic = Diagnostic.Create(
                        Rule,
                        GetExpression(invocation.Expression).GetLocation(),
                        "Returns value",
                        typeList
                    );
                    context.ReportDiagnostic(diagnostic);
                    break;
                }
            }

            // TU001 for parameters
            foreach (var param in methodSymbol.Parameters)
            {
                foreach (var attr in param.GetAttributes())
                {
                    if (IsTypeUnion(attr, out var unionTypes))
                    {
                        var typeList = FormatTypeList(unionTypes);

                        var diagnostic = Diagnostic.Create(
                            Rule,
                            GetExpression(invocation.Expression).GetLocation(),
                            $"Parameter \'{param.Name}\' expects",
                            typeList
                        );

                        context.ReportDiagnostic(diagnostic);
                    }
                }
            }
        }
    }

    private static ExpressionSyntax GetExpression(ExpressionSyntax expression)
    {
        if (expression is MemberAccessExpressionSyntax mae)
        {
            return mae.Name;
        }

        return expression;
    }

    private static bool IsTypeUnion(AttributeData attr, out TypedConstant values)
    {
        values = default;
        if (attr.AttributeClass?.Name != "TypeUnionAttribute")
            return false;

        if (attr.ConstructorArguments.FirstOrDefault().Kind == TypedConstantKind.Array)
        {
            values = attr.ConstructorArguments[0];
            return true;
        }

        return false;
    }

    private void AnalyzeReturnStatement(SyntaxNodeAnalysisContext context)
    {
        var returnStatement = (ReturnStatementSyntax)context.Node;

        // Find enclosing method
        var methodSyntax = returnStatement.FirstAncestorOrSelf<MethodDeclarationSyntax>();
        if (methodSyntax == null)
            return;

        var methodSymbol = context.SemanticModel.GetDeclaredSymbol(methodSyntax);
        if (methodSymbol == null)
            return;

        // Check for [return: TypeUnion(...)]
        var returnAttr = methodSymbol.GetReturnTypeAttributes()
            .FirstOrDefault(attr => IsTypeUnion(attr, out _));

        if (returnAttr == null || !IsTypeUnion(returnAttr, out var unionTypes))
            return;

        var expr = returnStatement.Expression;
        if (expr == null)
            return;

        var compilation = context.SemanticModel.Compilation;

        // Special handling for conditional expressions
        if (expr is ConditionalExpressionSyntax conditional)
        {
            var whenTrueType = context.SemanticModel.GetTypeInfo(conditional.WhenTrue).Type;
            var whenFalseType = context.SemanticModel.GetTypeInfo(conditional.WhenFalse).Type;

            bool isTrueCompatible = IsAnyImplicit(whenTrueType, unionTypes, compilation);
            bool isFalseCompatible = IsAnyImplicit(whenFalseType, unionTypes, compilation);

            if (!isTrueCompatible || !isFalseCompatible)
            {
                var diagnostic = Diagnostic.Create(
                    IncompatibleReturnTypeRule,
                    expr.GetLocation(),
                    "return (conditional branches)",
                    FormatTypeList(unionTypes),
                    $"{whenTrueType?.ToDisplayString()} | {whenFalseType?.ToDisplayString()}"
                );
                context.ReportDiagnostic(diagnostic);
            }

            return; // handled
        }

        // Normal return expression
        var returnType = context.SemanticModel.GetTypeInfo(expr).Type;
        if (returnType == null)
            return;

        bool isCompatible = IsAnyImplicit(returnType, unionTypes, compilation);
        if (!isCompatible)
        {
            var diagnostic = Diagnostic.Create(
                IncompatibleReturnTypeRule,
                expr.GetLocation(),
                "return",
                FormatTypeList(unionTypes),
                returnType.ToDisplayString()
            );
            context.ReportDiagnostic(diagnostic);
        }
    }

    private static bool IsAnyImplicit(ITypeSymbol? source, TypedConstant unionTypes, Compilation compilation)
    {
        if (source == null)
            return false;

        return unionTypes.Values.Any(tc =>
            tc.Value is ITypeSymbol target &&
            compilation.ClassifyConversion(source, target).IsImplicit);
    }

    private void AnalyzeIsPattern(SyntaxNodeAnalysisContext context)
    {
        var isPattern = (IsPatternExpressionSyntax)context.Node;

        // We only care about patterns like `x is int y`
        if (isPattern.Pattern is not DeclarationPatternSyntax declarationPattern)
            return;

        var exprType = context.SemanticModel.GetTypeInfo(isPattern.Expression).Type;
        if (exprType == null)
            return;

        // Get the declared type in the pattern, e.g. "int"
        var targetType = context.SemanticModel.GetTypeInfo(declarationPattern.Type).Type;
        if (targetType == null)
            return;

        // Try to find the symbol of the expression we're checking, e.g., "y" in "y is int"
        var symbolInfo = context.SemanticModel.GetSymbolInfo(isPattern.Expression).Symbol;

        if (symbolInfo is not IParameterSymbol paramSymbol)
            return;

        var attr = paramSymbol.GetAttributes()
            .FirstOrDefault(a => a.AttributeClass?.Name == "TypeUnionAttribute");

        if (attr == null || !IsTypeUnion(attr, out var allowedTypes))
            return;

        var compilation = context.SemanticModel.Compilation;

        var isCompatible = IsAnyImplicit(targetType, allowedTypes, compilation);
        if (!isCompatible)
        {
            var diagnostic = Diagnostic.Create(
                InvalidPatternRule,
                declarationPattern.Type.GetLocation(),
                FormatTypeList(allowedTypes),
                targetType.ToDisplayString()
            );
            context.ReportDiagnostic(diagnostic);
        }
    }

    private void AnalyzeSwitchStatement(SyntaxNodeAnalysisContext context)
    {
        var switchStmt = (SwitchStatementSyntax)context.Node;
        var exprSymbol = context.SemanticModel.GetSymbolInfo(switchStmt.Expression).Symbol;

        if (exprSymbol is not IParameterSymbol paramSymbol)
            return;

        var attr = paramSymbol.GetAttributes()
            .FirstOrDefault(a => IsTypeUnion(a, out _));

        if (attr == null || !IsTypeUnion(attr, out var allowedTypes))
            return;

        var compilation = context.SemanticModel.Compilation;

        foreach (var section in switchStmt.Sections)
        {
            foreach (var label in section.Labels.OfType<CasePatternSwitchLabelSyntax>())
            {
                if (label.Pattern is DeclarationPatternSyntax declarationPattern)
                {
                    var patternType = context.SemanticModel.GetTypeInfo(declarationPattern.Type).Type;
                    if (patternType != null && !IsAnyImplicit(patternType, allowedTypes, compilation))
                    {
                        var diagnostic = Diagnostic.Create(
                            InvalidPatternRule,
                            declarationPattern.Type.GetLocation(),
                            paramSymbol.Name,
                            patternType.ToDisplayString()
                        );
                        context.ReportDiagnostic(diagnostic);
                    }
                }
            }
        }
    }

    private void AnalyzeSwitchExpression(SyntaxNodeAnalysisContext context)
    {
        var switchExpr = (SwitchExpressionSyntax)context.Node;
        var exprSymbol = context.SemanticModel.GetSymbolInfo(switchExpr.GoverningExpression).Symbol;

        if (exprSymbol is not IParameterSymbol paramSymbol)
            return;

        var attr = paramSymbol.GetAttributes()
            .FirstOrDefault(a => IsTypeUnion(a, out _));

        if (attr == null || !IsTypeUnion(attr, out var allowedTypes))
            return;

        var compilation = context.SemanticModel.Compilation;

        foreach (var arm in switchExpr.Arms)
        {
            if (arm.Pattern is DeclarationPatternSyntax declarationPattern)
            {
                var patternType = context.SemanticModel.GetTypeInfo(declarationPattern.Type).Type;
                if (patternType != null && !IsAnyImplicit(patternType, allowedTypes, compilation))
                {
                    var diagnostic = Diagnostic.Create(
                        InvalidPatternRule,
                        declarationPattern.Type.GetLocation(),
                        paramSymbol.Name,
                        patternType.ToDisplayString()
                    );
                    context.ReportDiagnostic(diagnostic);
                }
            }
        }
    }

    private void AnalyzeFieldDeclaration(SyntaxNodeAnalysisContext context)
    {
        var fieldDecl = (FieldDeclarationSyntax)context.Node;
        var typeLocation = fieldDecl.Declaration.Type.GetLocation();

        foreach (var variable in fieldDecl.Declaration.Variables)
        {
            var symbol = context.SemanticModel.GetDeclaredSymbol(variable) as IFieldSymbol;
            if (symbol != null)
            {
                foreach (var attr in symbol.GetAttributes())
                {
                    if (IsTypeUnion(attr, out var typeArgs))
                    {
                        if (IsNotUnionCompatibleType(symbol.Type, context.Compilation))
                        {
                            var diag = Diagnostic.Create(
                                MustBeObjectTypeRule,
                                fieldDecl.Declaration.Type.GetLocation(),
                                $"Field '{symbol.Name}'"
                            );
                            context.ReportDiagnostic(diag);
                        }

                        var typeList = FormatTypeList(typeArgs);
                        var diagnostic = Diagnostic.Create(
                            Rule,
                            typeLocation,
                            $"Field '{symbol.Name}' expects",
                            typeList
                        );
                        context.ReportDiagnostic(diagnostic);
                        break;
                    }
                }
            }
        }
    }

    private void AnalyzePropertyDeclaration(SyntaxNodeAnalysisContext context)
    {
        var propDecl = (PropertyDeclarationSyntax)context.Node;
        var symbol = context.SemanticModel.GetDeclaredSymbol(propDecl) as IPropertySymbol;
        var typeLocation = propDecl.Type.GetLocation();

        if (symbol != null)
        {
            foreach (var attr in symbol.GetAttributes())
            {
                if (IsTypeUnion(attr, out var typeArgs))
                {
                    if (IsNotUnionCompatibleType(symbol.Type, context.Compilation))
                    {
                        var diag = Diagnostic.Create(
                            MustBeObjectTypeRule,
                            propDecl.Type.GetLocation(),
                            $"Property '{symbol.Name}'"
                        );
                        context.ReportDiagnostic(diag);
                    }

                    var typeList = FormatTypeList(typeArgs);
                    var diagnostic = Diagnostic.Create(
                        Rule,
                        typeLocation,
                        $"Property '{symbol.Name}' is",
                        typeList
                    );
                    context.ReportDiagnostic(diagnostic);
                    break;
                }
            }
        }
    }

    private void AnalyzeAssignmentExpression(SyntaxNodeAnalysisContext context)
    {
        var assignment = (AssignmentExpressionSyntax)context.Node;
        var leftSymbol = context.SemanticModel.GetSymbolInfo(assignment.Left).Symbol;

        if (leftSymbol is IFieldSymbol or IPropertySymbol)
        {
            var targetSymbol = leftSymbol!;

            var attr = targetSymbol.GetAttributes()
                .FirstOrDefault(a => IsTypeUnion(a, out _));
            if (attr == null || !IsTypeUnion(attr, out var allowedTypes))
                return;

            var rightType = context.SemanticModel.GetTypeInfo(assignment.Right).Type;
            if (rightType == null)
                return;

            if (!IsAnyImplicit(rightType, allowedTypes, context.SemanticModel.Compilation))
            {
                var diagnostic = Diagnostic.Create(
                    IncompatibleTypeRule,
                    assignment.Right.GetLocation(),
                    targetSymbol.Name,
                    FormatTypeList(allowedTypes),
                    rightType.ToDisplayString()
                );
                context.ReportDiagnostic(diagnostic);
            }
        }
    }

    private static string FormatTypeList(TypedConstant typeArgs)
    {
        return string.Join(" or ", typeArgs.Values.Select(v => v.Value is ITypeSymbol t ? $"\'{t.ToDisplayString()}\'" : "unknown"));
    }

    private static bool IsNotUnionCompatibleType(ITypeSymbol type, Compilation compilation) => type.SpecialType != SpecialType.System_Object &&
               type.TypeKind != TypeKind.Dynamic &&
               !SymbolEqualityComparer.Default.Equals(type, compilation.GetSpecialType(SpecialType.System_Object));

    private static bool IsTypeUnionAttribute(AttributeData attr) =>
        attr.AttributeClass?.Name == "TypeUnionAttribute" ||
        attr.AttributeClass?.ToDisplayString() == "TypeUnionAttribute" ||
        attr.AttributeClass?.ToDisplayString().EndsWith(".TypeUnionAttribute") == true;
}
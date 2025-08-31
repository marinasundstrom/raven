using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Globalization;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Operations;

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
        messageFormat: "Type {1} is not allowed by {0}",
        category: "TypeChecking",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true
    );

    private static readonly DiagnosticDescriptor MustBeObjectTypeRule = new DiagnosticDescriptor(
        id: "TU005",
        title: "TypeUnion must be declared with object type",
        messageFormat: "{0} must be declared with type object when using [TypeUnion]",
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
        context.RegisterOperationBlockStartAction(AnalyzeOperationBlock);
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

            if (!parameter.Locations.Any(l => l.IsInSource) &&
                IsNotUnionCompatibleType(parameter.Type, context.SemanticModel.Compilation))
            {
                var diag = Diagnostic.Create(
                    MustBeObjectTypeRule,
                    argExpr.GetLocation(),
                    $"Parameter '{parameter.Name}'");
                context.ReportDiagnostic(diag);
            }

            var allowedTypes = unionAttr.ConstructorArguments.FirstOrDefault();
            if (allowedTypes.Kind != TypedConstantKind.Array)
                continue;

            var argType = context.SemanticModel.GetTypeInfo(argExpr).Type;
            var argConst = context.SemanticModel.GetConstantValue(argExpr);

            bool isCompatible = IsAnyImplicit(argType, argConst.HasValue ? argConst.Value : null, allowedTypes, context.SemanticModel.Compilation);

            if (!isCompatible)
            {
                var diagnostic = Diagnostic.Create(
                    IncompatibleTypeRule,
                    argExpr.GetLocation(),
                    parameter.Name,
                    FormatTypeList(allowedTypes),
                    FormatTypeName(argType)
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
                    if (!methodSymbol.Locations.Any(l => l.IsInSource) &&
                        IsNotUnionCompatibleType(methodSymbol.ReturnType, context.SemanticModel.Compilation))
                    {
                        var diag = Diagnostic.Create(
                            MustBeObjectTypeRule,
                            GetExpression(invocation.Expression).GetLocation(),
                            $"Return type of method '{methodSymbol.Name}'");
                        context.ReportDiagnostic(diag);
                    }

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
            var whenTrueConst = context.SemanticModel.GetConstantValue(conditional.WhenTrue);
            var whenFalseConst = context.SemanticModel.GetConstantValue(conditional.WhenFalse);

            bool isTrueCompatible = IsAnyImplicit(whenTrueType, whenTrueConst.HasValue ? whenTrueConst.Value : null, unionTypes, compilation);
            bool isFalseCompatible = IsAnyImplicit(whenFalseType, whenFalseConst.HasValue ? whenFalseConst.Value : null, unionTypes, compilation);

            if (!isTrueCompatible || !isFalseCompatible)
            {
                var diagnostic = Diagnostic.Create(
                    IncompatibleReturnTypeRule,
                    expr.GetLocation(),
                    "return (conditional branches)",
                    FormatTypeList(unionTypes),
                    $"{FormatTypeName(whenTrueType)} | {FormatTypeName(whenFalseType)}"
                );
                context.ReportDiagnostic(diagnostic);
            }

            return; // handled
        }

        // Normal return expression
        var returnType = context.SemanticModel.GetTypeInfo(expr).Type;
        var returnConst = context.SemanticModel.GetConstantValue(expr);

        bool isCompatible = IsAnyImplicit(returnType, returnConst.HasValue ? returnConst.Value : null, unionTypes, compilation);
        if (!isCompatible)
        {
            var diagnostic = Diagnostic.Create(
                IncompatibleReturnTypeRule,
                expr.GetLocation(),
                "return",
                FormatTypeList(unionTypes),
                FormatTypeName(returnType)
            );
            context.ReportDiagnostic(diagnostic);
        }
    }

    private static bool IsAnyImplicit(ITypeSymbol? source, object? constantValue, TypedConstant unionTypes, Compilation compilation)
    {
        if (source == null && constantValue == null)
            return ContainsNullType(unionTypes);

        foreach (var tc in unionTypes.Values)
        {
            if (tc.Value is ITypeSymbol target)
            {
                if (source != null && compilation.ClassifyConversion(source, target).IsImplicit)
                    return true;
            }
            else if (Equals(constantValue, tc.Value))
            {
                return true;
            }
        }

        return false;
    }

    private void AnalyzeIsPattern(SyntaxNodeAnalysisContext context)
    {
        var isPattern = (IsPatternExpressionSyntax)context.Node;

        if (isPattern.Pattern is DeclarationPatternSyntax declarationPattern)
        {
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

            var isCompatible = IsAnyImplicit(targetType, null, allowedTypes, compilation);
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
        else if (isPattern.Pattern is ConstantPatternSyntax constPattern &&
                 constPattern.Expression.IsKind(SyntaxKind.NullLiteralExpression))
        {
            var symbolInfo = context.SemanticModel.GetSymbolInfo(isPattern.Expression).Symbol;
            if (symbolInfo is not IParameterSymbol paramSymbol)
                return;

            var attr = paramSymbol.GetAttributes()
                .FirstOrDefault(a => a.AttributeClass?.Name == "TypeUnionAttribute");

            if (attr == null || !IsTypeUnion(attr, out var allowedTypes))
                return;

            if (!ContainsNullType(allowedTypes))
            {
                var diagnostic = Diagnostic.Create(
                    InvalidPatternRule,
                    constPattern.GetLocation(),
                    FormatTypeList(allowedTypes),
                    "null"
                );
                context.ReportDiagnostic(diagnostic);
            }
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
                    if (patternType != null && !IsAnyImplicit(patternType, null, allowedTypes, compilation))
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
                else if (label.Pattern is ConstantPatternSyntax constPattern &&
                         constPattern.Expression.IsKind(SyntaxKind.NullLiteralExpression))
                {
                    if (!ContainsNullType(allowedTypes))
                    {
                        var diagnostic = Diagnostic.Create(
                            InvalidPatternRule,
                            constPattern.GetLocation(),
                            paramSymbol.Name,
                            "null"
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
                if (patternType != null && !IsAnyImplicit(patternType, null, allowedTypes, compilation))
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
            else if (arm.Pattern is ConstantPatternSyntax constPattern &&
                     constPattern.Expression.IsKind(SyntaxKind.NullLiteralExpression))
            {
                if (!ContainsNullType(allowedTypes))
                {
                    var diagnostic = Diagnostic.Create(
                        InvalidPatternRule,
                        constPattern.GetLocation(),
                        paramSymbol.Name,
                        "null"
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

            if (!targetSymbol.Locations.Any(l => l.IsInSource) &&
                IsNotUnionCompatibleType((targetSymbol as IFieldSymbol)?.Type ?? (targetSymbol as IPropertySymbol)?.Type!, context.SemanticModel.Compilation))
            {
                var diag = Diagnostic.Create(
                    MustBeObjectTypeRule,
                    assignment.Left.GetLocation(),
                    $"{(targetSymbol is IPropertySymbol ? "Property" : "Field")} '{targetSymbol.Name}'");
                context.ReportDiagnostic(diag);
            }

            var rightType = context.SemanticModel.GetTypeInfo(assignment.Right).Type;
            var rightConst = context.SemanticModel.GetConstantValue(assignment.Right);

            if (!IsAnyImplicit(rightType, rightConst.HasValue ? rightConst.Value : null, allowedTypes, context.SemanticModel.Compilation))
            {
                var diagnostic = Diagnostic.Create(
                    IncompatibleTypeRule,
                    assignment.Right.GetLocation(),
                    targetSymbol.Name,
                    FormatTypeList(allowedTypes),
                    FormatTypeName(rightType)
                );
                context.ReportDiagnostic(diagnostic);
            }
        }
    }

    private void AnalyzeOperationBlock(OperationBlockStartAnalysisContext context)
    {
        var locals = new Dictionary<ILocalSymbol, ImmutableArray<ITypeSymbol>>(SymbolEqualityComparer.Default);

        context.RegisterOperationAction(ctx =>
        {
            var declarator = (IVariableDeclaratorOperation)ctx.Operation;
            if (declarator.Initializer != null &&
                TryGetUnionTypesFromExpression(declarator.Initializer.Value, ctx.Compilation, locals, out var types))
            {
                locals[declarator.Symbol] = types;
                if (declarator.Syntax is VariableDeclaratorSyntax vds)
                {
                    var diag = Diagnostic.Create(
                        Rule,
                        vds.Identifier.GetLocation(),
                        $"Variable '{declarator.Symbol.Name}' is",
                        FormatTypeList(types));
                    ctx.ReportDiagnostic(diag);
                }
            }
        }, OperationKind.VariableDeclarator);

        context.RegisterOperationAction(ctx =>
        {
            var assignment = (ISimpleAssignmentOperation)ctx.Operation;
            if (assignment.Target is ILocalReferenceOperation localRef &&
                TryGetUnionTypesFromExpression(assignment.Value, ctx.Compilation, locals, out var types))
            {
                locals[localRef.Local] = types;
                var diag = Diagnostic.Create(
                    Rule,
                    assignment.Target.Syntax.GetLocation(),
                    $"Variable '{localRef.Local.Name}' is",
                    FormatTypeList(types));
                ctx.ReportDiagnostic(diag);
            }
        }, OperationKind.SimpleAssignment);

        context.RegisterOperationAction(ctx =>
        {
            var localRef = (ILocalReferenceOperation)ctx.Operation;
            if (locals.TryGetValue(localRef.Local, out var types))
            {
                var diag = Diagnostic.Create(
                    Rule,
                    localRef.Syntax.GetLocation(),
                    $"Variable '{localRef.Local.Name}' is",
                    FormatTypeList(types));
                ctx.ReportDiagnostic(diag);
            }
        }, OperationKind.LocalReference);
    }

    private static bool TryGetUnionTypes(ISymbol symbol, out ImmutableArray<ITypeSymbol> types)
    {
        IEnumerable<AttributeData> attrs = symbol switch
        {
            IMethodSymbol m => m.GetReturnTypeAttributes().Concat(m.GetAttributes()),
            _ => symbol.GetAttributes()
        };

        foreach (var attr in attrs)
        {
            if (IsTypeUnion(attr, out var tc))
            {
                types = tc.Values
                    .Select(v => v.Value as ITypeSymbol)
                    .OfType<ITypeSymbol>()
                    .ToImmutableArray();
                return true;
            }
        }

        types = default;
        return false;
    }

    private static bool TryGetUnionTypesFromExpression(IOperation op, Compilation compilation, Dictionary<ILocalSymbol, ImmutableArray<ITypeSymbol>> locals, out ImmutableArray<ITypeSymbol> types)
    {
        switch (op)
        {
            case IInvocationOperation invocation when TryGetUnionTypes(invocation.TargetMethod, out types):
                return true;
            case ILocalReferenceOperation localRef when locals.TryGetValue(localRef.Local, out types):
                return true;
            case IParameterReferenceOperation paramRef when TryGetUnionTypes(paramRef.Parameter, out types):
                return true;
            case IPropertyReferenceOperation propRef when TryGetUnionTypes(propRef.Property, out types):
                return true;
            case IFieldReferenceOperation fieldRef when TryGetUnionTypes(fieldRef.Field, out types):
                return true;
            case IConditionalOperation cond:
                var hasTrue = TryGetUnionTypesFromExpression(cond.WhenTrue, compilation, locals, out var trueTypes);
                var hasFalse = TryGetUnionTypesFromExpression(cond.WhenFalse, compilation, locals, out var falseTypes);
                if (hasTrue || hasFalse)
                {
                    types = trueTypes.Concat(falseTypes).Distinct<ITypeSymbol>(SymbolEqualityComparer.Default).ToImmutableArray();
                    return types.Length > 0;
                }
                break;
        }

        types = default;
        return false;
    }

    private static string FormatTypeList(ImmutableArray<ITypeSymbol> types)
        => string.Join(" or ", types.Select(FormatTypeName));

    private static bool IsNullShim(ITypeSymbol type)
        => type.Name == "Null" && type.ContainingNamespace?.IsGlobalNamespace == true;

    private static bool ContainsNullType(TypedConstant typeArgs)
        => typeArgs.Values.Any(v => (v.Value is ITypeSymbol t && IsNullShim(t)) || v.Value is null);

    private static string FormatTypeName(ITypeSymbol? type)
        => type == null || IsNullShim(type) ? "null" : type.ToDisplayString();

    private static string FormatTypeList(TypedConstant typeArgs)
        => string.Join(" or ", typeArgs.Values.Select(FormatValue));

    private static string FormatValue(TypedConstant tc)
    {
        if (tc.Value is ITypeSymbol t)
            return FormatTypeName(t);
        return FormatConstant(tc.Value);
    }

    private static string FormatConstant(object? value)
        => value switch
        {
            null => "null",
            string s => SymbolDisplay.FormatLiteral(s, true),
            char c => SymbolDisplay.FormatLiteral(c, true),
            bool b => b ? "true" : "false",
            _ => Convert.ToString(value, CultureInfo.InvariantCulture) ?? value?.ToString() ?? "unknown"
        };

    private static bool IsNotUnionCompatibleType(ITypeSymbol type, Compilation compilation) => type.SpecialType != SpecialType.System_Object &&
               type.TypeKind != TypeKind.Dynamic &&
               !SymbolEqualityComparer.Default.Equals(type, compilation.GetSpecialType(SpecialType.System_Object));

    private static bool IsTypeUnionAttribute(AttributeData attr) =>
        attr.AttributeClass?.Name == "TypeUnionAttribute" ||
        attr.AttributeClass?.ToDisplayString() == "TypeUnionAttribute" ||
        attr.AttributeClass?.ToDisplayString().EndsWith(".TypeUnionAttribute") == true;
}

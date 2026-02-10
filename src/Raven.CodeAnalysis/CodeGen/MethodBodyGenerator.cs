using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics.SymbolStore;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Text;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.CodeGen;

internal class MethodBodyGenerator
{
    private TypeBuilder _typeBuilder;
    private MethodBase _methodBase;
    private Compilation _compilation;
    private IMethodSymbol _methodSymbol;
    private TypeGenerator.LambdaClosure? _lambdaClosure;
    private readonly Dictionary<ILabelSymbol, ILLabel> _labels = new(SymbolEqualityComparer.Default);
    private readonly Dictionary<ILabelSymbol, Scope> _labelScopes = new(SymbolEqualityComparer.Default);
    private ILLabel? _returnLabel;
    private IILocal? _returnValueLocal;
    private readonly Dictionary<SyntaxTree, ISymbolDocumentWriter> _symbolDocuments = new();
    private static readonly Guid CSharpLanguageId = new("3f5162f8-07c6-11d3-9053-00c04fa302a1");
    private static readonly Guid DocumentTypeId = new("5a869d0b-6611-11d3-bd2a-0000f80849bd");
    private static readonly Guid DocumentVendorId = new("994b45c4-e6e9-11d2-903f-00c04fa302a1");
    private static readonly Guid Sha256AlgorithmId = new("8829d00f-11b8-4213-878b-770e8597ac16");
    private static readonly MethodInfo ConsoleErrorGetter =
        typeof(Console).GetProperty(nameof(Console.Error))?.GetMethod
        ?? throw new InvalidOperationException("System.Console.Error getter was not found.");
    private static readonly MethodInfo TextWriterWriteLineObject =
        typeof(TextWriter).GetMethod(nameof(TextWriter.WriteLine), [typeof(object)])
        ?? throw new InvalidOperationException("System.IO.TextWriter.WriteLine(object) was not found.");

    public MethodBodyGenerator(MethodGenerator methodGenerator)
    {
        MethodGenerator = methodGenerator;
    }

    public Compilation Compilation => _compilation ??= MethodGenerator.Compilation;
    public MethodGenerator MethodGenerator { get; }
    public IMethodSymbol MethodSymbol => _methodSymbol ??= MethodGenerator.MethodSymbol;
    public TypeBuilder TypeBuilder => _typeBuilder ??= MethodGenerator.TypeGenerator.TypeBuilder!;
    public MethodBase MethodBase => _methodBase ??= MethodGenerator.MethodBase;

    private BaseGenerator baseGenerator;
    private Scope scope;

    public IILBuilder ILGenerator { get; private set; }

    internal bool TryGetCapturedField(ISymbol symbol, out FieldBuilder fieldBuilder)
        => TryGetCapturedField(symbol, out fieldBuilder, out _);

    internal bool TryGetCapturedField(ISymbol symbol, out FieldBuilder fieldBuilder, out bool fromStateMachine)
    {
        fromStateMachine = false;

        if (_lambdaClosure is null)
        {
            if (symbol is ILocalSymbol localSymbol &&
                MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol asyncStateMachine &&
                asyncStateMachine.TryGetHoistedLocalField(localSymbol, out var hoistedField) &&
                MethodGenerator.TypeGenerator.CodeGen.GetMemberBuilder(hoistedField) is FieldBuilder hoistedBuilder)
            {
                fieldBuilder = hoistedBuilder;
                fromStateMachine = true;
                return true;
            }

            fieldBuilder = default!;
            return false;
        }

        return _lambdaClosure.TryGetField(symbol, out fieldBuilder);
    }

    internal ILLabel GetOrCreateLabel(ILabelSymbol labelSymbol)
    {
        if (!_labels.TryGetValue(labelSymbol, out var label))
        {
            label = ILGenerator.DefineLabel();
            _labels[labelSymbol] = label;
        }

        return label;
    }

    internal ILLabel GetOrCreateReturnLabel()
    {
        return _returnLabel ??= ILGenerator.DefineLabel();
    }

    internal IILocal EnsureReturnValueLocal()
    {
        if (_returnValueLocal is not null)
            return _returnValueLocal;

        var returnType = MethodSymbol.ReturnType;
        if (returnType.SpecialType is SpecialType.System_Void or SpecialType.System_Unit)
            throw new InvalidOperationException("Void-like methods do not require a return value local.");

        var clrType = TypeSymbolExtensionsForCodeGen.GetClrType(returnType, MethodGenerator.TypeGenerator.CodeGen);
        _returnValueLocal = ILGenerator.DeclareLocal(clrType);
        return _returnValueLocal;
    }

    internal bool TryGetReturnValueLocal(out IILocal? local)
    {
        local = _returnValueLocal;
        return local is not null;
    }

    internal void RegisterLabelScope(ILabelSymbol labelSymbol, Scope scope)
    {
        _labelScopes[labelSymbol] = scope;
    }

    internal Scope? GetLabelScope(ILabelSymbol labelSymbol)
    {
        return _labelScopes.TryGetValue(labelSymbol, out var scope)
            ? scope
            : null;
    }

    private ISymbolDocumentWriter GetOrAddDocument(SyntaxTree syntaxTree)
    {
        if (_symbolDocuments.TryGetValue(syntaxTree, out var document))
            return document;

        var moduleBuilder = MethodGenerator.TypeGenerator.CodeGen.ModuleBuilder;
        document = moduleBuilder.DefineDocument(syntaxTree.FilePath, CSharpLanguageId, DocumentVendorId, DocumentTypeId);

        var checksum = syntaxTree.GetText().GetChecksum();
        if (!checksum.IsDefaultOrEmpty)
            document.SetCheckSum(Sha256AlgorithmId, checksum.ToArray());

        _symbolDocuments[syntaxTree] = document;
        return document;
    }

    internal void EmitSequencePoint(BoundStatement statement)
    {
        var syntax = TryGetSyntax(statement);
        if (syntax is null)
            return;

        EmitSequencePoint(syntax);
    }

    internal void EmitSequencePoint(SyntaxNode syntax)
    {
        if (syntax.SyntaxTree is null)
            return;

        var span = syntax.Span;
        if (span.Length == 0)
            return;

        var sourceText = syntax.SyntaxTree.GetText();
        var (startLine, startColumn) = sourceText.GetLineAndColumn(span.Start);
        var (endLine, endColumn) = sourceText.GetLineAndColumn(span.End);
        var document = GetOrAddDocument(syntax.SyntaxTree);

        ILGenerator.Emit(OpCodes.Nop);
        ILGenerator.MarkSequencePoint(document, startLine + 1, startColumn + 1, endLine + 1, endColumn + 1);
    }

    private SyntaxNode? TryGetSyntax(BoundNode node)
    {
        var syntaxRef = MethodSymbol.DeclaringSyntaxReferences.FirstOrDefault();
        if (syntaxRef is null)
            return null;

        var semanticModel = Compilation.GetSemanticModel(syntaxRef.SyntaxTree);
        return semanticModel.GetSyntax(node);
    }

    internal void EmitLoadClosure()
    {
        if (_lambdaClosure is null)
            throw new InvalidOperationException("No closure parameter available for this lambda.");

        if (MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol asyncStateMachine &&
            asyncStateMachine.GetConstructedMembers(asyncStateMachine.AsyncMethod).ThisField is { } closureField)
        {
            ILGenerator.Emit(OpCodes.Ldarg_0);
            ILGenerator.Emit(
                OpCodes.Ldfld,
                closureField.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen));
            return;
        }

        ILGenerator.Emit(OpCodes.Ldarg_0);
    }

    public void Emit()
    {
        baseGenerator = new BaseGenerator(this);
        scope = new Scope(baseGenerator);

        ILGenerator = MethodGenerator.ILBuilderFactory.Create(MethodGenerator);

        try
        {
            EmitCore();
        }
        finally
        {
            if (ILGenerator is ILabelTrackingILBuilder tracking)
                tracking.MarkAllLabels();
        }
    }

    private void EmitCore()
    {

        if (MethodSymbol is SynthesizedMainMethodSymbol mainSymbol && mainSymbol.AsyncImplementation is { } asyncImplementation)
        {
            EmitEntryPointBridge(MethodSymbol, asyncImplementation);
            return;
        }

        if (MethodSymbol is SynthesizedEntryPointBridgeMethodSymbol bridgeMethod)
        {
            EmitEntryPointBridge(bridgeMethod, bridgeMethod.AsyncImplementation);
            return;
        }

        if (MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol asyncStateMachine)
        {
            EmitAsyncStateMachineMethod(asyncStateMachine);
            return;
        }

        if (MethodSymbol.ContainingType is SynthesizedIteratorTypeSymbol iteratorType)
        {
            EmitIteratorMethod(iteratorType);
            return;
        }

        if (MethodSymbol.MethodKind == MethodKind.PropertyGet &&
            MethodSymbol.ContainingType.TryGetDiscriminatedUnionCase() is not null &&
            MethodSymbol.ContainingSymbol is SourcePropertySymbol unionCaseProperty &&
            unionCaseProperty.BackingField is SourceFieldSymbol unionCaseField)
        {
            EmitUnionCasePropertyGetter(unionCaseProperty, unionCaseField);
            return;
        }

        if (MethodSymbol.Name == "Deconstruct" &&
            MethodSymbol.MethodKind == MethodKind.Ordinary &&
            MethodSymbol.ReturnType.SpecialType == SpecialType.System_Unit &&
            MethodSymbol.DeclaringSyntaxReferences.IsDefaultOrEmpty)
        {
            // Union-case deconstruct can be requested on either the source case type
            // or a constructed case type (e.g. generic instantiation). Normalize to the
            // source definition so we can locate the backing fields reliably.
            var caseType = MethodSymbol.ContainingType switch
            {
                SourceDiscriminatedUnionCaseTypeSymbol s => s,
                ConstructedNamedTypeSymbol { OriginalDefinition: SourceDiscriminatedUnionCaseTypeSymbol s } => s,
                _ => null
            };

            if (caseType is not null)
            {
                EmitUnionCaseDeconstruct(caseType);
                return;
            }
        }

        if (MethodSymbol.MethodKind == MethodKind.Conversion &&
            MethodSymbol.ReturnType.TryGetDiscriminatedUnion() is not null &&
            MethodSymbol.Parameters.Length == 1 &&
            MethodSymbol.Parameters[0].Type.TryGetDiscriminatedUnionCase() is not null &&
            TryGetSourceDiscriminatedUnionDefinition(MethodSymbol.ContainingType) is { } conversionUnion)
        {
            EmitDiscriminatedUnionConversion(conversionUnion);
            return;
        }

        if (MethodSymbol.ContainingType is SourceDiscriminatedUnionSymbol tryGetUnion &&
            MethodSymbol.ReturnType.SpecialType == SpecialType.System_Boolean &&
            MethodSymbol.Parameters.Length == 1 &&
            MethodSymbol.Parameters[0].RefKind == RefKind.Out &&
            MethodSymbol.Parameters[0].Type.TryGetDiscriminatedUnionCase() is { } tryGetCase &&
            MethodSymbol.DeclaringSyntaxReferences.IsDefaultOrEmpty &&
            MethodSymbol.Name.StartsWith("TryGet", StringComparison.Ordinal))
        {
            EmitDiscriminatedUnionTryGetMethod(tryGetUnion, tryGetCase, MethodSymbol.Parameters[0]);
            return;
        }

        if (MethodSymbol.Name == nameof(object.ToString) &&
            MethodSymbol.Parameters.Length == 0 &&
            MethodSymbol.ReturnType.SpecialType == SpecialType.System_String)
        {
            if (MethodSymbol.ContainingType is SourceDiscriminatedUnionCaseTypeSymbol caseType2)
            {
                EmitUnionCaseToString(caseType2);
                return;
            }

            if (MethodSymbol.ContainingType is SourceDiscriminatedUnionSymbol unionType)
            {
                EmitDiscriminatedUnionToString(unionType);
                return;
            }
        }

        var recordType = TryGetRecordTypeDefinition(MethodSymbol.ContainingType);
        if (recordType is not null && TryEmitRecordMethod(recordType))
            return;

        var syntaxReference = MethodSymbol.DeclaringSyntaxReferences.FirstOrDefault();
        if (syntaxReference is null)
        {
            ILGenerator.Emit(OpCodes.Ret);
            return;
        }

        var syntax = syntaxReference.GetSyntax();

        var semanticModel = Compilation.GetSemanticModel(syntax.SyntaxTree);

        BoundBlockStatement? boundBody = syntax switch
        {
            MethodDeclarationSyntax m when m.Body != null => semanticModel.GetBoundNode(m.Body) as BoundBlockStatement,
            OperatorDeclarationSyntax o when o.Body != null => semanticModel.GetBoundNode(o.Body) as BoundBlockStatement,
            ConversionOperatorDeclarationSyntax c when c.Body != null => semanticModel.GetBoundNode(c.Body) as BoundBlockStatement,
            FunctionStatementSyntax l when l.Body != null => semanticModel.GetBoundNode(l.Body) as BoundBlockStatement,
            BaseConstructorDeclarationSyntax c when c.Body != null => semanticModel.GetBoundNode(c.Body) as BoundBlockStatement,
            AccessorDeclarationSyntax a when a.Body != null => semanticModel.GetBoundNode(a.Body) as BoundBlockStatement,
            _ => null
        };

        BoundExpression? expressionBody = syntax switch
        {
            MethodDeclarationSyntax m when m.ExpressionBody is not null
                => semanticModel.GetBoundNode(m.ExpressionBody.Expression) as BoundExpression,
            OperatorDeclarationSyntax o when o.ExpressionBody is not null
                => semanticModel.GetBoundNode(o.ExpressionBody.Expression) as BoundExpression,
            ConversionOperatorDeclarationSyntax c when c.ExpressionBody is not null
                => semanticModel.GetBoundNode(c.ExpressionBody.Expression) as BoundExpression,
            BaseConstructorDeclarationSyntax c when c.ExpressionBody is not null
                => semanticModel.GetBoundNode(c.ExpressionBody.Expression) as BoundExpression,
            AccessorDeclarationSyntax a when a.ExpressionBody is not null
                => semanticModel.GetBoundNode(a.ExpressionBody.Expression) as BoundExpression,
            PropertyDeclarationSyntax p when p.ExpressionBody is not null
                => semanticModel.GetBoundNode(p.ExpressionBody.Expression) as BoundExpression,
            FunctionStatementSyntax l when l.ExpressionBody is not null
                => semanticModel.GetBoundNode(l.ExpressionBody.Expression) as BoundExpression,
            _ => null
        };

        ExpressionSyntax? expressionBodySyntax = syntax switch
        {
            MethodDeclarationSyntax m when m.ExpressionBody is not null => m.ExpressionBody.Expression,
            OperatorDeclarationSyntax o when o.ExpressionBody is not null => o.ExpressionBody.Expression,
            ConversionOperatorDeclarationSyntax c when c.ExpressionBody is not null => c.ExpressionBody.Expression,
            BaseConstructorDeclarationSyntax c when c.ExpressionBody is not null => c.ExpressionBody.Expression,
            AccessorDeclarationSyntax a when a.ExpressionBody is not null => a.ExpressionBody.Expression,
            PropertyDeclarationSyntax p when p.ExpressionBody is not null => p.ExpressionBody.Expression,
            FunctionStatementSyntax l when l.ExpressionBody is not null => l.ExpressionBody.Expression,
            _ => null
        };

        if (boundBody != null)
            DeclareLocals(boundBody);

        switch (syntax)
        {
            case CompilationUnitSyntax compilationUnit:
                if (MethodSymbol is SourceMethodSymbol &&
                    semanticModel.GetBoundNode(compilationUnit) is BoundBlockStatement topLevelBody)
                {
                    DeclareLocals(topLevelBody);
                    EmitMethodBlock(topLevelBody);
                    break;
                }

                foreach (var localDeclStmt in syntax.DescendantNodes()
                    .OfType<LocalDeclarationStatementSyntax>())
                {
                    foreach (var localDeclarator in localDeclStmt.Declaration.Declarators)
                    {
                        var localSymbol = GetDeclaredSymbol<ILocalSymbol>(localDeclarator);
                        if (localSymbol?.Type is null)
                            continue;

                        var clrType = ResolveClrType(localSymbol.Type);
                        var builder = ILGenerator.DeclareLocal(clrType);
                        builder.SetLocalSymInfo(localSymbol.Name);

                        scope.AddLocal(localSymbol, builder);
                    }
                }

                var topLevelDisposables = ImmutableArray.CreateBuilder<ILocalSymbol>();

                foreach (var usingDeclStmt in syntax.DescendantNodes()
                    .OfType<UseDeclarationStatementSyntax>())
                {
                    foreach (var localDeclarator in usingDeclStmt.Declaration.Declarators)
                    {
                        var localSymbol = GetDeclaredSymbol<ILocalSymbol>(localDeclarator);
                        if (localSymbol?.Type is null)
                            continue;

                        var clrType = ResolveClrType(localSymbol.Type);
                        var builder = ILGenerator.DeclareLocal(clrType);
                        builder.SetLocalSymInfo(localSymbol.Name);

                        scope.AddLocal(localSymbol, builder);

                        if (usingDeclStmt.Parent is GlobalStatementSyntax)
                            topLevelDisposables.Add(localSymbol);
                    }
                }

                foreach (var localFunctionStmt in compilationUnit.DescendantNodes().OfType<FunctionStatementSyntax>())
                {
                    var methodSymbol = GetDeclaredSymbol<IMethodSymbol>(localFunctionStmt);
                    if (methodSymbol is null)
                        continue;
                    if (MethodGenerator.TypeGenerator.HasMethodGenerator(methodSymbol))
                        continue;
                    EmitFunction(localFunctionStmt);
                }

                var statements = GetTopLevelStatements(compilationUnit);
                EmitIL(statements, topLevelDisposables.ToImmutable());
                break;

            case FunctionStatementSyntax functionStatement:
                if (boundBody != null)
                {
                    EmitMethodBlock(boundBody);
                }
                else if (expressionBody is not null)
                {
                    if (expressionBodySyntax is not null)
                        EmitSequencePoint(expressionBodySyntax);

                    EmitExpressionBody(expressionBody);
                }
                else
                {
                    ILGenerator.Emit(OpCodes.Ret);
                }
                break;

            case MethodDeclarationSyntax methodDeclaration:
                if (boundBody != null)
                    EmitMethodBlock(boundBody);
                else if (expressionBody is not null)
                {
                    if (expressionBodySyntax is not null)
                        EmitSequencePoint(expressionBodySyntax);

                    EmitExpressionBody(expressionBody);
                }
                else
                    ILGenerator.Emit(OpCodes.Ret);
                break;
            case OperatorDeclarationSyntax:
            case ConversionOperatorDeclarationSyntax:
                if (boundBody != null)
                    EmitMethodBlock(boundBody);
                else if (expressionBody is not null)
                {
                    if (expressionBodySyntax is not null)
                        EmitSequencePoint(expressionBodySyntax);

                    EmitExpressionBody(expressionBody);
                }
                else
                {
                    ILGenerator.Emit(OpCodes.Ret);
                }
                break;

            case BaseConstructorDeclarationSyntax constructorDeclaration:
                var ordinaryConstr = !MethodSymbol.IsNamedConstructor;

                if (ordinaryConstr && !MethodSymbol.IsStatic)
                {
                    EmitConstructorInitializer();
                }

                EmitFieldInitializers(MethodSymbol.IsStatic);

                if (boundBody != null)
                    EmitMethodBlock(boundBody, includeImplicitReturn: false);
                else if (expressionBody is not null)
                {
                    if (expressionBodySyntax is not null)
                        EmitSequencePoint(expressionBodySyntax);

                    EmitExpressionBody(expressionBody, includeReturn: !ordinaryConstr);
                }

                if (ordinaryConstr)
                {
                    ILGenerator.Emit(OpCodes.Ret);
                }
                break;

            case AccessorDeclarationSyntax accessorDeclaration:
                {
                    if (boundBody != null)
                    {
                        EmitMethodBlock(boundBody);
                    }
                    else if (expressionBody is not null)
                    {
                        if (expressionBodySyntax is not null)
                            EmitSequencePoint(expressionBodySyntax);

                        if (MethodSymbol.MethodKind == MethodKind.PropertyGet)
                        {
                            new ExpressionGenerator(baseGenerator, expressionBody).Emit();
                        }
                        else
                        {
                            if (expressionBody is BoundAssignmentExpression assignment)
                            {
                                var stmt = new BoundAssignmentStatement(assignment);
                                new StatementGenerator(baseGenerator, stmt).Emit();
                            }
                            else
                            {
                                var stmt = new BoundExpressionStatement(expressionBody);
                                new StatementGenerator(baseGenerator, stmt).Emit();
                            }
                        }

                        ILGenerator.Emit(OpCodes.Ret);
                    }
                    else if (MethodSymbol.ContainingSymbol is SourceEventSymbol eventSymbol &&
                             eventSymbol.BackingField is SourceFieldSymbol eventBackingField)
                    {
                        EmitAutoEventAccessor(eventSymbol, eventBackingField);
                    }
                    else if (MethodSymbol.ContainingSymbol is SourcePropertySymbol propertySymbol &&
                             propertySymbol.BackingField is SourceFieldSymbol backingField)
                    {
                        EmitAutoPropertyAccessor(accessorDeclaration, propertySymbol, backingField);
                    }
                    else
                    {
                        ILGenerator.Emit(OpCodes.Ret);
                    }
                    break;
                }

            case PropertyDeclarationSyntax propertyDeclaration:
                if (expressionBody is not null)
                {
                    if (expressionBodySyntax is not null)
                        EmitSequencePoint(expressionBodySyntax);

                    new ExpressionGenerator(baseGenerator, expressionBody).Emit();
                    ILGenerator.Emit(OpCodes.Ret);
                }
                else
                {
                    ILGenerator.Emit(OpCodes.Ret);
                }
                break;

            case DelegateDeclarationSyntax:
                {
                    ILGenerator.Emit(OpCodes.Ret);
                }
                break;

            case EventDeclarationSyntax:
                {
                    if (MethodSymbol.ContainingSymbol is SourceEventSymbol eventSymbol &&
                        eventSymbol.BackingField is SourceFieldSymbol backingField)
                    {
                        EmitAutoEventAccessor(eventSymbol, backingField);
                    }
                    else
                    {
                        ILGenerator.Emit(OpCodes.Ret);
                    }
                    break;
                }

            case ClassDeclarationSyntax:
            case RecordDeclarationSyntax:
            case StructDeclarationSyntax:
                if (!MethodSymbol.IsStatic)
                {
                    EmitConstructorInitializer();
                }

                EmitFieldInitializers(MethodSymbol.IsStatic);

                ILGenerator.Emit(OpCodes.Ret);
                break;

            case UnionCaseClauseSyntax:
                EmitUnionCaseConstructor();
                break;

            default:
                throw new InvalidOperationException($"Unsupported syntax node in MethodBodyGenerator: {syntax.GetType().Name}");
        }
    }

    private void EmitEntryPointBridge(IMethodSymbol bridgeMethod, IMethodSymbol implementationMethod)
    {
        var codeGen = MethodGenerator.TypeGenerator.CodeGen;
        var implementationMethodInfo = codeGen.RuntimeSymbolResolver.GetMethodInfo(implementationMethod);

        if (bridgeMethod.Parameters.Length == 1)
            ILGenerator.Emit(OpCodes.Ldarg_0);

        ILGenerator.Emit(GetCallOpCode(implementationMethod, implementationMethodInfo), implementationMethodInfo);

        var producedType = implementationMethod.ReturnType;

        if (AwaitablePattern.TryFind(implementationMethod.ReturnType, isAccessible: null, out var awaitable, out _, out _))
        {
            var getAwaiter = codeGen.RuntimeSymbolResolver.GetMethodInfo(awaitable.GetAwaiterMethod);
            ILGenerator.Emit(GetCallOpCode(awaitable.GetAwaiterMethod, getAwaiter), getAwaiter);

            var awaiterType = codeGen.RuntimeSymbolResolver.GetType(awaitable.AwaiterType, treatUnitAsVoid: true);
            var awaiterLocal = ILGenerator.DeclareLocal(awaiterType);
            awaiterLocal.SetLocalSymInfo("awaiter");
            ILGenerator.Emit(OpCodes.Stloc, awaiterLocal);
            ILGenerator.Emit(OpCodes.Ldloca, awaiterLocal);

            var getResult = codeGen.RuntimeSymbolResolver.GetMethodInfo(awaitable.GetResultMethod);
            ILGenerator.Emit(GetCallOpCode(awaitable.GetResultMethod, getResult), getResult);
            producedType = awaitable.GetResultMethod.ReturnType;
        }

        if (EntryPointSignature.TryGetResultPayloadTypes(producedType, out _, out _, out _) &&
            producedType is INamedTypeSymbol resultType)
        {
            EmitResultEntryPointBridgeReturn(bridgeMethod, resultType);
            return;
        }

        if (bridgeMethod.ReturnType.SpecialType == SpecialType.System_Unit)
        {
            if (producedType.SpecialType is not SpecialType.System_Void
                and not SpecialType.System_Unit)
                ILGenerator.Emit(OpCodes.Pop);
            ILGenerator.Emit(OpCodes.Ret);
            return;
        }

        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitResultEntryPointBridgeReturn(IMethodSymbol bridgeMethod, INamedTypeSymbol resultType)
    {
        var codeGen = MethodGenerator.TypeGenerator.CodeGen;
        var resultClrType = codeGen.RuntimeSymbolResolver.GetType(resultType, treatUnitAsVoid: true);
        var resultLocal = ILGenerator.DeclareLocal(resultClrType);
        resultLocal.SetLocalSymInfo("entryResult");
        ILGenerator.Emit(OpCodes.Stloc, resultLocal);

        var okMethod = FindSingleOutBoolMethod(resultType, "TryGetOk");
        var errorMethod = FindSingleOutBoolMethod(resultType, "TryGetError");

        var doneLabel = ILGenerator.DefineLabel();
        var isIntBridge = bridgeMethod.ReturnType.SpecialType == SpecialType.System_Int32;

        if (isIntBridge && okMethod is not null)
        {
            var okMethodInfo = codeGen.RuntimeSymbolResolver.GetMethodInfo(okMethod);
            var okCaseRuntimeType = TryGetOutLocalElementType(okMethodInfo, resultClrType) ?? typeof(object);
            var okCaseLocal = ILGenerator.DeclareLocal(okCaseRuntimeType);
            okCaseLocal.SetLocalSymInfo("okCase");

            var nextLabel = ILGenerator.DefineLabel();
            EmitLoadInstanceReceiver(resultLocal, resultClrType);
            ILGenerator.Emit(OpCodes.Ldloca, okCaseLocal);
            ILGenerator.Emit(GetCallOpCode(okMethod, okMethodInfo), okMethodInfo);
            ILGenerator.Emit(OpCodes.Brfalse_S, nextLabel);
            EmitLoadCasePayload(okCaseLocal, okCaseRuntimeType, okMethod.Parameters[0].Type, "Value");
            ILGenerator.Emit(OpCodes.Ret);
            ILGenerator.MarkLabel(nextLabel);
        }

        if (errorMethod is not null)
        {
            var errorMethodInfo = codeGen.RuntimeSymbolResolver.GetMethodInfo(errorMethod);
            var errorCaseRuntimeType = TryGetOutLocalElementType(errorMethodInfo, resultClrType) ?? typeof(object);
            var errorCaseLocal = ILGenerator.DeclareLocal(errorCaseRuntimeType);
            errorCaseLocal.SetLocalSymInfo("errorCase");

            var nextLabel = ILGenerator.DefineLabel();
            EmitLoadInstanceReceiver(resultLocal, resultClrType);
            ILGenerator.Emit(OpCodes.Ldloca, errorCaseLocal);
            ILGenerator.Emit(GetCallOpCode(errorMethod, errorMethodInfo), errorMethodInfo);
            ILGenerator.Emit(OpCodes.Brfalse_S, nextLabel);
            EmitWriteCasePayloadToStderr(errorCaseLocal, errorCaseRuntimeType, errorMethod.Parameters[0].Type, "Error");
            ILGenerator.Emit(OpCodes.Br_S, doneLabel);
            ILGenerator.MarkLabel(nextLabel);
        }

        ILGenerator.MarkLabel(doneLabel);

        if (isIntBridge)
            ILGenerator.Emit(OpCodes.Ldc_I4_0);

        ILGenerator.Emit(OpCodes.Ret);
    }

    private static Type? TryGetOutLocalElementType(MethodInfo methodInfo, Type carrierRuntimeType)
    {
        var parameters = methodInfo.GetParameters();
        if (parameters.Length != 1)
            return null;

        var parameterType = parameters[0].ParameterType;
        if (!parameterType.IsByRef)
            return null;

        var elementType = parameterType.GetElementType();
        if (elementType is null)
            return null;

        var closed = CloseTypeFromMethodContext(elementType, methodInfo.DeclaringType);
        return CloseNestedCarrierCaseType(closed, carrierRuntimeType);
    }

    private static Type CloseTypeFromMethodContext(Type type, Type? declaringType)
    {
        if (type.IsByRef)
            return CloseTypeFromMethodContext(type.GetElementType()!, declaringType).MakeByRefType();

        if (type.IsPointer)
            return CloseTypeFromMethodContext(type.GetElementType()!, declaringType).MakePointerType();

        if (type.IsArray)
        {
            var closedElement = CloseTypeFromMethodContext(type.GetElementType()!, declaringType);
            return type.GetArrayRank() == 1
                ? closedElement.MakeArrayType()
                : closedElement.MakeArrayType(type.GetArrayRank());
        }

        if (type.IsGenericParameter)
        {
            if (type.DeclaringMethod is not null)
                return type;

            if (declaringType is { IsGenericType: true, ContainsGenericParameters: false })
            {
                var typeArguments = declaringType.GetGenericArguments();
                var ordinal = type.GenericParameterPosition;
                if ((uint)ordinal < (uint)typeArguments.Length)
                    return typeArguments[ordinal];
            }

            return type;
        }

        if (!type.IsGenericType)
            return type;

        var definition = type.IsGenericTypeDefinition ? type : type.GetGenericTypeDefinition();
        var arguments = type.GetGenericArguments();
        var substituted = new Type[arguments.Length];
        var changed = false;

        for (var i = 0; i < arguments.Length; i++)
        {
            var updated = CloseTypeFromMethodContext(arguments[i], declaringType);
            substituted[i] = updated;
            if (!ReferenceEquals(updated, arguments[i]))
                changed = true;
        }

        if (!changed)
            return type;

        return definition.MakeGenericType(substituted);
    }

    private static Type CloseNestedCarrierCaseType(Type caseType, Type carrierType)
    {
        if (!caseType.ContainsGenericParameters)
            return caseType;

        if (!carrierType.IsGenericType || carrierType.ContainsGenericParameters)
            return caseType;

        var flags = BindingFlags.Public | BindingFlags.NonPublic;
        var nestedType = carrierType.GetNestedType(caseType.Name, flags);
        if (nestedType is null)
        {
            var caseBaseName = caseType.Name.Split('`')[0];
            nestedType = carrierType.GetNestedTypes(flags)
                .FirstOrDefault(type =>
                    string.Equals(type.Name, caseType.Name, StringComparison.Ordinal) ||
                    string.Equals(type.Name.Split('`')[0], caseBaseName, StringComparison.Ordinal));
        }

        if (nestedType is not null && nestedType.ContainsGenericParameters)
        {
            var nestedDefinition = nestedType.IsGenericTypeDefinition
                ? nestedType
                : nestedType.GetGenericTypeDefinition();
            var carrierArguments = carrierType.GetGenericArguments();
            if (nestedDefinition.GetGenericArguments().Length == carrierArguments.Length)
            {
                nestedType = nestedDefinition.MakeGenericType(carrierArguments);
            }
        }

        return nestedType ?? caseType;
    }

    private void EmitWriteCasePayloadToStderr(
        IILocal caseLocal,
        Type caseRuntimeType,
        ITypeSymbol caseSymbolType,
        string preferredPropertyName)
    {
        var payloadGetterSymbol = ResolveCasePayloadGetter(caseSymbolType, preferredPropertyName);

        if (payloadGetterSymbol is null)
        {
            EmitWriteLocalObjectToStderr(caseLocal, caseRuntimeType);
            return;
        }

        var payloadTypeSymbol = payloadGetterSymbol.ReturnType;
        var payloadGetter = MethodGenerator.TypeGenerator.CodeGen.RuntimeSymbolResolver.GetMethodInfo(payloadGetterSymbol);
        ILGenerator.Emit(OpCodes.Call, ConsoleErrorGetter);
        EmitLoadInstanceReceiver(caseLocal, caseRuntimeType);
        ILGenerator.Emit(OpCodes.Call, payloadGetter);

        if (!payloadTypeSymbol.IsReferenceType)
        {
            var payloadRuntimeType = MethodGenerator.TypeGenerator.CodeGen.RuntimeSymbolResolver.GetType(payloadTypeSymbol, treatUnitAsVoid: true);
            ILGenerator.Emit(OpCodes.Box, payloadRuntimeType);
        }

        ILGenerator.Emit(OpCodes.Callvirt, TextWriterWriteLineObject);
    }

    private void EmitLoadCasePayload(
        IILocal caseLocal,
        Type caseRuntimeType,
        ITypeSymbol caseSymbolType,
        string preferredPropertyName)
    {
        var payloadGetterSymbol = ResolveCasePayloadGetter(caseSymbolType, preferredPropertyName);
        if (payloadGetterSymbol is null)
            throw new InvalidOperationException("Could not resolve case payload getter.");

        EmitLoadInstanceReceiver(caseLocal, caseRuntimeType);
        var payloadGetter = MethodGenerator.TypeGenerator.CodeGen.RuntimeSymbolResolver.GetMethodInfo(payloadGetterSymbol);
        ILGenerator.Emit(OpCodes.Call, payloadGetter);
    }

    private static IMethodSymbol? ResolveCasePayloadGetter(ITypeSymbol caseSymbolType, string preferredPropertyName)
    {
        var normalizedCaseType = caseSymbolType.GetPlainType();
        if (normalizedCaseType is ByRefTypeSymbol byRef)
            normalizedCaseType = byRef.ElementType;

        var caseNamedType = normalizedCaseType as INamedTypeSymbol;
        var payloadGetterSymbol = caseNamedType?
            .GetMembers(preferredPropertyName)
            .OfType<IPropertySymbol>()
            .FirstOrDefault(property => property.GetMethod is not null)
            ?.GetMethod;

        if (payloadGetterSymbol is null)
        {
            payloadGetterSymbol = caseNamedType?
                .GetMembers()
                .OfType<IPropertySymbol>()
                .FirstOrDefault(property => property.GetMethod is not null)
                ?.GetMethod;
        }

        return payloadGetterSymbol;
    }

    private void EmitLoadInstanceReceiver(IILocal local, Type localRuntimeType)
    {
        if (localRuntimeType.IsValueType)
        {
            ILGenerator.Emit(OpCodes.Ldloca, local);
            return;
        }

        ILGenerator.Emit(OpCodes.Ldloc, local);
    }

    private void EmitWriteLocalObjectToStderr(IILocal local, Type runtimeType)
    {
        ILGenerator.Emit(OpCodes.Call, ConsoleErrorGetter);
        ILGenerator.Emit(OpCodes.Ldloc, local);
        if (runtimeType.IsValueType)
            ILGenerator.Emit(OpCodes.Box, runtimeType);
        ILGenerator.Emit(OpCodes.Callvirt, TextWriterWriteLineObject);
    }

    private static IMethodSymbol? FindSingleOutBoolMethod(INamedTypeSymbol type, string methodName)
    {
        return type.GetMembers(methodName)
            .OfType<IMethodSymbol>()
            .FirstOrDefault(method =>
                method.Parameters.Length == 1 &&
                method.Parameters[0].RefKind == RefKind.Out &&
                method.ReturnType.SpecialType == SpecialType.System_Boolean);
    }



    private static OpCode GetCallOpCode(IMethodSymbol methodSymbol, MethodInfo methodInfo)
    {
        if (methodSymbol.IsStatic)
            return OpCodes.Call;

        return methodInfo.DeclaringType is { IsValueType: true }
            ? OpCodes.Call
            : OpCodes.Callvirt;
    }

    private void EmitIteratorMethod(SynthesizedIteratorTypeSymbol iteratorType)
    {
        if (SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.Constructor))
        {
            EmitConstructorInitializer();
            ILGenerator.Emit(OpCodes.Ret);
            return;
        }

        var body = GetIteratorBody(iteratorType);
        if (body is null)
        {
            ILGenerator.Emit(OpCodes.Ret);
            return;
        }

        DeclareLocals(body);
        EmitMethodBlock(body);
    }

    private void EmitAsyncStateMachineMethod(SynthesizedAsyncStateMachineTypeSymbol asyncStateMachine)
    {
        var previousClosure = _lambdaClosure;

        if (_lambdaClosure is null)
            _lambdaClosure = GetAsyncLambdaClosure(asyncStateMachine);

        if (SymbolEqualityComparer.Default.Equals(MethodSymbol, asyncStateMachine.Constructor))
        {
            ILGenerator.Emit(OpCodes.Ret);
            _lambdaClosure = previousClosure;
            return;
        }

        var body = GetAsyncStateMachineBody(asyncStateMachine);
        if (body is null)
        {
            ILGenerator.Emit(OpCodes.Ret);
            _lambdaClosure = previousClosure;
            return;
        }

        try
        {
            DeclareLocals(body);
            EmitMethodBlock(body);
        }
        finally
        {
            _lambdaClosure = previousClosure;
        }
    }

    private TypeGenerator.LambdaClosure? GetAsyncLambdaClosure(SynthesizedAsyncStateMachineTypeSymbol asyncStateMachine)
    {
        if (asyncStateMachine.AsyncMethod is not SourceLambdaSymbol { HasCaptures: true } lambda)
            return null;

        if (MethodGenerator.LambdaClosure is { } existing)
            return existing;

        if (lambda.ContainingType is null)
            return null;

        var containingType = asyncStateMachine.AsyncMethod.ContainingType ?? lambda.ContainingType;
        var containingGenerator = MethodGenerator.TypeGenerator.CodeGen
            .GetOrCreateTypeGenerator(containingType);

        var lambdaMethodGenerator = containingGenerator.GetMethodGenerator(lambda);
        if (lambdaMethodGenerator?.LambdaClosure is { } lambdaClosure)
            return lambdaClosure;

        if (containingGenerator.TryGetLambdaClosure(lambda, out var closure))
            return closure;

        if (containingGenerator.TypeBuilder is null)
            return null;

        return containingGenerator.EnsureLambdaClosure(lambda);
    }

    private BoundBlockStatement? GetAsyncStateMachineBody(SynthesizedAsyncStateMachineTypeSymbol asyncStateMachine)
    {
        if (SymbolEqualityComparer.Default.Equals(MethodSymbol, asyncStateMachine.MoveNextMethod))
            return asyncStateMachine.MoveNextBody;

        if (SymbolEqualityComparer.Default.Equals(MethodSymbol, asyncStateMachine.SetStateMachineMethod))
            return asyncStateMachine.SetStateMachineBody;

        return null;
    }

    private BoundBlockStatement? GetIteratorBody(SynthesizedIteratorTypeSymbol iteratorType)
    {
        if (SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.MoveNextMethod))
            return iteratorType.MoveNextBody;

        if (SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.DisposeMethod))
            return iteratorType.DisposeBody;

        if (SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.ResetMethod))
            return iteratorType.ResetBody;

        if (SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.CurrentProperty.GetMethod))
            return iteratorType.CurrentGetterBody;

        if (SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.NonGenericCurrentProperty.GetMethod))
            return iteratorType.NonGenericCurrentGetterBody;

        if (iteratorType.GenericGetEnumeratorMethod is not null &&
            SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.GenericGetEnumeratorMethod))
            return iteratorType.GenericGetEnumeratorBody;

        if (iteratorType.NonGenericGetEnumeratorMethod is not null &&
            SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.NonGenericGetEnumeratorMethod))
            return iteratorType.NonGenericGetEnumeratorBody;

        return null;
    }

    public void EmitLambda(
        BoundLambdaExpression lambda,
        TypeGenerator.LambdaClosure? closure,
        BoundBlockStatement? rewrittenBody = null)
    {
        baseGenerator = new BaseGenerator(this);
        scope = new Scope(baseGenerator);

        ILGenerator = MethodGenerator.ILBuilderFactory.Create(MethodGenerator);

        _lambdaClosure = closure;

        try
        {
            if (_lambdaClosure is not null)
                InitializeCapturedParameters();

            var block = rewrittenBody ?? CreateLambdaBody(lambda);
            DeclareLocals(block);
            EmitMethodBlock(block);
        }
        finally
        {
            _lambdaClosure = null;
        }
    }

    private static BoundBlockStatement CreateLambdaBody(BoundLambdaExpression lambda)
    {
        if (lambda.Body is BoundBlockExpression blockExpression)
            return new BoundBlockStatement(blockExpression.Statements, blockExpression.LocalsToDispose);

        var statements = new List<BoundStatement>();

        if (lambda.ReturnType.SpecialType == SpecialType.System_Unit)
        {
            statements.Add(new BoundExpressionStatement(lambda.Body));
        }
        else
        {
            statements.Add(new BoundReturnStatement(lambda.Body));
        }

        return new BoundBlockStatement(statements);
    }

    private void InitializeCapturedParameters()
    {
        if (_lambdaClosure is null)
            return;

        foreach (var parameter in MethodSymbol.Parameters)
        {
            if (!_lambdaClosure.TryGetField(parameter, out var fieldBuilder))
                continue;

            ILGenerator.Emit(OpCodes.Ldarg_0);

            var parameterBuilder = MethodGenerator.GetParameterBuilder(parameter);
            var position = parameterBuilder.Position;
            if (MethodSymbol.IsStatic)
                position -= 1;

            ILGenerator.Emit(OpCodes.Ldarg, position);
            ILGenerator.Emit(OpCodes.Stfld, fieldBuilder);
        }
    }

    private void EmitFieldInitializers(bool isStatic)
    {
        var fields = MethodSymbol.ContainingType!
            .GetMembers()
            .OfType<SourceFieldSymbol>()
            .Where(f => f.IsStatic == isStatic && f.Initializer is not null);

        foreach (var field in fields)
        {
            BoundExpression assignment = new BoundFieldAssignmentExpression(
                isStatic ? null : new BoundSelfExpression(MethodSymbol.ContainingType!),
                field,
                field.Initializer!,
                Compilation.GetSpecialType(SpecialType.System_Unit));

            var statement = new BoundAssignmentStatement((BoundAssignmentExpression)assignment);
            new StatementGenerator(baseGenerator, statement).Emit();
        }
    }

    private void EmitAutoPropertyAccessor(
        AccessorDeclarationSyntax accessorDeclaration,
        SourcePropertySymbol propertySymbol,
        SourceFieldSymbol backingField)
    {
        var accessorKind = accessorDeclaration.Kind == SyntaxKind.GetAccessorDeclaration
            ? MethodKind.PropertyGet
            : MethodKind.PropertySet;
        EmitAutoPropertyAccessor(accessorKind, propertySymbol, backingField);
    }

    private void EmitAutoPropertyAccessor(
        MethodKind accessorKind,
        SourcePropertySymbol propertySymbol,
        SourceFieldSymbol backingField)
    {
        var fieldInfo = backingField.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);
        var isGet = accessorKind == MethodKind.PropertyGet;

        if (isGet)
        {
            if (propertySymbol.IsStatic)
            {
                ILGenerator.Emit(OpCodes.Ldsfld, fieldInfo);
            }
            else
            {
                ILGenerator.Emit(OpCodes.Ldarg_0);
                ILGenerator.Emit(OpCodes.Ldfld, fieldInfo);
            }
        }
        else
        {
            if (!propertySymbol.IsStatic)
            {
                ILGenerator.Emit(OpCodes.Ldarg_0);
                ILGenerator.Emit(OpCodes.Ldarg_1);
                ILGenerator.Emit(OpCodes.Stfld, fieldInfo);
            }
            else
            {
                ILGenerator.Emit(OpCodes.Ldarg_0);
                ILGenerator.Emit(OpCodes.Stsfld, fieldInfo);
            }
        }

        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitAutoEventAccessor(SourceEventSymbol eventSymbol, SourceFieldSymbol backingField)
    {
        var fieldInfo = backingField.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);
        var delegateType = MethodGenerator.ResolveClrType(eventSymbol.Type);
        var combineMethod = typeof(Delegate).GetMethod(nameof(Delegate.Combine), [typeof(Delegate), typeof(Delegate)]);
        var removeMethod = typeof(Delegate).GetMethod(nameof(Delegate.Remove), [typeof(Delegate), typeof(Delegate)]);
        var updateMethod = MethodSymbol.MethodKind == MethodKind.EventAdd ? combineMethod : removeMethod;

        if (updateMethod is null)
            throw new InvalidOperationException("Delegate combine/remove method not found.");

        if (eventSymbol.IsStatic)
        {
            ILGenerator.Emit(OpCodes.Ldsfld, fieldInfo);
            ILGenerator.Emit(OpCodes.Ldarg_0);
            ILGenerator.Emit(OpCodes.Call, updateMethod);
            ILGenerator.Emit(OpCodes.Castclass, delegateType);
            ILGenerator.Emit(OpCodes.Stsfld, fieldInfo);
        }
        else
        {
            ILGenerator.Emit(OpCodes.Ldarg_0);
            ILGenerator.Emit(OpCodes.Dup);
            ILGenerator.Emit(OpCodes.Ldfld, fieldInfo);
            ILGenerator.Emit(OpCodes.Ldarg_1);
            ILGenerator.Emit(OpCodes.Call, updateMethod);
            ILGenerator.Emit(OpCodes.Castclass, delegateType);
            ILGenerator.Emit(OpCodes.Stfld, fieldInfo);
        }

        ILGenerator.Emit(OpCodes.Ret);
    }

    private bool TryEmitRecordMethod(SourceNamedTypeSymbol recordType)
    {
        if (MethodSymbol.DeclaringSyntaxReferences.IsDefaultOrEmpty &&
            MethodSymbol.MethodKind is MethodKind.PropertyGet or MethodKind.PropertySet or MethodKind.InitOnly &&
            MethodSymbol.ContainingSymbol is SourcePropertySymbol propertySymbol &&
            propertySymbol.BackingField is SourceFieldSymbol backingField)
        {
            EmitAutoPropertyAccessor(MethodSymbol.MethodKind, propertySymbol, backingField);
            return true;
        }

        if (!MethodSymbol.DeclaringSyntaxReferences.IsDefaultOrEmpty)
            return false;

        if (MethodSymbol.Name == nameof(object.Equals) &&
            MethodSymbol.Parameters.Length == 1 &&
            MethodSymbol.ReturnType.SpecialType == SpecialType.System_Boolean)
        {
            if (MethodSymbol.Parameters[0].Type.SpecialType == SpecialType.System_Object)
            {
                EmitRecordObjectEquals(recordType);
                return true;
            }

            if (SymbolEqualityComparer.Default.Equals(MethodSymbol.Parameters[0].Type, recordType))
            {
                EmitRecordTypedEquals(recordType);
                return true;
            }
        }

        if (MethodSymbol.Name == nameof(object.GetHashCode) &&
            MethodSymbol.Parameters.Length == 0 &&
            MethodSymbol.ReturnType.SpecialType == SpecialType.System_Int32)
        {
            EmitRecordGetHashCode(recordType);
            return true;
        }

        if (MethodSymbol.Name == nameof(object.ToString) &&
            MethodSymbol.Parameters.Length == 0 &&
            MethodSymbol.ReturnType.SpecialType == SpecialType.System_String)
        {
            EmitRecordToString(recordType);
            return true;
        }

        if (MethodSymbol.Name == "Deconstruct" &&
            MethodSymbol.MethodKind == MethodKind.Ordinary &&
            MethodSymbol.ReturnType.SpecialType == SpecialType.System_Unit)
        {
            EmitRecordDeconstruct(recordType);
            return true;
        }

        if (MethodSymbol.MethodKind == MethodKind.Constructor &&
            MethodSymbol.Parameters.Length == 1 &&
            SymbolEqualityComparer.Default.Equals(MethodSymbol.Parameters[0].Type, recordType))
        {
            EmitRecordCopyConstructor(recordType);
            return true;
        }

        if (MethodSymbol.MethodKind == MethodKind.UserDefinedOperator &&
            MethodSymbol.Parameters.Length == 2 &&
            MethodSymbol.ReturnType.SpecialType == SpecialType.System_Boolean &&
            SymbolEqualityComparer.Default.Equals(MethodSymbol.Parameters[0].Type, recordType) &&
            SymbolEqualityComparer.Default.Equals(MethodSymbol.Parameters[1].Type, recordType))
        {
            if (string.Equals(MethodSymbol.Name, "op_Equality", StringComparison.Ordinal))
            {
                EmitRecordEqualityOperator(recordType);
                return true;
            }

            if (string.Equals(MethodSymbol.Name, "op_Inequality", StringComparison.Ordinal))
            {
                EmitRecordInequalityOperator(recordType);
                return true;
            }
        }

        return false;
    }

    private void EmitRecordCopyConstructor(SourceNamedTypeSymbol recordType)
    {
        var baseCtor = GetBaseConstructor();
        ILGenerator.Emit(OpCodes.Ldarg_0);
        ILGenerator.Emit(OpCodes.Call, baseCtor);

        foreach (var property in recordType.RecordProperties)
        {
            if (property.BackingField is not SourceFieldSymbol backingField)
                continue;

            var fieldInfo = backingField.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);

            ILGenerator.Emit(OpCodes.Ldarg_0);
            ILGenerator.Emit(OpCodes.Ldarg_1);
            ILGenerator.Emit(OpCodes.Ldfld, fieldInfo);
            ILGenerator.Emit(OpCodes.Stfld, fieldInfo);
        }

        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitRecordObjectEquals(SourceNamedTypeSymbol recordType)
    {
        var recordClrType = MethodGenerator.ResolveClrType(recordType);
        var otherLocal = ILGenerator.DeclareLocal(recordClrType);
        var compareFieldsLabel = ILGenerator.DefineLabel();
        var returnFalseLabel = ILGenerator.DefineLabel();

        ILGenerator.Emit(OpCodes.Ldarg_1);
        ILGenerator.Emit(OpCodes.Isinst, recordClrType);
        ILGenerator.Emit(OpCodes.Stloc, otherLocal);
        ILGenerator.Emit(OpCodes.Ldloc, otherLocal);
        ILGenerator.Emit(OpCodes.Brtrue_S, compareFieldsLabel);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Ret);

        ILGenerator.MarkLabel(compareFieldsLabel);

        var referenceEquals = typeof(object).GetMethod(nameof(object.ReferenceEquals), [typeof(object), typeof(object)])!;
        var fieldComparisonLabel = ILGenerator.DefineLabel();
        ILGenerator.Emit(OpCodes.Ldarg_0);
        ILGenerator.Emit(OpCodes.Ldloc, otherLocal);
        ILGenerator.Emit(OpCodes.Call, referenceEquals);
        ILGenerator.Emit(OpCodes.Brfalse_S, fieldComparisonLabel);
        ILGenerator.Emit(OpCodes.Ldc_I4_1);
        ILGenerator.Emit(OpCodes.Ret);

        ILGenerator.MarkLabel(fieldComparisonLabel);
        EmitRecordFieldComparisons(
            recordType,
            loadLeft: () => ILGenerator.Emit(OpCodes.Ldarg_0),
            loadRight: () => ILGenerator.Emit(OpCodes.Ldloc, otherLocal),
            returnFalseLabel);

        ILGenerator.Emit(OpCodes.Ldc_I4_1);
        ILGenerator.Emit(OpCodes.Ret);

        ILGenerator.MarkLabel(returnFalseLabel);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitRecordTypedEquals(SourceNamedTypeSymbol recordType)
    {
        var compareFieldsLabel = ILGenerator.DefineLabel();
        var returnFalseLabel = ILGenerator.DefineLabel();

        ILGenerator.Emit(OpCodes.Ldarg_1);
        ILGenerator.Emit(OpCodes.Brtrue_S, compareFieldsLabel);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Ret);

        ILGenerator.MarkLabel(compareFieldsLabel);

        var referenceEquals = typeof(object).GetMethod(nameof(object.ReferenceEquals), [typeof(object), typeof(object)])!;
        var fieldComparisonLabel = ILGenerator.DefineLabel();
        ILGenerator.Emit(OpCodes.Ldarg_0);
        ILGenerator.Emit(OpCodes.Ldarg_1);
        ILGenerator.Emit(OpCodes.Call, referenceEquals);
        ILGenerator.Emit(OpCodes.Brfalse_S, fieldComparisonLabel);
        ILGenerator.Emit(OpCodes.Ldc_I4_1);
        ILGenerator.Emit(OpCodes.Ret);

        ILGenerator.MarkLabel(fieldComparisonLabel);
        EmitRecordFieldComparisons(
            recordType,
            loadLeft: () => ILGenerator.Emit(OpCodes.Ldarg_0),
            loadRight: () => ILGenerator.Emit(OpCodes.Ldarg_1),
            returnFalseLabel);

        ILGenerator.Emit(OpCodes.Ldc_I4_1);
        ILGenerator.Emit(OpCodes.Ret);

        ILGenerator.MarkLabel(returnFalseLabel);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitRecordEqualityOperator(SourceNamedTypeSymbol recordType)
    {
        var leftNotNullLabel = ILGenerator.DefineLabel();
        var compareFieldsLabel = ILGenerator.DefineLabel();

        var referenceEquals = typeof(object).GetMethod(nameof(object.ReferenceEquals), [typeof(object), typeof(object)])!;
        ILGenerator.Emit(OpCodes.Ldarg_0);
        ILGenerator.Emit(OpCodes.Ldarg_1);
        ILGenerator.Emit(OpCodes.Call, referenceEquals);
        ILGenerator.Emit(OpCodes.Brfalse_S, compareFieldsLabel);
        ILGenerator.Emit(OpCodes.Ldc_I4_1);
        ILGenerator.Emit(OpCodes.Ret);

        ILGenerator.MarkLabel(compareFieldsLabel);
        ILGenerator.Emit(OpCodes.Ldarg_0);
        ILGenerator.Emit(OpCodes.Brtrue_S, leftNotNullLabel);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Ret);

        ILGenerator.MarkLabel(leftNotNullLabel);
        var rightNotNullLabel = ILGenerator.DefineLabel();
        ILGenerator.Emit(OpCodes.Ldarg_1);
        ILGenerator.Emit(OpCodes.Brtrue_S, rightNotNullLabel);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Ret);

        ILGenerator.MarkLabel(rightNotNullLabel);
        var fieldComparisonFail = ILGenerator.DefineLabel();
        EmitRecordFieldComparisons(
            recordType,
            loadLeft: () => ILGenerator.Emit(OpCodes.Ldarg_0),
            loadRight: () => ILGenerator.Emit(OpCodes.Ldarg_1),
            fieldComparisonFail);

        ILGenerator.Emit(OpCodes.Ldc_I4_1);
        ILGenerator.Emit(OpCodes.Ret);

        ILGenerator.MarkLabel(fieldComparisonFail);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitRecordInequalityOperator(SourceNamedTypeSymbol recordType)
    {
        var returnTrueLabel = ILGenerator.DefineLabel();
        var compareFieldsLabel = ILGenerator.DefineLabel();

        var referenceEquals = typeof(object).GetMethod(nameof(object.ReferenceEquals), [typeof(object), typeof(object)])!;
        ILGenerator.Emit(OpCodes.Ldarg_0);
        ILGenerator.Emit(OpCodes.Ldarg_1);
        ILGenerator.Emit(OpCodes.Call, referenceEquals);
        ILGenerator.Emit(OpCodes.Brfalse_S, compareFieldsLabel);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Ret);

        ILGenerator.MarkLabel(compareFieldsLabel);
        ILGenerator.Emit(OpCodes.Ldarg_0);
        ILGenerator.Emit(OpCodes.Brfalse_S, returnTrueLabel);
        ILGenerator.Emit(OpCodes.Ldarg_1);
        ILGenerator.Emit(OpCodes.Brfalse_S, returnTrueLabel);

        EmitRecordFieldComparisons(
            recordType,
            loadLeft: () => ILGenerator.Emit(OpCodes.Ldarg_0),
            loadRight: () => ILGenerator.Emit(OpCodes.Ldarg_1),
            returnTrueLabel);

        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Ret);

        ILGenerator.MarkLabel(returnTrueLabel);
        ILGenerator.Emit(OpCodes.Ldc_I4_1);
        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitRecordGetHashCode(SourceNamedTypeSymbol recordType)
    {
        var recordProperties = recordType.RecordProperties;
        var hashCodeType = typeof(HashCode);
        var addMethod = hashCodeType.GetMethods()
            .First(m => m.Name == nameof(HashCode.Add) && m.IsGenericMethodDefinition && m.GetParameters().Length == 1);
        var toHashCodeMethod = hashCodeType.GetMethod(nameof(HashCode.ToHashCode), Type.EmptyTypes)!;

        var hashLocal = ILGenerator.DeclareLocal(hashCodeType);
        ILGenerator.Emit(OpCodes.Ldloca, hashLocal);
        ILGenerator.Emit(OpCodes.Initobj, hashCodeType);

        foreach (var property in recordProperties)
        {
            if (property.BackingField is not SourceFieldSymbol backingField)
                continue;

            var fieldInfo = backingField.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);
            var propertyClrType = MethodGenerator.ResolveClrType(property.Type);

            ILGenerator.Emit(OpCodes.Ldloca, hashLocal);
            ILGenerator.Emit(OpCodes.Ldarg_0);
            ILGenerator.Emit(OpCodes.Ldfld, fieldInfo);
            ILGenerator.Emit(OpCodes.Call, addMethod.MakeGenericMethod(propertyClrType));
        }

        ILGenerator.Emit(OpCodes.Ldloca, hashLocal);
        ILGenerator.Emit(OpCodes.Call, toHashCodeMethod);
        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitRecordToString(SourceNamedTypeSymbol recordType)
    {
        var recordProperties = recordType.RecordProperties;
        var builderType = typeof(StringBuilder);
        var builderCtor = builderType.GetConstructor(Type.EmptyTypes)!;
        var appendString = builderType.GetMethod(nameof(StringBuilder.Append), new[] { typeof(string) })!;
        var appendChar = builderType.GetMethod(nameof(StringBuilder.Append), new[] { typeof(char) })!;
        var builderToString = builderType.GetMethod(nameof(StringBuilder.ToString), Type.EmptyTypes)!;
        var stringIndexOf = typeof(string).GetMethod(nameof(string.IndexOf), new[] { typeof(char) })!;
        var stringSubstring = typeof(string).GetMethod(nameof(string.Substring), new[] { typeof(int), typeof(int) })!;
        var stringReplace = typeof(string).GetMethod(nameof(string.Replace), new[] { typeof(string), typeof(string) })!;
        var objectToString = typeof(object).GetMethod(nameof(ToString), Type.EmptyTypes)!;
        var typeType = typeof(Type);
        var typeFromHandle = typeType.GetMethod(nameof(Type.GetTypeFromHandle), new[] { typeof(RuntimeTypeHandle) })!;
        var typeNameGetter = typeType.GetProperty(nameof(Type.Name))!.GetMethod!;
        var typeGenericArgumentsGetter = typeType.GetProperty(nameof(Type.GenericTypeArguments))!.GetMethod!;

        var builderLocal = ILGenerator.DeclareLocal(builderType);
        builderLocal.SetLocalSymInfo("builder");
        var typeLocal = ILGenerator.DeclareLocal(typeType);
        typeLocal.SetLocalSymInfo("recordType");
        var nameLocal = ILGenerator.DeclareLocal(typeof(string));
        nameLocal.SetLocalSymInfo("name");
        var tickIndexLocal = ILGenerator.DeclareLocal(typeof(int));
        tickIndexLocal.SetLocalSymInfo("tickIndex");
        var argsLocal = ILGenerator.DeclareLocal(typeof(Type[]));
        argsLocal.SetLocalSymInfo("typeArgs");
        var argsLengthLocal = ILGenerator.DeclareLocal(typeof(int));
        argsLengthLocal.SetLocalSymInfo("typeArgLength");
        var argIndexLocal = ILGenerator.DeclareLocal(typeof(int));
        argIndexLocal.SetLocalSymInfo("typeArgIndex");
        var argTypeLocal = ILGenerator.DeclareLocal(typeType);
        argTypeLocal.SetLocalSymInfo("typeArg");
        var argNameLocal = ILGenerator.DeclareLocal(typeof(string));
        argNameLocal.SetLocalSymInfo("typeArgName");
        var firstPropertyLocal = ILGenerator.DeclareLocal(typeof(bool));
        firstPropertyLocal.SetLocalSymInfo("firstProperty");

        ILGenerator.Emit(OpCodes.Newobj, builderCtor);
        ILGenerator.Emit(OpCodes.Stloc, builderLocal);

        var recordClrType = Generator.InstantiateType(ResolveClrType(recordType));
        EmitUnionFriendlyName(
            recordClrType,
            builderLocal,
            typeLocal,
            nameLocal,
            tickIndexLocal,
            argsLocal,
            argsLengthLocal,
            argIndexLocal,
            argTypeLocal,
            argNameLocal,
            appendString,
            appendChar,
            typeFromHandle,
            typeNameGetter,
            typeGenericArgumentsGetter,
            stringIndexOf,
            stringSubstring);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldstr, " { ");
        ILGenerator.Emit(OpCodes.Callvirt, appendString);
        ILGenerator.Emit(OpCodes.Pop);

        ILGenerator.Emit(OpCodes.Ldc_I4_1);
        ILGenerator.Emit(OpCodes.Stloc, firstPropertyLocal);

        foreach (var property in recordProperties)
        {
            if (property.BackingField is not SourceFieldSymbol backingField)
                continue;

            var fieldInfo = backingField.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);

            var skipCommaLabel = ILGenerator.DefineLabel();
            ILGenerator.Emit(OpCodes.Ldloc, firstPropertyLocal);
            ILGenerator.Emit(OpCodes.Brtrue, skipCommaLabel);
            ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
            ILGenerator.Emit(OpCodes.Ldstr, ", ");
            ILGenerator.Emit(OpCodes.Callvirt, appendString);
            ILGenerator.Emit(OpCodes.Pop);
            ILGenerator.MarkLabel(skipCommaLabel);

            ILGenerator.Emit(OpCodes.Ldc_I4_0);
            ILGenerator.Emit(OpCodes.Stloc, firstPropertyLocal);

            ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
            ILGenerator.Emit(OpCodes.Ldstr, property.Name);
            ILGenerator.Emit(OpCodes.Callvirt, appendString);
            ILGenerator.Emit(OpCodes.Pop);

            ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
            ILGenerator.Emit(OpCodes.Ldstr, " = ");
            ILGenerator.Emit(OpCodes.Callvirt, appendString);
            ILGenerator.Emit(OpCodes.Pop);

            EmitAppendFormattedValue(
                builderLocal,
                () =>
                {
                    ILGenerator.Emit(OpCodes.Ldarg_0);
                    ILGenerator.Emit(OpCodes.Ldfld, fieldInfo);
                },
                property.Type,
                appendString,
                appendChar,
                stringReplace,
                objectToString);
        }

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldstr, " }");
        ILGenerator.Emit(OpCodes.Callvirt, appendString);
        ILGenerator.Emit(OpCodes.Pop);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Callvirt, builderToString);
        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitRecordDeconstruct(SourceNamedTypeSymbol recordType)
    {
        var recordProperties = recordType.RecordProperties;
        var parameterCount = Math.Min(recordProperties.Length, MethodSymbol.Parameters.Length);

        for (var i = 0; i < parameterCount; i++)
        {
            if (recordProperties[i].BackingField is not SourceFieldSymbol backingField)
                continue;

            var fieldInfo = backingField.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);
            EmitStoreRecordDeconstructParameter(MethodSymbol.Parameters[i], recordProperties[i].Type, fieldInfo);
        }

        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitRecordFieldComparisons(
        SourceNamedTypeSymbol recordType,
        Action loadLeft,
        Action loadRight,
        ILLabel returnFalseLabel)
    {
        var recordProperties = recordType.RecordProperties;
        foreach (var property in recordProperties)
        {
            if (property.BackingField is not SourceFieldSymbol backingField)
                continue;

            var fieldInfo = backingField.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);
            var propertyClrType = MethodGenerator.ResolveClrType(property.Type);
            var comparerTypeDefinition = typeof(EqualityComparer<>);
            var comparerType = comparerTypeDefinition.MakeGenericType(propertyClrType);
            var comparerGenericArgument = comparerTypeDefinition.GetGenericArguments()[0];
            var defaultGetterDefinition = comparerTypeDefinition.GetProperty("Default")?.GetGetMethod();
            var equalsDefinition = comparerTypeDefinition.GetMethod("Equals", [comparerGenericArgument, comparerGenericArgument]);
            var defaultGetter = GetConstructedMethod(comparerType, defaultGetterDefinition);
            var equalsMethod = GetConstructedMethod(comparerType, equalsDefinition);

            if (defaultGetter is null || equalsMethod is null)
                continue;

            ILGenerator.Emit(OpCodes.Call, defaultGetter);
            loadLeft();
            ILGenerator.Emit(OpCodes.Ldfld, fieldInfo);
            loadRight();
            ILGenerator.Emit(OpCodes.Ldfld, fieldInfo);
            ILGenerator.Emit(OpCodes.Callvirt, equalsMethod);
            ILGenerator.Emit(OpCodes.Brfalse_S, returnFalseLabel);
        }
    }

    private static MethodInfo? GetConstructedMethod(Type constructedType, MethodInfo? methodDefinition)
    {
        if (methodDefinition is null)
            return null;

        // TypeBuilderInstantiation throws NotSupportedException for many reflection APIs
        // (including GetMethod/GetProperty/FindMembers). When we're dealing with a
        // constructed generic type that includes TypeBuilder or TypeBuilderInstantiation
        // arguments, always go through TypeBuilder.GetMethod.
        static bool IsTypeBuilderInstantiation(Type t)
            => string.Equals(
                t.GetType().FullName,
                "System.Reflection.Emit.TypeBuilderInstantiation",
                StringComparison.Ordinal);

        var declaring = methodDefinition.DeclaringType;

        // If the constructed type itself is emitted/instantiated, always use the metadata
        // definition mapping API.
        if (constructedType is TypeBuilder || IsTypeBuilderInstantiation(constructedType))
            return TypeBuilder.GetMethod(constructedType, methodDefinition);

        // For generic constructed types where any argument is emitted/instantiated, use
        // TypeBuilder.GetMethod to avoid reflection paths that are not supported.
        if (constructedType.IsGenericType && declaring is { IsGenericTypeDefinition: true })
        {
            var args = constructedType.GetGenericArguments();
            if (args.Any(a => a is TypeBuilder || IsTypeBuilderInstantiation(a)))
                return TypeBuilder.GetMethod(constructedType, methodDefinition);
        }

        // Regular runtime types: bind by name + parameter types.
        var parameterTypes = methodDefinition
            .GetParameters()
            .Select(p => p.ParameterType)
            .ToArray();

        const BindingFlags flags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;

        return constructedType.GetMethod(methodDefinition.Name, flags, binder: null, types: parameterTypes, modifiers: null);
    }

    private void EmitUnionCasePropertyGetter(SourcePropertySymbol propertySymbol, SourceFieldSymbol backingField)
    {
        var fieldInfo = backingField.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);

        if (propertySymbol.IsStatic)
        {
            ILGenerator.Emit(OpCodes.Ldsfld, fieldInfo);
        }
        else
        {
            ILGenerator.Emit(OpCodes.Ldarg_0);
            ILGenerator.Emit(OpCodes.Ldfld, fieldInfo);
        }

        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitDiscriminatedUnionConversion(SourceDiscriminatedUnionSymbol unionSymbol)
    {
        if (MethodSymbol.Parameters.Length != 1)
        {
            ILGenerator.Emit(OpCodes.Ret);
            return;
        }

        var parameter = MethodSymbol.Parameters[0];
        var parameterType = parameter.Type;
        var caseSymbol = parameterType.TryGetDiscriminatedUnionCase();

        if (caseSymbol is null)
        {
            ILGenerator.Emit(OpCodes.Ret);
            return;
        }

        var unionClrType = Generator.InstantiateType(
            MethodGenerator.TypeGenerator.TypeBuilder
                ?? ResolveClrType(MethodSymbol.ContainingType!));
        var unionLocal = ILGenerator.DeclareLocal(unionClrType);

        ILGenerator.Emit(OpCodes.Ldloca, unionLocal);
        ILGenerator.Emit(OpCodes.Initobj, unionClrType);

        var discriminatorField = ((SourceFieldSymbol)unionSymbol.DiscriminatorField)
            .GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);

        ILGenerator.Emit(OpCodes.Ldloca, unionLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4, caseSymbol.Ordinal);
        ILGenerator.Emit(OpCodes.Stfld, discriminatorField);

        var payloadFieldSymbol = (SourceFieldSymbol)DiscriminatedUnionFieldUtilities.GetRequiredPayloadField(
            unionSymbol,
            caseSymbol);
        var payloadField = payloadFieldSymbol.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);

        ILGenerator.Emit(OpCodes.Ldloca, unionLocal);
        ILGenerator.Emit(OpCodes.Ldarg_0);

        ILGenerator.Emit(OpCodes.Stfld, payloadField);
        ILGenerator.Emit(OpCodes.Ldloc, unionLocal);
        ILGenerator.Emit(OpCodes.Ret);
    }

    private static SourceNamedTypeSymbol? TryGetRecordTypeDefinition(INamedTypeSymbol? typeSymbol)
    {
        if (typeSymbol is null)
            return null;

        if (typeSymbol is SourceNamedTypeSymbol { IsRecord: true } source)
            return source;

        if (typeSymbol is ConstructedNamedTypeSymbol constructed &&
            constructed.OriginalDefinition is SourceNamedTypeSymbol { IsRecord: true } sourceDefinition)
            return sourceDefinition;

        return null;
    }

    private static SourceDiscriminatedUnionSymbol? TryGetSourceDiscriminatedUnionDefinition(INamedTypeSymbol? typeSymbol)
    {
        switch (typeSymbol)
        {
            case SourceDiscriminatedUnionSymbol sourceUnion:
                return sourceUnion;
            case ConstructedNamedTypeSymbol { OriginalDefinition: SourceDiscriminatedUnionSymbol sourceUnion }:
                return sourceUnion;
            default:
                return null;
        }
    }

    private void EmitDiscriminatedUnionTryGetMethod(
        SourceDiscriminatedUnionSymbol unionSymbol,
        IDiscriminatedUnionCaseSymbol caseSymbol,
        IParameterSymbol targetParameter)
    {
        var discriminatorField = ((SourceFieldSymbol)unionSymbol.DiscriminatorField)
            .GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);
        var payloadFieldSymbol = (SourceFieldSymbol)DiscriminatedUnionFieldUtilities.GetRequiredPayloadField(
            unionSymbol,
            caseSymbol);
        var payloadField = payloadFieldSymbol.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);
        var failLabel = ILGenerator.DefineLabel();

        ILGenerator.Emit(OpCodes.Ldarg_0);
        ILGenerator.Emit(OpCodes.Ldfld, discriminatorField);
        ILGenerator.Emit(OpCodes.Ldc_I4, caseSymbol.Ordinal);
        ILGenerator.Emit(OpCodes.Bne_Un_S, failLabel);

        EmitStoreDiscriminatedUnionPayload(targetParameter, payloadFieldSymbol.Type, payloadField);

        ILGenerator.Emit(OpCodes.Ldc_I4_1);
        ILGenerator.Emit(OpCodes.Ret);

        ILGenerator.MarkLabel(failLabel);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitStoreDiscriminatedUnionPayload(
        IParameterSymbol parameter,
        ITypeSymbol payloadType,
        FieldInfo payloadField)
    {
        var parameterPosition = GetParameterPosition(parameter);
        ILGenerator.Emit(OpCodes.Ldarg, parameterPosition);
        ILGenerator.Emit(OpCodes.Ldarg_0);
        ILGenerator.Emit(OpCodes.Ldfld, payloadField);

        var parameterType = parameter.Type;

        if (parameterType.IsValueType)
        {
            var parameterClrType = ResolveUnionCaseClrType(parameterType);

            if (!payloadType.IsValueType)
                ILGenerator.Emit(OpCodes.Unbox_Any, parameterClrType);

            ILGenerator.Emit(OpCodes.Stobj, parameterClrType);
            return;
        }

        if (payloadType.IsValueType)
        {
            var payloadClrType = ResolveClrType(payloadType);
            ILGenerator.Emit(OpCodes.Box, payloadClrType);
        }

        if (!SymbolEqualityComparer.Default.Equals(parameterType, payloadType))
        {
            var parameterClrType = ResolveClrType(parameterType);
            ILGenerator.Emit(OpCodes.Castclass, parameterClrType);
        }

        ILGenerator.Emit(OpCodes.Stind_Ref);
    }

    private void EmitStoreRecordDeconstructParameter(
        IParameterSymbol parameter,
        ITypeSymbol payloadType,
        FieldInfo payloadField)
    {
        var parameterPosition = GetParameterPosition(parameter);
        ILGenerator.Emit(OpCodes.Ldarg, parameterPosition);
        ILGenerator.Emit(OpCodes.Ldarg_0);
        ILGenerator.Emit(OpCodes.Ldfld, payloadField);

        var parameterType = parameter.Type;

        if (parameterType.IsValueType)
        {
            var parameterClrType = ResolveClrType(parameterType);

            if (!payloadType.IsValueType)
                ILGenerator.Emit(OpCodes.Unbox_Any, parameterClrType);

            ILGenerator.Emit(OpCodes.Stobj, parameterClrType);
            return;
        }

        if (payloadType.IsValueType)
        {
            var payloadClrType = ResolveClrType(payloadType);
            ILGenerator.Emit(OpCodes.Box, payloadClrType);
        }

        if (!SymbolEqualityComparer.Default.Equals(parameterType, payloadType))
        {
            var parameterClrType = ResolveClrType(parameterType);
            ILGenerator.Emit(OpCodes.Castclass, parameterClrType);
        }

        ILGenerator.Emit(OpCodes.Stind_Ref);
    }

    private void EmitDiscriminatedUnionToString(SourceDiscriminatedUnionSymbol unionSymbol)
    {
        var discriminatorField = ((SourceFieldSymbol)unionSymbol.DiscriminatorField)
            .GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);
        var objectToString = typeof(object).GetMethod(nameof(ToString), Type.EmptyTypes)!;
        var cases = unionSymbol.Cases;
        var endLabel = ILGenerator.DefineLabel();

        foreach (var caseSymbol in cases)
        {
            var nextLabel = ILGenerator.DefineLabel();

            ILGenerator.Emit(OpCodes.Ldarg_0);
            ILGenerator.Emit(OpCodes.Ldfld, discriminatorField);
            ILGenerator.Emit(OpCodes.Ldc_I4, caseSymbol.Ordinal);
            ILGenerator.Emit(OpCodes.Bne_Un_S, nextLabel);

            var payloadFieldSymbol = (SourceFieldSymbol)DiscriminatedUnionFieldUtilities.GetRequiredPayloadField(
                unionSymbol,
                caseSymbol);
            var payloadField = payloadFieldSymbol.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);
            var payloadType = payloadFieldSymbol.Type;

            ILGenerator.Emit(OpCodes.Ldarg_0);

            if (payloadType.IsValueType)
            {
                ILGenerator.Emit(OpCodes.Ldflda, payloadField);
                var payloadClrType = ResolveUnionCaseClrType(payloadType);
                ILGenerator.Emit(OpCodes.Constrained, payloadClrType);
            }
            else
            {
                ILGenerator.Emit(OpCodes.Ldfld, payloadField);
            }

            ILGenerator.Emit(OpCodes.Callvirt, objectToString);
            ILGenerator.Emit(OpCodes.Br, endLabel);

            ILGenerator.MarkLabel(nextLabel);
        }

        ILGenerator.Emit(OpCodes.Ldstr, "<Uninitialized>");
        ILGenerator.MarkLabel(endLabel);
        ILGenerator.Emit(OpCodes.Ret);
    }


    private void EmitUnionCaseToString(SourceDiscriminatedUnionCaseTypeSymbol caseSymbol)
    {
        var builderType = typeof(StringBuilder);
        var builderCtor = builderType.GetConstructor(Type.EmptyTypes)!;
        var appendString = builderType.GetMethod(nameof(StringBuilder.Append), new[] { typeof(string) })!;
        var appendChar = builderType.GetMethod(nameof(StringBuilder.Append), new[] { typeof(char) })!;
        var builderToString = builderType.GetMethod(nameof(StringBuilder.ToString), Type.EmptyTypes)!;
        var stringIndexOf = typeof(string).GetMethod(nameof(string.IndexOf), new[] { typeof(char) })!;
        var stringSubstring = typeof(string).GetMethod(nameof(string.Substring), new[] { typeof(int), typeof(int) })!;
        var stringReplace = typeof(string).GetMethod(nameof(string.Replace), new[] { typeof(string), typeof(string) })!;
        var objectToString = typeof(object).GetMethod(nameof(ToString), Type.EmptyTypes)!;
        var typeType = typeof(Type);
        var typeFromHandle = typeType.GetMethod(nameof(Type.GetTypeFromHandle), new[] { typeof(RuntimeTypeHandle) })!;
        var typeNameGetter = typeType.GetProperty(nameof(Type.Name))!.GetMethod!;
        var typeGenericArgumentsGetter = typeType.GetProperty(nameof(Type.GenericTypeArguments))!.GetMethod!;

        var parameterInfos = CollectUnionCaseParameters(caseSymbol);

        var builderLocal = ILGenerator.DeclareLocal(builderType);
        builderLocal.SetLocalSymInfo("builder");
        var unionTypeLocal = ILGenerator.DeclareLocal(typeType);
        unionTypeLocal.SetLocalSymInfo("unionType");
        var nameLocal = ILGenerator.DeclareLocal(typeof(string));
        nameLocal.SetLocalSymInfo("name");
        var tickIndexLocal = ILGenerator.DeclareLocal(typeof(int));
        tickIndexLocal.SetLocalSymInfo("tickIndex");
        var argsLocal = ILGenerator.DeclareLocal(typeof(Type[]));
        argsLocal.SetLocalSymInfo("typeArgs");
        var argsLengthLocal = ILGenerator.DeclareLocal(typeof(int));
        argsLengthLocal.SetLocalSymInfo("typeArgLength");
        var argIndexLocal = ILGenerator.DeclareLocal(typeof(int));
        argIndexLocal.SetLocalSymInfo("typeArgIndex");
        var argTypeLocal = ILGenerator.DeclareLocal(typeType);
        argTypeLocal.SetLocalSymInfo("typeArg");
        var argNameLocal = ILGenerator.DeclareLocal(typeof(string));
        argNameLocal.SetLocalSymInfo("typeArgName");
        var includeParameterNames = parameterInfos.Count > 1;
        IILocal? firstParameterLocal = null;

        if (includeParameterNames)
        {
            firstParameterLocal = ILGenerator.DeclareLocal(typeof(bool));
            firstParameterLocal.SetLocalSymInfo("firstParameter");
        }

        ILGenerator.Emit(OpCodes.Newobj, builderCtor);
        ILGenerator.Emit(OpCodes.Stloc, builderLocal);

        var unionClrType = Generator.InstantiateType(ResolveClrType(caseSymbol.Union));

        EmitUnionFriendlyName(
            unionClrType,
            builderLocal,
            unionTypeLocal,
            nameLocal,
            tickIndexLocal,
            argsLocal,
            argsLengthLocal,
            argIndexLocal,
            argTypeLocal,
            argNameLocal,
            appendString,
            appendChar,
            typeFromHandle,
            typeNameGetter,
            typeGenericArgumentsGetter,
            stringIndexOf,
            stringSubstring);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'.');
        ILGenerator.Emit(OpCodes.Callvirt, appendChar);
        ILGenerator.Emit(OpCodes.Pop);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldstr, caseSymbol.Name);
        ILGenerator.Emit(OpCodes.Callvirt, appendString);
        ILGenerator.Emit(OpCodes.Pop);

        if (parameterInfos.Count > 0)
        {
            ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
            ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'(');
            ILGenerator.Emit(OpCodes.Callvirt, appendChar);
            ILGenerator.Emit(OpCodes.Pop);

            if (includeParameterNames)
            {
                ILGenerator.Emit(OpCodes.Ldc_I4_1);
                ILGenerator.Emit(OpCodes.Stloc, firstParameterLocal!);
            }

            foreach (var parameter in parameterInfos)
            {
                if (includeParameterNames)
                {
                    var skipParameterComma = ILGenerator.DefineLabel();
                    ILGenerator.Emit(OpCodes.Ldloc, firstParameterLocal!);
                    ILGenerator.Emit(OpCodes.Brtrue_S, skipParameterComma);
                    ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
                    ILGenerator.Emit(OpCodes.Ldstr, ", ");
                    ILGenerator.Emit(OpCodes.Callvirt, appendString);
                    ILGenerator.Emit(OpCodes.Pop);
                    ILGenerator.MarkLabel(skipParameterComma);

                    ILGenerator.Emit(OpCodes.Ldc_I4_0);
                    ILGenerator.Emit(OpCodes.Stloc, firstParameterLocal!);

                    ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
                    ILGenerator.Emit(OpCodes.Ldstr, parameter.Name);
                    ILGenerator.Emit(OpCodes.Callvirt, appendString);
                    ILGenerator.Emit(OpCodes.Pop);

                    ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
                    ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'=');
                    ILGenerator.Emit(OpCodes.Callvirt, appendChar);
                    ILGenerator.Emit(OpCodes.Pop);
                }

                EmitAppendFormattedValue(
                    builderLocal,
                    () =>
                    {
                        ILGenerator.Emit(OpCodes.Ldarg_0);
                        ILGenerator.Emit(OpCodes.Ldfld, parameter.Field);
                    },
                    parameter.Type,
                    appendString,
                    appendChar,
                    stringReplace,
                    objectToString);
            }

            ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
            ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)')');
            ILGenerator.Emit(OpCodes.Callvirt, appendChar);
            ILGenerator.Emit(OpCodes.Pop);
        }

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Callvirt, builderToString);
        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitUnionCaseDeconstruct(SourceDiscriminatedUnionCaseTypeSymbol caseSymbol)
    {
        var parameterInfos = CollectUnionCaseParameters(caseSymbol);
        var parameterCount = Math.Min(parameterInfos.Count, MethodSymbol.Parameters.Length);

        for (var i = 0; i < parameterCount; i++)
        {
            var parameter = MethodSymbol.Parameters[i];
            var info = parameterInfos[i];
            EmitStoreRecordDeconstructParameter(parameter, info.Type, info.Field);
        }

        ILGenerator.Emit(OpCodes.Ret);
    }

    private List<(string Name, ITypeSymbol Type, FieldInfo Field)> CollectUnionCaseParameters(SourceDiscriminatedUnionCaseTypeSymbol caseSymbol)
    {
        var parameterInfos = new List<(string, ITypeSymbol, FieldInfo)>();

        foreach (var parameter in caseSymbol.ConstructorParameters)
        {
            if (parameter.RefKind != RefKind.None || parameter.Type is null)
                continue;

            var propertyName = GetUnionCasePropertyName(parameter.Name);

            if (caseSymbol
                    .GetMembers(propertyName)
                    .OfType<IPropertySymbol>()
                    .FirstOrDefault() is not SourcePropertySymbol property)
            {
                continue;
            }

            if (property.BackingField is not SourceFieldSymbol backingField)
                continue;

            var fieldInfo = backingField.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);
            parameterInfos.Add((property.Name, parameter.Type, fieldInfo));
        }

        return parameterInfos;
    }

    private static string GetUnionCasePropertyName(string parameterName)
    {
        if (string.IsNullOrEmpty(parameterName) || char.IsUpper(parameterName[0]))
            return parameterName;

        Span<char> buffer = stackalloc char[parameterName.Length];
        parameterName.AsSpan().CopyTo(buffer);
        buffer[0] = char.ToUpperInvariant(buffer[0]);
        return new string(buffer);
    }

    private void EmitUnionFriendlyName(
        Type unionClrType,
        IILocal builderLocal,
        IILocal unionTypeLocal,
        IILocal nameLocal,
        IILocal tickIndexLocal,
        IILocal argsLocal,
        IILocal argsLengthLocal,
        IILocal argIndexLocal,
        IILocal argTypeLocal,
        IILocal argNameLocal,
        MethodInfo appendString,
        MethodInfo appendChar,
        MethodInfo typeFromHandle,
        MethodInfo typeNameGetter,
        MethodInfo typeGenericArgumentsGetter,
        MethodInfo stringIndexOf,
        MethodInfo stringSubstring)
    {
        ILGenerator.Emit(OpCodes.Ldtoken, unionClrType);
        ILGenerator.Emit(OpCodes.Call, typeFromHandle);
        ILGenerator.Emit(OpCodes.Stloc, unionTypeLocal);

        ILGenerator.Emit(OpCodes.Ldloc, unionTypeLocal);
        ILGenerator.Emit(OpCodes.Callvirt, typeNameGetter);
        ILGenerator.Emit(OpCodes.Stloc, nameLocal);

        ILGenerator.Emit(OpCodes.Ldloc, nameLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'`');
        ILGenerator.Emit(OpCodes.Callvirt, stringIndexOf);
        ILGenerator.Emit(OpCodes.Stloc, tickIndexLocal);

        var noTickLabel = ILGenerator.DefineLabel();
        ILGenerator.Emit(OpCodes.Ldloc, tickIndexLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Blt, noTickLabel);
        ILGenerator.Emit(OpCodes.Ldloc, nameLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Ldloc, tickIndexLocal);
        ILGenerator.Emit(OpCodes.Callvirt, stringSubstring);
        ILGenerator.Emit(OpCodes.Stloc, nameLocal);
        ILGenerator.MarkLabel(noTickLabel);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldloc, nameLocal);
        ILGenerator.Emit(OpCodes.Callvirt, appendString);
        ILGenerator.Emit(OpCodes.Pop);

        ILGenerator.Emit(OpCodes.Ldloc, unionTypeLocal);
        ILGenerator.Emit(OpCodes.Callvirt, typeGenericArgumentsGetter);
        ILGenerator.Emit(OpCodes.Stloc, argsLocal);
        ILGenerator.Emit(OpCodes.Ldloc, argsLocal);
        ILGenerator.Emit(OpCodes.Ldlen);
        ILGenerator.Emit(OpCodes.Conv_I4);
        ILGenerator.Emit(OpCodes.Stloc, argsLengthLocal);

        var skipGenericsLabel = ILGenerator.DefineLabel();
        ILGenerator.Emit(OpCodes.Ldloc, argsLengthLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Ble, skipGenericsLabel);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'<');
        ILGenerator.Emit(OpCodes.Callvirt, appendChar);
        ILGenerator.Emit(OpCodes.Pop);

        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Stloc, argIndexLocal);

        var typeLoopCheck = ILGenerator.DefineLabel();
        var typeLoopBody = ILGenerator.DefineLabel();
        ILGenerator.Emit(OpCodes.Br, typeLoopCheck);
        ILGenerator.MarkLabel(typeLoopBody);

        var skipCommaLabel = ILGenerator.DefineLabel();
        ILGenerator.Emit(OpCodes.Ldloc, argIndexLocal);
        ILGenerator.Emit(OpCodes.Brfalse, skipCommaLabel);
        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldstr, ", ");
        ILGenerator.Emit(OpCodes.Callvirt, appendString);
        ILGenerator.Emit(OpCodes.Pop);
        ILGenerator.MarkLabel(skipCommaLabel);

        ILGenerator.Emit(OpCodes.Ldloc, argsLocal);
        ILGenerator.Emit(OpCodes.Ldloc, argIndexLocal);
        ILGenerator.Emit(OpCodes.Ldelem_Ref);
        ILGenerator.Emit(OpCodes.Stloc, argTypeLocal);

        ILGenerator.Emit(OpCodes.Ldloc, argTypeLocal);
        ILGenerator.Emit(OpCodes.Callvirt, typeNameGetter);
        ILGenerator.Emit(OpCodes.Stloc, argNameLocal);

        ILGenerator.Emit(OpCodes.Ldloc, argNameLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'`');
        ILGenerator.Emit(OpCodes.Callvirt, stringIndexOf);
        ILGenerator.Emit(OpCodes.Stloc, tickIndexLocal);

        var argNoTickLabel = ILGenerator.DefineLabel();
        ILGenerator.Emit(OpCodes.Ldloc, tickIndexLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Blt, argNoTickLabel);
        ILGenerator.Emit(OpCodes.Ldloc, argNameLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Ldloc, tickIndexLocal);
        ILGenerator.Emit(OpCodes.Callvirt, stringSubstring);
        ILGenerator.Emit(OpCodes.Stloc, argNameLocal);
        ILGenerator.MarkLabel(argNoTickLabel);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldloc, argNameLocal);
        ILGenerator.Emit(OpCodes.Callvirt, appendString);
        ILGenerator.Emit(OpCodes.Pop);

        ILGenerator.Emit(OpCodes.Ldloc, argIndexLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_1);
        ILGenerator.Emit(OpCodes.Add);
        ILGenerator.Emit(OpCodes.Stloc, argIndexLocal);

        ILGenerator.MarkLabel(typeLoopCheck);
        ILGenerator.Emit(OpCodes.Ldloc, argIndexLocal);
        ILGenerator.Emit(OpCodes.Ldloc, argsLengthLocal);
        ILGenerator.Emit(OpCodes.Blt, typeLoopBody);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'>');
        ILGenerator.Emit(OpCodes.Callvirt, appendChar);
        ILGenerator.Emit(OpCodes.Pop);
        ILGenerator.MarkLabel(skipGenericsLabel);
    }

    private void EmitAppendFormattedValue(
        IILocal builderLocal,
        Action emitLoadValue,
        ITypeSymbol valueType,
        MethodInfo appendString,
        MethodInfo appendChar,
        MethodInfo stringReplace,
        MethodInfo objectToString)
    {
        var objectGetType = typeof(object).GetMethod(nameof(GetType))!;
        var typeFromHandle = typeof(Type).GetMethod(nameof(Type.GetTypeFromHandle), new[] { typeof(RuntimeTypeHandle) })!;

        if (valueType.SpecialType == SpecialType.System_String)
        {
            var stringLocal = ILGenerator.DeclareLocal(typeof(string));
            stringLocal.SetLocalSymInfo("stringValue");

            emitLoadValue();
            ILGenerator.Emit(OpCodes.Stloc, stringLocal);

            var nonNullLabel = ILGenerator.DefineLabel();
            var endLabel = ILGenerator.DefineLabel();

            ILGenerator.Emit(OpCodes.Ldloc, stringLocal);
            ILGenerator.Emit(OpCodes.Brtrue, nonNullLabel);

            ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
            ILGenerator.Emit(OpCodes.Ldstr, "null");
            ILGenerator.Emit(OpCodes.Callvirt, appendString);
            ILGenerator.Emit(OpCodes.Pop);
            ILGenerator.Emit(OpCodes.Br, endLabel);

            ILGenerator.MarkLabel(nonNullLabel);

            ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
            ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'\"');
            ILGenerator.Emit(OpCodes.Callvirt, appendChar);
            ILGenerator.Emit(OpCodes.Pop);

            ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
            ILGenerator.Emit(OpCodes.Ldloc, stringLocal);
            ILGenerator.Emit(OpCodes.Ldstr, "\"");
            ILGenerator.Emit(OpCodes.Ldstr, "\\\"");
            ILGenerator.Emit(OpCodes.Callvirt, stringReplace);
            ILGenerator.Emit(OpCodes.Callvirt, appendString);
            ILGenerator.Emit(OpCodes.Pop);

            ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
            ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'\"');
            ILGenerator.Emit(OpCodes.Callvirt, appendChar);
            ILGenerator.Emit(OpCodes.Pop);

            ILGenerator.MarkLabel(endLabel);
            return;
        }

        if (valueType.SpecialType == SpecialType.System_Char)
        {
            var charLocal = ILGenerator.DeclareLocal(typeof(char));
            charLocal.SetLocalSymInfo("charValue");

            emitLoadValue();
            ILGenerator.Emit(OpCodes.Stloc, charLocal);

            ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
            ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'\'');
            ILGenerator.Emit(OpCodes.Callvirt, appendChar);
            ILGenerator.Emit(OpCodes.Pop);

            EmitAppendEscapedChar(builderLocal, charLocal, appendString, appendChar);

            ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
            ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'\'');
            ILGenerator.Emit(OpCodes.Callvirt, appendChar);
            ILGenerator.Emit(OpCodes.Pop);

            return;
        }

        var valueClrType = ResolveClrType(valueType);
        var valueLocal = ILGenerator.DeclareLocal(valueClrType);
        valueLocal.SetLocalSymInfo("value");

        emitLoadValue();
        ILGenerator.Emit(OpCodes.Stloc, valueLocal);

        var boxedValueLocal = ILGenerator.DeclareLocal(typeof(object));
        boxedValueLocal.SetLocalSymInfo("valueObject");

        ILGenerator.Emit(OpCodes.Ldloc, valueLocal);

        // Box value-type and type-parameter payloads so they can be null-checked
        // and formatted safely even when instantiated with struct arguments.
        if (valueType.IsValueType || valueType is ITypeParameterSymbol)
            ILGenerator.Emit(OpCodes.Box, valueClrType);
        ILGenerator.Emit(OpCodes.Stloc, boxedValueLocal);

        var boxedNotNullLabel = ILGenerator.DefineLabel();
        var boxedDoneLabel = ILGenerator.DefineLabel();
        var boxedStringLabel = ILGenerator.DefineLabel();
        var boxedCharLabel = ILGenerator.DefineLabel();

        ILGenerator.Emit(OpCodes.Ldloc, boxedValueLocal);
        ILGenerator.Emit(OpCodes.Brtrue, boxedNotNullLabel);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldstr, "null");
        ILGenerator.Emit(OpCodes.Callvirt, appendString);
        ILGenerator.Emit(OpCodes.Pop);
        ILGenerator.Emit(OpCodes.Br, boxedDoneLabel);

        ILGenerator.MarkLabel(boxedNotNullLabel);

        // Handle runtime string payloads for generic parameters
        ILGenerator.Emit(OpCodes.Ldloc, boxedValueLocal);
        ILGenerator.Emit(OpCodes.Callvirt, objectGetType);
        ILGenerator.Emit(OpCodes.Ldtoken, typeof(string));
        ILGenerator.Emit(OpCodes.Call, typeFromHandle);
        ILGenerator.Emit(OpCodes.Ceq);
        ILGenerator.Emit(OpCodes.Brtrue, boxedStringLabel);

        // Handle runtime char payloads for generic parameters
        ILGenerator.Emit(OpCodes.Ldloc, boxedValueLocal);
        ILGenerator.Emit(OpCodes.Callvirt, objectGetType);
        ILGenerator.Emit(OpCodes.Ldtoken, typeof(char));
        ILGenerator.Emit(OpCodes.Call, typeFromHandle);
        ILGenerator.Emit(OpCodes.Ceq);
        ILGenerator.Emit(OpCodes.Brtrue, boxedCharLabel);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldloc, boxedValueLocal);
        ILGenerator.Emit(OpCodes.Callvirt, objectToString);
        ILGenerator.Emit(OpCodes.Dup);

        var boxedHasTextLabel = ILGenerator.DefineLabel();
        ILGenerator.Emit(OpCodes.Brtrue, boxedHasTextLabel);
        ILGenerator.Emit(OpCodes.Pop);
        ILGenerator.Emit(OpCodes.Ldstr, "null");
        ILGenerator.MarkLabel(boxedHasTextLabel);
        ILGenerator.Emit(OpCodes.Callvirt, appendString);
        ILGenerator.Emit(OpCodes.Pop);

        ILGenerator.Emit(OpCodes.Br, boxedDoneLabel);

        ILGenerator.MarkLabel(boxedStringLabel);
        var boxedStringLocal = ILGenerator.DeclareLocal(typeof(string));
        boxedStringLocal.SetLocalSymInfo("boxedString");

        ILGenerator.Emit(OpCodes.Ldloc, boxedValueLocal);
        ILGenerator.Emit(OpCodes.Castclass, typeof(string));
        ILGenerator.Emit(OpCodes.Stloc, boxedStringLocal);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'\"');
        ILGenerator.Emit(OpCodes.Callvirt, appendChar);
        ILGenerator.Emit(OpCodes.Pop);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldloc, boxedStringLocal);
        ILGenerator.Emit(OpCodes.Ldstr, "\"");
        ILGenerator.Emit(OpCodes.Ldstr, "\\\"");
        ILGenerator.Emit(OpCodes.Callvirt, stringReplace);
        ILGenerator.Emit(OpCodes.Callvirt, appendString);
        ILGenerator.Emit(OpCodes.Pop);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'\"');
        ILGenerator.Emit(OpCodes.Callvirt, appendChar);
        ILGenerator.Emit(OpCodes.Pop);
        ILGenerator.Emit(OpCodes.Br, boxedDoneLabel);

        ILGenerator.MarkLabel(boxedCharLabel);
        var boxedCharLocal = ILGenerator.DeclareLocal(typeof(char));
        boxedCharLocal.SetLocalSymInfo("boxedChar");

        ILGenerator.Emit(OpCodes.Ldloc, boxedValueLocal);
        ILGenerator.Emit(OpCodes.Unbox_Any, typeof(char));
        ILGenerator.Emit(OpCodes.Stloc, boxedCharLocal);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'\'');
        ILGenerator.Emit(OpCodes.Callvirt, appendChar);
        ILGenerator.Emit(OpCodes.Pop);

        EmitAppendEscapedChar(builderLocal, boxedCharLocal, appendString, appendChar);

        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'\'');
        ILGenerator.Emit(OpCodes.Callvirt, appendChar);
        ILGenerator.Emit(OpCodes.Pop);
        ILGenerator.Emit(OpCodes.Br, boxedDoneLabel);

        ILGenerator.MarkLabel(boxedDoneLabel);
    }

    private void EmitAppendEscapedChar(
        IILocal builderLocal,
        IILocal charLocal,
        MethodInfo appendString,
        MethodInfo appendChar)
    {
        var escapeBackslashLabel = ILGenerator.DefineLabel();
        var escapeQuoteLabel = ILGenerator.DefineLabel();
        var writeCharLabel = ILGenerator.DefineLabel();

        ILGenerator.Emit(OpCodes.Ldloc, charLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'\\');
        ILGenerator.Emit(OpCodes.Ceq);
        ILGenerator.Emit(OpCodes.Brtrue, escapeBackslashLabel);

        ILGenerator.Emit(OpCodes.Ldloc, charLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_S, (int)'\'');
        ILGenerator.Emit(OpCodes.Ceq);
        ILGenerator.Emit(OpCodes.Brtrue, escapeQuoteLabel);

        ILGenerator.Emit(OpCodes.Br, writeCharLabel);

        ILGenerator.MarkLabel(escapeBackslashLabel);
        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldstr, "\\\\");
        ILGenerator.Emit(OpCodes.Callvirt, appendString);
        ILGenerator.Emit(OpCodes.Pop);
        ILGenerator.Emit(OpCodes.Br, writeCharLabel);

        ILGenerator.MarkLabel(escapeQuoteLabel);
        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldstr, "\\'");
        ILGenerator.Emit(OpCodes.Callvirt, appendString);
        ILGenerator.Emit(OpCodes.Pop);
        ILGenerator.Emit(OpCodes.Br, writeCharLabel);

        ILGenerator.MarkLabel(writeCharLabel);
        ILGenerator.Emit(OpCodes.Ldloc, builderLocal);
        ILGenerator.Emit(OpCodes.Ldloc, charLocal);
        ILGenerator.Emit(OpCodes.Callvirt, appendChar);
        ILGenerator.Emit(OpCodes.Pop);
    }

    private void EmitUnionCaseConstructor()
    {
        if (!MethodSymbol.IsConstructor)
        {
            ILGenerator.Emit(OpCodes.Ret);
            return;
        }

        EmitFieldInitializers(MethodSymbol.IsStatic);
        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitExpressionBody(BoundExpression expression, bool includeReturn = true)
    {
        var returnType = MethodSymbol.ReturnType;

        if (returnType is null || returnType.SpecialType == SpecialType.System_Void)
        {
            EmitExpressionStatement(expression);

            if (includeReturn)
                ILGenerator.Emit(OpCodes.Ret);

            return;
        }

        new ExpressionGenerator(baseGenerator, expression).Emit();

        if (returnType.SpecialType == SpecialType.System_Unit)
        {
            ILGenerator.Emit(OpCodes.Pop);
        }
        else if (expression.Type is { IsValueType: true } expressionType &&
                 (returnType.SpecialType is SpecialType.System_Object || returnType is ITypeUnionSymbol))
        {
            ILGenerator.Emit(OpCodes.Box, ResolveClrType(expressionType));
        }

        if (includeReturn)
            ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitExpressionStatement(BoundExpression expression)
    {
        var statement = new BoundExpressionStatement(expression);
        new StatementGenerator(baseGenerator, statement).Emit();
    }

    private void EmitFunction(FunctionStatementSyntax localFunctionStmt)
    {
        var methodSymbol = GetDeclaredSymbol<IMethodSymbol>(localFunctionStmt);
        if (methodSymbol is null)
            return;

        if (MethodGenerator.TypeGenerator.HasMethodGenerator(methodSymbol))
            return;

        var methodGenerator = new MethodGenerator(MethodGenerator.TypeGenerator, methodSymbol, MethodGenerator.ILBuilderFactory);
        MethodGenerator.TypeGenerator.Add(methodSymbol, methodGenerator);
        methodGenerator.DefineMethodBuilder();
    }

    private void DeclareLocals(BoundBlockStatement block)
    {
        DeclareLocals(scope, block);
    }

    internal void DeclareLocals(Scope targetScope, IEnumerable<BoundStatement> statements)
    {
        var block = statements as BoundBlockStatement ?? new BoundBlockStatement(statements);
        DeclareLocals(targetScope, block);
    }

    private void DeclareLocals(Scope targetScope, BoundBlockStatement block)
    {
        var collector = new LocalCollector(MethodSymbol);
        collector.Visit(block);

        foreach (var localSymbol in collector.Locals)
        {
            // Skip locals without a type. This can occur when the initializer
            // contains an early return, making the declaration unreachable.
            if (localSymbol.Type is null)
                continue;

            // Locals are declared once at the method scope. When the block is emitted
            // we may revisit the same locals to populate nested scopes with the
            // existing builders. Ensure we reuse the builder instead of declaring a
            // duplicate slot.
            var existingBuilder = targetScope.GetLocal(localSymbol);
            if (existingBuilder is not null)
            {
                targetScope.AddLocal(localSymbol, existingBuilder);
                continue;
            }

            var clrType = ResolveClrType(localSymbol.Type);
            if (clrType is null)
            {
                var typeDisplay = localSymbol.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
                throw new InvalidOperationException($"Failed to resolve CLR type for local '{localSymbol.Name}' of type '{typeDisplay}' in method '{MethodSymbol}'.");
            }

            var builder = ILGenerator.DeclareLocal(clrType);
            builder.SetLocalSymInfo(localSymbol.Name);
            targetScope.AddLocal(localSymbol, builder);
        }
    }

    private void EmitMethodBlock(BoundBlockStatement block, bool includeImplicitReturn = true)
    {
        EmitBlock(block, treatAsMethodBody: true, includeImplicitReturn);
    }

    private void EmitBoundBlock(BoundBlockStatement block)
    {
        EmitBlock(block, treatAsMethodBody: false, includeImplicitReturn: false);
    }

    private void EmitBlock(BoundBlockStatement block, bool treatAsMethodBody, bool includeImplicitReturn)
    {
        block = Lowerer.LowerBlock(MethodSymbol, block);
        var statements = block.Statements as IReadOnlyList<BoundStatement> ?? block.Statements.ToArray();
        var blockScope = new Scope(scope, block.LocalsToDispose);

        // Locals synthesized during lowering (e.g., iterator state machines) won't
        // be present in the original bound body we used for the initial declaration
        // pass. Ensure we register builders for any newly introduced locals so
        // downstream emitters can load and store them.
        DeclareLocals(blockScope, block);

        for (var i = 0; i < statements.Count; i++)
        {
            var statement = statements[i];

            // If this is the last statement in the block and the method expects a
            // value, treat a bare expression statement as an implicit return. This
            // allows functions to omit an explicit `return` for the final
            // expression, while still emitting any required boxing.
            if (treatAsMethodBody && includeImplicitReturn &&
                i == statements.Count - 1 &&
                MethodSymbol.ReturnType.SpecialType is not SpecialType.System_Void and not SpecialType.System_Unit &&
                statement is BoundExpressionStatement exprStmt)
            {
                var returnStatement = new BoundReturnStatement(exprStmt.Expression);
                new StatementGenerator(blockScope, returnStatement).Emit();
                return;
            }

            new StatementGenerator(blockScope, statement).Emit();
        }

        blockScope.EmitDispose(block.LocalsToDispose);

        if (_returnLabel is ILLabel exitLabel)
        {
            ILGenerator.MarkLabel(exitLabel);

            if (treatAsMethodBody && includeImplicitReturn)
            {
                if (TryGetReturnValueLocal(out var returnValueLocal) && returnValueLocal is not null)
                    ILGenerator.Emit(OpCodes.Ldloc, returnValueLocal);

                ILGenerator.Emit(OpCodes.Ret);
                return;
            }
        }

        if (!treatAsMethodBody || !includeImplicitReturn)
            return;

        var endsWithTerminator = statements.Count > 0 &&
            statements[^1] is BoundReturnStatement or BoundThrowStatement;

        if (!endsWithTerminator && ShouldEmitImplicitReturn())
        {
            ILGenerator.Emit(OpCodes.Nop);
            ILGenerator.Emit(OpCodes.Ret);
        }
    }

    private sealed class LocalCollector : Raven.CodeAnalysis.BoundTreeWalker
    {
        private readonly ISymbol _containingSymbol;

        public LocalCollector(ISymbol containingSymbol)
        {
            _containingSymbol = containingSymbol;
        }

        public List<ILocalSymbol> Locals { get; } = new();

        public override void VisitLocalDeclarationStatement(BoundLocalDeclarationStatement node)
        {
            foreach (var d in node.Declarators)
                Locals.Add(d.Local);

            base.VisitLocalDeclarationStatement(node);
        }

        public override void VisitAssignmentStatement(BoundAssignmentStatement node)
        {
            if (node.Expression is BoundPatternAssignmentExpression patternAssignment &&
                patternAssignment.Pattern is { } pattern)
            {
                foreach (var designator in pattern.GetDesignators())
                {
                    if (designator is BoundSingleVariableDesignator single &&
                        !Locals.Any(l => SymbolEqualityComparer.Default.Equals(l, single.Local)))
                    {
                        Locals.Add(single.Local);
                    }
                }
            }

            base.VisitAssignmentStatement(node);
        }

    }

    private void EmitIL(IEnumerable<StatementSyntax> statements, ImmutableArray<ILocalSymbol> localsToDispose, bool withReturn = true)
    {
        var statementArray = statements as StatementSyntax[] ?? statements.ToArray();

        if (statementArray.Length == 0)
        {
            ILGenerator.Emit(OpCodes.Nop);
            ILGenerator.Emit(OpCodes.Ret);
            return;
        }

        var executionScope = localsToDispose.IsDefaultOrEmpty
            ? scope
            : new Scope(scope, localsToDispose);

        var semanticModel = Compilation.GetSemanticModel(statementArray.First().SyntaxTree);

        foreach (var statement in statementArray)
        {
            var boundNode = semanticModel.GetBoundNode(statement) as BoundStatement;

            if (boundNode is null)
                continue;

            boundNode = Lowerer.LowerStatement(MethodSymbol, boundNode);
            new StatementGenerator(executionScope, boundNode).Emit();
        }

        executionScope.EmitDispose(localsToDispose);

        if (withReturn && ShouldEmitImplicitReturn())
        {
            ILGenerator.Emit(OpCodes.Nop);
            ILGenerator.Emit(OpCodes.Ret);
        }
    }

    private bool ShouldEmitImplicitReturn()
    {
        var returnType = MethodSymbol.ReturnType;
        if (returnType is null)
            return true;

        return returnType.SpecialType is SpecialType.System_Void or SpecialType.System_Unit;
    }

    private void EmitStatement(BoundStatement statement)
    {
        new StatementGenerator(scope, statement).Emit();
    }

    protected SymbolInfo GetSymbolInfo(SyntaxNode syntaxNode)
    {
        return Compilation
                        .GetSemanticModel(syntaxNode.SyntaxTree)
                        .GetSymbolInfo(syntaxNode);
    }

    protected TNode? GetDeclaredSymbol<TNode>(SyntaxNode syntaxNode)
        where TNode : class, ISymbol
    {
        return Compilation
                        .GetSemanticModel(syntaxNode.SyntaxTree)
                        .GetDeclaredSymbol(syntaxNode) as TNode;
    }

    private void EmitConstructorInitializer()
    {
        if (MethodSymbol is SourceMethodSymbol sourceMethod)
        {
            if (sourceMethod.ConstructorInitializer is { } initializer)
            {
                new ExpressionGenerator(baseGenerator, initializer).Emit();
                return;
            }

            if (sourceMethod.HasConstructorInitializerSyntax)
                return;
        }

        ILGenerator.Emit(OpCodes.Ldarg_0);
        var baseCtor = GetBaseConstructor();
        ILGenerator.Emit(OpCodes.Call, baseCtor);
    }

    private ConstructorInfo GetBaseConstructor()
    {
        var baseType = MethodSymbol.ContainingType!.BaseType!;
        var ctorSymbol = baseType.Constructors.FirstOrDefault(c => !c.IsStatic && c.Parameters.Length == 0)
            ?? throw new NotSupportedException("Base type requires a parameterless constructor");

        return MethodGenerator.TypeGenerator.CodeGen.RuntimeSymbolResolver.GetConstructorInfo(ctorSymbol);
    }

    public Type ResolveClrType(ITypeSymbol typeSymbol)
    {
        return TypeSymbolExtensionsForCodeGen.GetClrType(typeSymbol, MethodGenerator.TypeGenerator.CodeGen);
    }

    private static IEnumerable<StatementSyntax> GetTopLevelStatements(CompilationUnitSyntax compilationUnit)
    {
        foreach (var member in compilationUnit.Members)
        {
            switch (member)
            {
                case GlobalStatementSyntax global:
                    yield return global.Statement;
                    break;
                case FileScopedNamespaceDeclarationSyntax fileScoped:
                    foreach (var nestedGlobal in fileScoped.Members.OfType<GlobalStatementSyntax>())
                        yield return nestedGlobal.Statement;
                    break;
            }
        }
    }

    private int GetParameterPosition(IParameterSymbol parameterSymbol)
    {
        var parameterBuilder = MethodGenerator.GetParameterBuilder(parameterSymbol);
        var position = parameterBuilder.Position;

        if (MethodSymbol.IsStatic)
            position -= 1;

        return position;
    }

    private Type ResolveUnionCaseClrType(ITypeSymbol caseTypeSymbol)
    {
        if (caseTypeSymbol is INamedTypeSymbol namedCase)
        {
            var caseGenerator = MethodGenerator.TypeGenerator.CodeGen.GetOrCreateTypeGenerator(namedCase);
            if (caseGenerator.TypeBuilder is null)
                caseGenerator.DefineTypeBuilder();

            if (caseGenerator.TypeBuilder is not null)
                return Generator.InstantiateType(caseGenerator.TypeBuilder);
        }

        return Generator.InstantiateType(ResolveClrType(caseTypeSymbol));
    }

}

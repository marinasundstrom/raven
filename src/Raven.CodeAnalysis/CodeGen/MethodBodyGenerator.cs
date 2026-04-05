using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics.SymbolStore;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.CompilerServices;
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
    // Shared closure for the outer method when it contains lambdas that capture locals.
    // Both the outer method and its lambdas access captured locals through this shared
    // instance, giving C#-style reference-based capture semantics.
    private TypeGenerator.LambdaClosure? _outerMethodClosure;
    private IILocal? _outerMethodClosureLocal;
    private HashSet<ISymbol>? _hoistedSymbols;
    private readonly Dictionary<ILabelSymbol, ILLabel> _labels = new(ReferenceEqualityComparer.Instance);
    private readonly Dictionary<ILabelSymbol, Scope> _labelScopes = new(ReferenceEqualityComparer.Instance);
    private ILLabel? _returnLabel;
    private IILocal? _returnValueLocal;
    private static readonly ConditionalWeakTable<ModuleBuilder, Dictionary<string, ISymbolDocumentWriter>> s_documentsByModule = new();
    private SemanticModel[]? _allSemanticModels;
    private SequencePointSignature? _lastSequencePoint;
    private readonly HashSet<SequencePointSignature> _emittedSequencePoints = new();
    private bool _emittedMethodEntrySequencePoint;
    private static readonly Guid CSharpLanguageId = new("3f5162f8-07c6-11d3-9053-00c04fa302a1");
    private static readonly Guid DocumentTypeId = new("5a869d0b-6611-11d3-bd2a-0000f80849bd");
    private static readonly Guid DocumentVendorId = new("994b45c4-e6e9-11d2-903f-00c04fa302a1");
    private static readonly Guid Sha256AlgorithmId = new("8829d00f-11b8-4213-878b-770e8597ac16");
    private const int HiddenSequencePointLine = 0xFEEFEE;
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

    /// <summary>The shared closure used by the outer method for reference-based local hoisting (null when not applicable).</summary>
    internal TypeGenerator.LambdaClosure? OuterMethodClosure => _outerMethodClosure;

    /// <summary>The IL local that holds the shared outer-method closure instance.</summary>
    internal IILocal? OuterMethodClosureLocal => _outerMethodClosureLocal;

    internal bool TryGetCapturedField(ISymbol symbol, out FieldBuilder fieldBuilder)
        => TryGetCapturedField(symbol, out fieldBuilder, out _);

    internal bool TryGetCapturedField(ISymbol symbol, out FieldBuilder fieldBuilder, out bool fromStateMachine)
    {
        fromStateMachine = false;

        if (_lambdaClosure is null)
        {
            // Check the shared outer-method closure first (reference-based capture).
            if (_outerMethodClosure is not null && _outerMethodClosure.TryGetField(symbol, out fieldBuilder))
                return true;

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

        var returnType = GetEffectiveReturnTypeForEmission();
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

    private static bool TypeMayRequireBoxing(ITypeSymbol type)
    {
        if (type.IsValueType)
            return true;

        if (type is ITypeParameterSymbol typeParameter)
            return (typeParameter.ConstraintKind & TypeParameterConstraintKind.ReferenceType) == 0;

        return false;
    }

    private static bool IsDefinitelyReferenceTypeForBoxTarget(ITypeSymbol type)
    {
        if (type.IsValueType)
            return false;

        if (type is ITypeParameterSymbol typeParameter)
            return (typeParameter.ConstraintKind & TypeParameterConstraintKind.ReferenceType) != 0;

        return true;
    }

    private static bool ShouldBoxForReferenceTarget(ITypeSymbol source, ITypeSymbol target)
        => TypeMayRequireBoxing(source) && IsDefinitelyReferenceTypeForBoxTarget(target);

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
        var key = GetDocumentKey(syntaxTree);
        var moduleBuilder = MethodGenerator.TypeGenerator.CodeGen.ModuleBuilder;
        var documentMap = s_documentsByModule.GetValue(
            moduleBuilder,
            static _ => new Dictionary<string, ISymbolDocumentWriter>(StringComparer.OrdinalIgnoreCase));

        lock (documentMap)
        {
            if (documentMap.TryGetValue(key, out var cachedDocument))
                return cachedDocument;

            var document = moduleBuilder.DefineDocument(syntaxTree.FilePath, CSharpLanguageId, DocumentVendorId, DocumentTypeId);

            var checksum = syntaxTree.GetText().GetChecksum();
            if (!checksum.IsDefaultOrEmpty)
                document.SetCheckSum(Sha256AlgorithmId, checksum.ToArray());

            documentMap[key] = document;
            return document;
        }
    }

    private static string GetDocumentKey(SyntaxTree syntaxTree)
    {
        var filePath = syntaxTree.FilePath;
        if (!string.IsNullOrWhiteSpace(filePath))
            return Path.GetFullPath(filePath);

        // Fallback for synthetic trees without a stable file path.
        return $"<tree:{RuntimeHelpers.GetHashCode(syntaxTree)}>";
    }

    internal void EmitSequencePoint(BoundStatement statement)
    {
        var syntax = TryGetSequencePointSyntax(statement);
        if (syntax is null)
            return;

        EmitSequencePoint(syntax);
    }

    internal void EmitSequencePoint(SyntaxNode syntax)
    {
        if (syntax is BlockStatementSyntax or BlockSyntax)
            return;

        syntax = NormalizeSequencePointSyntaxForEmission(syntax);
        if (syntax is BlockStatementSyntax or BlockSyntax)
            return;
        if (syntax.SyntaxTree is null)
            return;

        var location = syntax switch
        {
            MatchExpressionSyntax matchExpression => matchExpression.MatchKeyword.GetLocation(),
            MatchStatementSyntax matchStatement => matchStatement.MatchKeyword.GetLocation(),
            _ => syntax.GetLocation()
        };
        var span = location.SourceSpan;

        if (span.Length == 0 || !location.IsInSource)
            return;

        var document = GetOrAddDocument(syntax.SyntaxTree);

        var lineSpan = location.GetLineSpan();
        var (startLine, startColumn, endLine, endColumn) = NormalizeSequencePoint(lineSpan, syntax.SyntaxTree);
        var signature = new SequencePointSignature(
            syntax.SyntaxTree.FilePath ?? string.Empty,
            startLine,
            startColumn,
            endLine,
            endColumn);
        if (_emittedSequencePoints.Contains(signature))
            return;

        if (_lastSequencePoint is SequencePointSignature last && last == signature)
            return;
        if (_lastSequencePoint is SequencePointSignature previous &&
            IsWiderBackwardOverlap(previous, signature))
        {
            return;
        }

        if (string.Equals(Environment.GetEnvironmentVariable("RAVEN_SEQ_LOG"), "1", StringComparison.Ordinal))
        {
            var spanLines = lineSpan.EndLinePosition.Line - lineSpan.StartLinePosition.Line;
            if (spanLines >= 8)
            {
                Console.Error.WriteLine(
                    $"[SEQ-WIDE] method={MethodSymbol.Name} syntax={syntax.GetType().Name} span={startLine}:{startColumn}-{endLine}:{endColumn} file={syntax.SyntaxTree.FilePath}");
            }
        }

        ILGenerator.Emit(OpCodes.Nop);
        try
        {
            ILGenerator.MarkSequencePoint(document, startLine, startColumn, endLine, endColumn);
        }
        catch (ArgumentOutOfRangeException ex) when (ex.ParamName == "endColumn")
        {
            throw new InvalidOperationException(
                $"Invalid sequence point for {syntax.GetType().Name} in {syntax.SyntaxTree.FilePath}: {startLine}:{startColumn}-{endLine}:{endColumn} (text length: {syntax.SyntaxTree.GetText()?.Length ?? 0})",
                ex);
        }
        _lastSequencePoint = signature;
        _emittedSequencePoints.Add(signature);
    }

    private readonly record struct SequencePointSignature(
        string FilePath,
        int StartLine,
        int StartColumn,
        int EndLine,
        int EndColumn);

    private static bool IsWiderBackwardOverlap(SequencePointSignature previous, SequencePointSignature current)
    {
        if (!string.Equals(previous.FilePath, current.FilePath, StringComparison.Ordinal))
            return false;

        // Suppress broad spans emitted after narrower spans in the same method.
        // This reduces debugger confusion where async/match lowering introduces overlaps.
        return ComparePosition(current.StartLine, current.StartColumn, previous.StartLine, previous.StartColumn) <= 0 &&
               ComparePosition(current.EndLine, current.EndColumn, previous.EndLine, previous.EndColumn) >= 0 &&
               ComparePosition(current.StartLine, current.StartColumn, previous.EndLine, previous.EndColumn) < 0 &&
               !current.Equals(previous);
    }

    private static int ComparePosition(int leftLine, int leftColumn, int rightLine, int rightColumn)
    {
        if (leftLine != rightLine)
            return leftLine.CompareTo(rightLine);

        return leftColumn.CompareTo(rightColumn);
    }

    private static (int startLine, int startColumn, int endLine, int endColumn) NormalizeSequencePoint(
        FileLinePositionSpan lineSpan,
        SyntaxTree syntaxTree)
    {
        var text = syntaxTree.GetText();

        var startLine = Math.Max(1, lineSpan.StartLinePosition.Line + 1);
        var endLine = Math.Max(1, lineSpan.EndLinePosition.Line + 1);

        var startColumn = Math.Max(1, lineSpan.StartLinePosition.Character + 1);
        var endColumn = Math.Max(1, lineSpan.EndLinePosition.Character + 1);

        var endLineLength = 0;
        if (text is not null)
        {
            startLine = Math.Min(startLine, Math.Max(1, text.GetLineCount()));
            endLine = Math.Min(endLine, Math.Max(1, text.GetLineCount()));

            var startLineLength = text.GetLineLength(startLine - 1);
            endLineLength = text.GetLineLength(endLine - 1);

            startColumn = Math.Min(startColumn, Math.Max(1, startLineLength + 1));
            endColumn = Math.Min(endColumn, Math.Max(1, endLineLength + 1));
        }

        if (endLine < startLine)
            endLine = startLine;

        if (endLine == startLine && endColumn <= startColumn)
        {
            if (endLineLength > 0)
            {
                startColumn = Math.Min(startColumn, endLineLength);
                endColumn = Math.Min(endLineLength + 1, startColumn + 1);
            }
            else
            {
                endColumn = startColumn + 1;
            }
        }

        return (startLine, startColumn, endLine, endColumn);
    }

    private static SyntaxNode NormalizeSequencePointSyntaxForEmission(SyntaxNode syntax)
    {
        return syntax switch
        {
            MethodDeclarationSyntax method => method.Body is { } methodBody && methodBody.Statements.Count > 0
                ? methodBody.Statements[0]
                : (SyntaxNode?)method.ExpressionBody?.Expression ?? method,
            FunctionStatementSyntax function => function.Body is { } functionBody && functionBody.Statements.Count > 0
                ? functionBody.Statements[0]
                : (SyntaxNode?)function.ExpressionBody?.Expression ?? function,
            CompilationUnitSyntax compilationUnit => (SyntaxNode?)GetTopLevelStatements(compilationUnit).FirstOrDefault() ?? compilationUnit,
            ArrowExpressionClauseSyntax arrow => arrow.Expression,
            FunctionExpressionSyntax lambda => (SyntaxNode?)lambda.Body ?? lambda.ExpressionBody?.Expression ?? lambda,
            BlockSyntax block => block.Statements.Count > 0 ? block.Statements[0] : block,
            _ => syntax
        };
    }

    private SyntaxNode? TryGetSyntax(BoundNode node)
    {
        // Prefer syntax mapped from the original (unlowered) bound tree. Lowered
        // syntax propagation can pick sibling arm spans for synthesized control-flow
        // nodes and produce debugger hops across match arms.
        var syntax = TryGetOriginalSyntax(node);
        if (syntax is not null)
            return syntax;

        // Fallback for synthesized lowered-only nodes when no original mapping exists.
        return TryGetSyntaxCore(node, static (model, n) => model.GetSyntax(n));
    }

    private SyntaxNode? TryGetOriginalSyntax(BoundNode node)
        => TryGetSyntaxCore(node, static (model, n) => model.GetOriginalSyntax(n));

    private SyntaxNode? TryGetSyntaxCore(BoundNode node, Func<SemanticModel, BoundNode, SyntaxNode?> resolve)
    {
        var syntaxRef = MethodSymbol.DeclaringSyntaxReferences.FirstOrDefault();
        if (syntaxRef is not null)
        {
            var semanticModel = Compilation.GetSemanticModel(syntaxRef.SyntaxTree);
            var syntax = resolve(semanticModel, node);
            if (syntax is not null)
                return syntax;
        }

        // Synthesized entry points/top-level methods can be bound through a different
        // semantic model than the method-declaring syntax reference. Search all source
        // semantic models to recover source-mapped syntax for sequence points.
        foreach (var semanticModel in GetAllSemanticModels())
        {
            var syntax = resolve(semanticModel, node);
            if (syntax is not null)
                return syntax;
        }

        return null;
    }

    private SemanticModel[] GetAllSemanticModels()
    {
        if (_allSemanticModels is not null)
            return _allSemanticModels;

        _allSemanticModels = Compilation.SyntaxTrees
            .Select(Compilation.GetSemanticModel)
            .ToArray();
        return _allSemanticModels;
    }

    private SyntaxNode? TryGetSequencePointSyntax(BoundStatement statement)
    {
        var matchSpecific = TryGetMatchSequencePointSyntax(statement);
        if (matchSpecific is not null)
            return NormalizeMatchSequencePointSyntax(matchSpecific);

        var direct = NormalizeSequencePointSyntax(TryGetSyntax(statement));
        if (direct is not null)
            return direct;

        var fallback = statement switch
        {
            BoundExpressionStatement expressionStatement => TryGetSyntax(expressionStatement)
                ?? TryGetSyntax(expressionStatement.Expression),
            BoundAssignmentStatement assignmentStatement => TryGetSyntax(assignmentStatement)
                ?? TryGetSyntax(assignmentStatement.Expression),
            BoundReturnStatement { Expression: not null } returnStatement => TryGetSyntax(returnStatement.Expression),
            BoundThrowStatement { Expression: not null } throwStatement => TryGetSyntax(throwStatement.Expression),
            BoundIfStatement ifStatement => TryGetSyntax(ifStatement.Condition)
                ?? TryGetSequencePointSyntax(ifStatement.ThenNode)
                ?? (ifStatement.ElseNode is null ? null : TryGetSequencePointSyntax(ifStatement.ElseNode)),
            BoundLocalDeclarationStatement localDeclaration => TryGetSyntax(localDeclaration)
                ?? localDeclaration.Declarators
                    .Select(static d => d.Initializer)
                    .Where(static i => i is not null)
                    .Select(i => TryGetSyntax(i!))
                    .FirstOrDefault(s => s is not null),
            BoundBlockStatement block => block.Statements
                .Select(TryGetSequencePointSyntax)
                .FirstOrDefault(s => s is not null),
            _ => null
        };

        return NormalizeSequencePointSyntax(fallback ?? TryFindSequencePointSyntax(statement));
    }

    private SyntaxNode? NormalizeMatchSequencePointSyntax(SyntaxNode? syntax)
    {
        if (syntax is null)
            return null;

        if (syntax is MatchArmSyntax armSyntax)
            return GetMatchArmEntrySyntax(armSyntax);

        if (syntax is WhenClauseSyntax whenClause)
            return whenClause.Guard as SyntaxNode;

        if (syntax.GetAncestor<MatchArmSyntax>() is not null)
            return syntax;

        if (syntax is MatchExpressionSyntax or MatchStatementSyntax)
            return syntax;

        return NormalizeSequencePointSyntax(syntax);
    }

    private SyntaxNode? TryGetMatchSequencePointSyntax(BoundStatement statement)
    {
        if (statement is BoundBlockStatement block &&
            TryGetEnclosingMatchSyntax(block) is { } firstMatchSyntax)
        {
            return firstMatchSyntax;
        }

        if (statement is BoundIfStatement guardIf &&
            TryGetMatchArmSyntax(guardIf.ThenNode) is { } guardArmSyntax &&
            guardArmSyntax.WhenClause is { } whenClauseSyntax)
        {
            return whenClauseSyntax.Guard as SyntaxNode;
        }

        if (statement is BoundAssignmentStatement
            {
                Expression: BoundLocalAssignmentExpression { Right: var rightExpression }
            } &&
            TryGetOriginalSyntax(rightExpression)?.GetAncestor<MatchArmSyntax>() is { } assignmentArm)
        {
            return GetMatchArmEntrySyntax(assignmentArm);
        }

        if (statement is BoundLocalDeclarationStatement localDeclaration &&
            IsMatchScrutineeLocalDeclaration(localDeclaration) &&
            TryGetMatchSyntaxFromDeclarators(localDeclaration) is { } matchSyntax)
        {
            return matchSyntax;
        }

        if (statement is BoundIfStatement { Condition: BoundIsPatternExpression patternCondition } &&
            TryGetMatchArmSyntax(patternCondition) is { } armSyntax)
        {
            var enclosingMatchSyntax = GetMatchSyntax(armSyntax);
            if (enclosingMatchSyntax is not null)
                return enclosingMatchSyntax;
        }
        else if (statement is BoundIfStatement candidateIf &&
            TryGetMatchArmSyntax(candidateIf.ThenNode) is { } thenArmSyntax &&
            GetMatchSyntax(thenArmSyntax) is { } thenMatchSyntax)
        {
            return thenMatchSyntax;
        }

        return null;
    }

    private static bool IsMatchScrutineeLocalDeclaration(BoundLocalDeclarationStatement localDeclaration)
    {
        foreach (var declarator in localDeclaration.Declarators)
        {
            if (declarator.Local.Name.StartsWith("<match_scrutinee>", StringComparison.Ordinal))
                return true;
        }

        return false;
    }

    private SyntaxNode? TryGetMatchSyntaxFromDeclarators(BoundLocalDeclarationStatement localDeclaration)
    {
        foreach (var declarator in localDeclaration.Declarators)
        {
            if (declarator.Initializer is null)
                continue;

            var initializerSyntax = TryGetOriginalSyntax(declarator.Initializer);
            if (initializerSyntax is null)
                continue;

            if (initializerSyntax.GetAncestor<MatchStatementSyntax>() is { } matchStatement)
                return matchStatement;

            if (initializerSyntax.GetAncestor<MatchExpressionSyntax>() is { } matchExpression)
                return matchExpression;
        }

        return null;
    }

    private static SyntaxNode GetMatchArmEntrySyntax(MatchArmSyntax armSyntax)
    {
        if (armSyntax.Expression is BlockSyntax block && block.Statements.Count > 0)
            return block.Statements[0];

        return armSyntax.Expression;
    }

    private static SyntaxNode? GetMatchSyntax(MatchArmSyntax armSyntax)
    {
        return armSyntax.GetAncestor<MatchStatementSyntax>()
            ?? (SyntaxNode?)armSyntax.GetAncestor<MatchExpressionSyntax>();
    }

    private MatchArmSyntax? TryGetMatchArmSyntax(BoundIsPatternExpression patternCondition)
    {
        static MatchArmSyntax? FromSyntaxNode(SyntaxNode? syntax)
        {
            if (syntax is null)
                return null;

            return syntax as MatchArmSyntax
                ?? syntax.GetAncestor<MatchArmSyntax>();
        }

        return FromSyntaxNode(TryGetOriginalSyntax(patternCondition.Pattern))
            ?? FromSyntaxNode(TryGetOriginalSyntax(patternCondition))
            ?? FromSyntaxNode(TryGetOriginalSyntax(patternCondition.Expression));
    }

    private MatchArmSyntax? TryGetMatchArmSyntax(BoundStatement statement)
    {
        switch (statement)
        {
            case BoundAssignmentStatement { Expression: BoundLocalAssignmentExpression localAssignment }:
                return TryGetMatchArmSyntax(localAssignment.Right);

            case BoundExpressionStatement expressionStatement:
                return TryGetMatchArmSyntax(expressionStatement.Expression);

            case BoundReturnStatement { Expression: not null } returnStatement:
                return TryGetMatchArmSyntax(returnStatement.Expression);

            case BoundThrowStatement { Expression: not null } throwStatement:
                return TryGetMatchArmSyntax(throwStatement.Expression);

            case BoundBlockStatement blockStatement:
                foreach (var child in blockStatement.Statements)
                {
                    if (TryGetMatchArmSyntax(child) is { } nestedArm)
                        return nestedArm;
                }

                return null;

            default:
                return null;
        }
    }

    private MatchArmSyntax? TryGetMatchArmSyntax(BoundExpression expression)
    {
        static MatchArmSyntax? FromSyntaxNode(SyntaxNode? syntax)
        {
            if (syntax is null)
                return null;

            return syntax as MatchArmSyntax
                ?? syntax.GetAncestor<MatchArmSyntax>();
        }

        return FromSyntaxNode(TryGetOriginalSyntax(expression));
    }

    private SyntaxNode? TryGetEnclosingMatchSyntax(BoundBlockStatement block)
    {
        foreach (var childStatement in block.Statements)
        {
            if (childStatement is BoundIfStatement { Condition: BoundIsPatternExpression patternCondition } &&
                TryGetMatchArmSyntax(patternCondition) is { } armSyntax &&
                GetMatchSyntax(armSyntax) is { } matchSyntax)
            {
                return matchSyntax;
            }

            var statementSyntax = TryGetOriginalSyntax(childStatement);
            if (statementSyntax is null)
                continue;

            if (statementSyntax.GetAncestor<MatchStatementSyntax>() is { } matchStatement)
                return matchStatement;

            if (statementSyntax.GetAncestor<MatchExpressionSyntax>() is { } matchExpression)
                return matchExpression;
        }

        return null;
    }

    private SyntaxNode? TryFindSequencePointSyntax(BoundStatement statement)
    {
        var finder = new SequencePointSyntaxFinder(this);
        finder.VisitStatement(statement);
        return finder.Result;
    }

    private SyntaxNode? NormalizeSequencePointSyntax(SyntaxNode? syntax)
    {
        if (syntax is null)
            return null;

        var isMatchRelatedSyntax =
            syntax is MatchExpressionSyntax or MatchStatementSyntax
            || syntax.GetAncestor<MatchExpressionSyntax>() is not null
            || syntax.GetAncestor<MatchStatementSyntax>() is not null
            || syntax.GetAncestor<MatchArmSyntax>() is not null
            || syntax.GetAncestor<WhenClauseSyntax>() is not null;

        var statementSyntax = syntax.AncestorsAndSelf()
            .OfType<StatementSyntax>()
            .FirstOrDefault(static statement => statement is
                ExpressionStatementSyntax or
                LocalDeclarationStatementSyntax or
                AssignmentStatementSyntax or
                ReturnStatementSyntax or
                ThrowStatementSyntax or
                UseDeclarationStatementSyntax);
        if (!isMatchRelatedSyntax &&
            statementSyntax is not null &&
            CanLiftToStatement(statementSyntax))
            return statementSyntax;

        // For expression statements, prefer statement-level mapping so debugger
        // highlighting covers the full executable line.
        var invocation = syntax.AncestorsAndSelf().OfType<InvocationExpressionSyntax>().FirstOrDefault();
        if (invocation?.Parent is ExpressionStatementSyntax invocationStatement &&
            CanLiftToInvocationStatement(invocationStatement))
            return invocationStatement;

        // Otherwise, when syntax mapping lands on a nested node inside an invocation
        // (for example the receiver identifier `Console`), lift to the invocation
        // expression to avoid token-only highlighting.
        if (invocation is not null &&
            !ReferenceEquals(invocation, syntax) &&
            CanLiftToInvocationExpression(invocation))
            return invocation;

        return syntax switch
        {
            MethodDeclarationSyntax method => method.Body is { } methodBody
                ? methodBody.Statements.FirstOrDefault()
                : (SyntaxNode?)method.ExpressionBody?.Expression ?? method,
            FunctionStatementSyntax function => function.Body is { } functionBody
                ? functionBody.Statements.FirstOrDefault()
                : (SyntaxNode?)function.ExpressionBody?.Expression ?? function,
            FunctionExpressionSyntax lambda => lambda.Body switch
            {
                { Statements.Count: > 0 } block => block.Statements[0],
                { } block => block,
                _ => (SyntaxNode?)lambda.ExpressionBody?.Expression ?? lambda
            },
            CompilationUnitSyntax compilationUnit => (SyntaxNode?)GetTopLevelStatements(compilationUnit).FirstOrDefault() ?? compilationUnit,
            BlockStatementSyntax => null,
            _ => syntax
        };
    }

    private bool CanLiftToInvocationStatement(ExpressionStatementSyntax invocationStatement)
    {
        if (!CanLiftToStatement(invocationStatement))
            return false;

        if (MethodSymbol.MethodKind != MethodKind.LambdaMethod)
            return true;

        if (!TryGetCurrentLambdaSyntax(out var lambdaSyntax))
            return true;

        return lambdaSyntax.Span.Contains(invocationStatement.Span);
    }

    private bool CanLiftToInvocationExpression(InvocationExpressionSyntax invocation)
    {
        if (MethodSymbol.MethodKind == MethodKind.LambdaMethod &&
            TryGetCurrentLambdaSyntax(out var lambdaSyntax) &&
            !lambdaSyntax.Span.Contains(invocation.Span))
        {
            return false;
        }

        if (MethodSymbol.MethodKind != MethodKind.LambdaMethod)
            return true;

        if (!TryGetCurrentLambdaSyntax(out lambdaSyntax))
            return true;

        return lambdaSyntax.Span.Contains(invocation.Span);
    }

    private bool CanLiftToStatement(StatementSyntax statementSyntax)
    {
        if (MethodSymbol.MethodKind != MethodKind.LambdaMethod)
            return true;

        if (!TryGetCurrentLambdaSyntax(out var lambdaSyntax))
            return true;

        return lambdaSyntax.Span.Contains(statementSyntax.Span);
    }

    private bool TryGetCurrentLambdaSyntax(out FunctionExpressionSyntax lambdaSyntax)
    {
        lambdaSyntax = MethodSymbol.DeclaringSyntaxReferences
            .Select(static r => r.GetSyntax())
            .OfType<FunctionExpressionSyntax>()
            .FirstOrDefault()!;

        return lambdaSyntax is not null;
    }

    private sealed class SequencePointSyntaxFinder : BoundTreeWalker
    {
        private readonly MethodBodyGenerator _owner;

        public SequencePointSyntaxFinder(MethodBodyGenerator owner)
        {
            _owner = owner;
        }

        public SyntaxNode? Result { get; private set; }

        public override void VisitStatement(BoundStatement statement)
        {
            if (TryCapture(statement))
                return;

            base.VisitStatement(statement);
        }

        public override void VisitExpression(BoundExpression node)
        {
            if (TryCapture(node))
                return;

            base.VisitExpression(node);
        }

        public override void VisitPattern(BoundPattern node)
        {
            if (TryCapture(node))
                return;

            base.VisitPattern(node);
        }

        public override void VisitVariableDeclarator(BoundVariableDeclarator node)
        {
            if (TryCapture(node))
                return;

            base.VisitVariableDeclarator(node);
        }

        public override void VisitCatchClause(BoundCatchClause node)
        {
            if (TryCapture(node))
                return;

            base.VisitCatchClause(node);
        }

        private bool TryCapture(BoundNode node)
        {
            if (Result is not null)
                return true;

            var syntax = _owner.TryGetSyntax(node);
            if (syntax is null || syntax.Span.Length == 0 || syntax.SyntaxTree is null)
                return false;

            var location = syntax.GetLocation();
            if (!location.IsInSource)
                return false;

            Result = syntax;
            return true;
        }
    }

    internal void EmitLoadClosure()
    {
        // Outer method with a shared hoisted closure: load the closure local variable.
        if (_lambdaClosure is null && _outerMethodClosure is not null && _outerMethodClosureLocal is not null)
        {
            ILGenerator.Emit(OpCodes.Ldloc, _outerMethodClosureLocal);
            return;
        }

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
        _lambdaClosure = MethodGenerator.LambdaClosure;

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

        if (Compilation.TryGetSynthesizedMethodBody(MethodSymbol, BoundTreeView.Lowered, out var synthesizedBody) &&
            synthesizedBody is not null)
        {
            if (MethodSymbol.MethodKind == MethodKind.Constructor &&
                MethodSymbol.ContainingType.TryGetUnion() is not null &&
                MethodSymbol.DeclaringSyntaxReferences.IsDefaultOrEmpty &&
                TryGetSourceDiscriminatedUnionDefinition(MethodSymbol.ContainingType) is { } constructorUnion)
            {
                EmitDiscriminatedUnionCarrierConstructorPrelude(constructorUnion);
            }

            if (MethodSymbol.MethodKind == MethodKind.Constructor &&
                TryGetSourceTypeDefinition(MethodSymbol.ContainingType) is { IsRecord: true } recordType &&
                MethodSymbol.Parameters.Length == 1 &&
                SymbolEqualityComparer.Default.Equals(MethodSymbol.Parameters[0].Type, MethodSymbol.ContainingType))
            {
                EmitRecordCopyConstructorPrelude(recordType);
            }
            else if (MethodSymbol.MethodKind == MethodKind.Constructor &&
                TryGetSourceUnionCaseTypeDefinition(MethodSymbol.ContainingType) is { } unionCaseType)
            {
                EmitSynthesizedConstructorPrelude(unionCaseType);
            }

            DeclareLocals(synthesizedBody);
            EmitMethodBlock(synthesizedBody);
            return;
        }

        var sourceType = TryGetSourceTypeDefinition(MethodSymbol.ContainingType);
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
            MethodDeclarationSyntax m when m.Body != null => semanticModel.GetBoundNode(m.Body, BoundTreeView.Lowered) as BoundBlockStatement,
            OperatorDeclarationSyntax o when o.Body != null => semanticModel.GetBoundNode(o.Body, BoundTreeView.Lowered) as BoundBlockStatement,
            ConversionOperatorDeclarationSyntax c when c.Body != null => semanticModel.GetBoundNode(c.Body, BoundTreeView.Lowered) as BoundBlockStatement,
            FunctionStatementSyntax l when l.Body != null => semanticModel.GetBoundNode(l.Body, BoundTreeView.Lowered) as BoundBlockStatement,
            FunctionStatementSyntax l when l.ExpressionBody is not null => GetLoweredArrowExpressionBody(semanticModel, l.ExpressionBody),
            BaseConstructorDeclarationSyntax c when c.Body != null => semanticModel.GetBoundNode(c.Body, BoundTreeView.Lowered) as BoundBlockStatement,
            ParameterlessConstructorDeclarationSyntax i when i.Body != null => semanticModel.GetBoundNode(i.Body, BoundTreeView.Lowered) as BoundBlockStatement,
            InitializerBlockDeclarationSyntax i when i.Body != null => semanticModel.GetBoundNode(i.Body, BoundTreeView.Lowered) as BoundBlockStatement,
            FinallyDeclarationSyntax f when f.Body != null => semanticModel.GetBoundNode(f.Body, BoundTreeView.Lowered) as BoundBlockStatement,
            AccessorDeclarationSyntax a when a.Body != null => semanticModel.GetBoundNode(a.Body, BoundTreeView.Lowered) as BoundBlockStatement,
            AccessorDeclarationSyntax a when a.ExpressionBody is not null => GetLoweredArrowExpressionBody(semanticModel, a.ExpressionBody),
            PropertyDeclarationSyntax p when p.ExpressionBody is not null => GetLoweredArrowExpressionBody(semanticModel, p.ExpressionBody),
            _ => null
        };

        BoundExpression? expressionBody = syntax switch
        {
            MethodDeclarationSyntax m when m.ExpressionBody is not null
                => semanticModel.GetBoundNode(m.ExpressionBody.Expression, BoundTreeView.Lowered) as BoundExpression,
            OperatorDeclarationSyntax o when o.ExpressionBody is not null
                => semanticModel.GetBoundNode(o.ExpressionBody.Expression, BoundTreeView.Lowered) as BoundExpression,
            ConversionOperatorDeclarationSyntax c when c.ExpressionBody is not null
                => semanticModel.GetBoundNode(c.ExpressionBody.Expression, BoundTreeView.Lowered) as BoundExpression,
            BaseConstructorDeclarationSyntax c when c.ExpressionBody is not null
                => semanticModel.GetBoundNode(c.ExpressionBody.Expression, BoundTreeView.Lowered) as BoundExpression,
            ParameterlessConstructorDeclarationSyntax i when i.ExpressionBody is not null
                => semanticModel.GetBoundNode(i.ExpressionBody.Expression, BoundTreeView.Lowered) as BoundExpression,
            AccessorDeclarationSyntax a when a.ExpressionBody is not null
                => semanticModel.GetBoundNode(a.ExpressionBody.Expression, BoundTreeView.Lowered) as BoundExpression,
            PropertyDeclarationSyntax p when p.ExpressionBody is not null
                => semanticModel.GetBoundNode(p.ExpressionBody.Expression, BoundTreeView.Lowered) as BoundExpression,
            FunctionStatementSyntax l when l.ExpressionBody is not null
                => semanticModel.GetBoundNode(l.ExpressionBody.Expression, BoundTreeView.Lowered) as BoundExpression,
            _ => null
        };

        ExpressionSyntax? expressionBodySyntax = syntax switch
        {
            MethodDeclarationSyntax m when m.ExpressionBody is not null => m.ExpressionBody.Expression,
            OperatorDeclarationSyntax o when o.ExpressionBody is not null => o.ExpressionBody.Expression,
            ConversionOperatorDeclarationSyntax c when c.ExpressionBody is not null => c.ExpressionBody.Expression,
            BaseConstructorDeclarationSyntax c when c.ExpressionBody is not null => c.ExpressionBody.Expression,
            ParameterlessConstructorDeclarationSyntax i when i.ExpressionBody is not null => i.ExpressionBody.Expression,
            AccessorDeclarationSyntax a when a.ExpressionBody is not null => a.ExpressionBody.Expression,
            PropertyDeclarationSyntax p when p.ExpressionBody is not null => p.ExpressionBody.Expression,
            FunctionStatementSyntax l when l.ExpressionBody is not null => l.ExpressionBody.Expression,
            _ => null
        };

        // Emit an initial visible point for all source-backed members so debugger
        // step-in can consistently land inside the member body.
        EmitMethodEntrySequencePointOnce();

        if (boundBody != null)
            DeclareLocals(boundBody);

        switch (syntax)
        {
            case CompilationUnitSyntax compilationUnit:
                if (MethodSymbol is SourceMethodSymbol &&
                    semanticModel.GetBoundNode(compilationUnit, BoundTreeView.Lowered) is BoundBlockStatement topLevelBody)
                {
                    DeclareLocals(topLevelBody);
                    EmitMethodBlock(topLevelBody);
                    break;
                }

                if (MethodSymbol is SourceMethodSymbol sourceTopLevelMethod &&
                    sourceTopLevelMethod.IsAsync &&
                    semanticModel.GetBoundNode(compilationUnit, BoundTreeView.Original) is BoundBlockStatement originalTopLevelBody)
                {
                    var loweredTopLevelBody = AsyncLowerer.ShouldRewrite(sourceTopLevelMethod, originalTopLevelBody)
                        ? AsyncLowerer.Rewrite(sourceTopLevelMethod, originalTopLevelBody)
                        : originalTopLevelBody;

                    DeclareLocals(loweredTopLevelBody);
                    EmitMethodBlock(loweredTopLevelBody);
                    break;
                }

                if (MethodSymbol is SourceMethodSymbol asyncTopLevelMethod &&
                    asyncTopLevelMethod.IsAsync)
                {
                    var topLevelStatements = GetTopLevelStatements(compilationUnit).ToArray();
                    var boundStatements = new List<BoundStatement>(topLevelStatements.Length);
                    var localsToDispose = ImmutableArray.CreateBuilder<ILocalSymbol>();

                    foreach (var statementSyntax in topLevelStatements)
                    {
                        if (semanticModel.GetBoundNode(statementSyntax, BoundTreeView.Original) is BoundStatement boundStatement)
                            boundStatements.Add(boundStatement);

                        if (statementSyntax is UseDeclarationStatementSyntax useDeclarationStatement)
                        {
                            foreach (var declarator in useDeclarationStatement.Declaration.Declarators)
                            {
                                if (GetDeclaredSymbol<ILocalSymbol>(declarator) is { } localSymbol)
                                    localsToDispose.Add(localSymbol);
                            }
                        }
                    }

                    var synthesizedBlock = new BoundBlockStatement(boundStatements, localsToDispose.ToImmutable());
                    var loweredTopLevelBody = AsyncLowerer.ShouldRewrite(asyncTopLevelMethod, synthesizedBlock)
                        ? AsyncLowerer.Rewrite(asyncTopLevelMethod, synthesizedBlock)
                        : synthesizedBlock;

                    DeclareLocals(loweredTopLevelBody);
                    EmitMethodBlock(loweredTopLevelBody);
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
                if (!MethodSymbol.IsStatic)
                {
                    EmitConstructorInitializer();
                }

                EmitFieldInitializers(MethodSymbol.IsStatic);
                EmitLifecycleInitBlocks(semanticModel, MethodSymbol.IsStatic);
                EmitPrimaryInitializerBlocks(semanticModel);

                if (boundBody != null)
                    EmitMethodBlock(boundBody, includeImplicitReturn: false);
                else if (expressionBody is not null)
                {
                    if (expressionBodySyntax is not null)
                        EmitSequencePoint(expressionBodySyntax);

                    EmitExpressionBody(expressionBody, includeReturn: false);
                }

                ILGenerator.Emit(OpCodes.Ret);
                break;

            case ParameterlessConstructorDeclarationSyntax:
                if (!MethodSymbol.IsStatic)
                {
                    EmitConstructorInitializer();
                }

                EmitFieldInitializers(MethodSymbol.IsStatic);
                EmitLifecycleInitBlocks(semanticModel, MethodSymbol.IsStatic);
                EmitPrimaryInitializerBlocks(semanticModel);

                ILGenerator.Emit(OpCodes.Ret);
                break;

            case InitializerBlockDeclarationSyntax:
                if (!MethodSymbol.IsStatic)
                {
                    EmitConstructorInitializer();
                }

                EmitFieldInitializers(MethodSymbol.IsStatic);
                EmitLifecycleInitBlocks(semanticModel, MethodSymbol.IsStatic);
                EmitPrimaryInitializerBlocks(semanticModel);

                ILGenerator.Emit(OpCodes.Ret);
                break;

            case FinallyDeclarationSyntax:
                if (boundBody != null)
                    EmitMethodBlock(boundBody);
                else
                    ILGenerator.Emit(OpCodes.Ret);
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
                            EmitExpressionBody(expressionBody, includeReturn: false);
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
                    else
                    {
                        ILGenerator.Emit(OpCodes.Ret);
                    }
                    break;
                }

            case PropertyDeclarationSyntax propertyDeclaration:
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
                EmitLifecycleInitBlocks(semanticModel, MethodSymbol.IsStatic);
                EmitPrimaryInitializerBlocks(semanticModel);

                ILGenerator.Emit(OpCodes.Ret);
                break;

            default:
                throw new InvalidOperationException($"Unsupported syntax node in MethodBodyGenerator: {syntax.GetType().Name}");
        }
    }

    private void EmitLifecycleInitBlocks(SemanticModel semanticModel, bool isStatic)
    {
        var ownerTypeSyntax = TryGetOwningTypeSyntax();

        if (ownerTypeSyntax is null)
            return;

        foreach (var initDecl in ownerTypeSyntax.Members.OfType<ParameterlessConstructorDeclarationSyntax>())
        {
            var initIsStatic = initDecl.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);
            if (initIsStatic != isStatic)
                continue;

            if (initDecl.Body is not null &&
                semanticModel.GetBoundNode(initDecl.Body, BoundTreeView.Lowered) is BoundBlockStatement initBlock)
            {
                EmitBoundBlock(initBlock);
            }
            else if (initDecl.ExpressionBody is not null &&
                     semanticModel.GetBoundNode(initDecl.ExpressionBody.Expression, BoundTreeView.Lowered) is BoundExpression initExpr)
            {
                if (initDecl.ExpressionBody.Expression is { } exprSyntax)
                    EmitSequencePoint(exprSyntax);

                EmitExpressionBody(initExpr, includeReturn: false);
            }
        }
    }

    private void EmitPrimaryInitializerBlocks(SemanticModel semanticModel)
    {
        if (!IsPrimaryConstructorSymbol(MethodSymbol))
            return;

        var ownerTypeSyntax = TryGetOwningTypeSyntax();
        if (ownerTypeSyntax is null)
            return;

        foreach (var initBlockDecl in ownerTypeSyntax.Members.OfType<InitializerBlockDeclarationSyntax>())
        {
            if (semanticModel.GetBoundNode(initBlockDecl.Body, BoundTreeView.Lowered) is not BoundBlockStatement initBlock)
                continue;

            EmitBoundBlock(initBlock);
        }
    }

    private TypeDeclarationSyntax? TryGetOwningTypeSyntax()
    {
        if (MethodSymbol.DeclaringSyntaxReferences.IsDefaultOrEmpty)
            return null;

        if (MethodSymbol is SourceMethodSymbol sourceMethod)
            return sourceMethod.GetDeclaringTypeSyntax();

        return MethodSymbol.DeclaringSyntaxReferences
            .Select(r => r.GetSyntax())
            .OfType<TypeDeclarationSyntax>()
            .FirstOrDefault()
            ?? MethodSymbol.DeclaringSyntaxReferences
                .Select(r => r.GetSyntax())
                .OfType<MemberDeclarationSyntax>()
                .Select(m => m.Parent)
                .OfType<TypeDeclarationSyntax>()
                .FirstOrDefault();
    }

    private static bool IsPrimaryConstructorSymbol(IMethodSymbol methodSymbol)
    {
        if (methodSymbol is SourceMethodSymbol sourceMethod)
            return sourceMethod.IsPrimaryConstructorSymbol;

        if (methodSymbol.MethodKind != MethodKind.Constructor || methodSymbol.IsStatic)
            return false;

        return methodSymbol.DeclaringSyntaxReferences
            .Select(static r => r.GetSyntax())
            .OfType<TypeDeclarationSyntax>()
            .Any(static typeDecl => typeDecl.ParameterList is not null);
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

        var resultUnion = resultType.TryGetUnion();
        var okCase = resultUnion?.CaseTypes.FirstOrDefault(c => c.Name == "Ok") as INamedTypeSymbol;
        var errorCase = resultUnion?.CaseTypes.FirstOrDefault(c => c.Name == "Error") as INamedTypeSymbol;

        var okMethod = FindTryGetValueMethod(resultType, okCase);
        var errorMethod = FindTryGetValueMethod(resultType, errorCase);

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
            ILGenerator.Emit(OpCodes.Brfalse, nextLabel);
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
            ILGenerator.Emit(OpCodes.Brfalse, nextLabel);
            EmitWriteCasePayloadToStderr(errorCaseLocal, errorCaseRuntimeType, errorMethod.Parameters[0].Type, "Error");

            if (isIntBridge)
            {
                ILGenerator.Emit(OpCodes.Ldc_I4_1);
                ILGenerator.Emit(OpCodes.Ret);
            }
            else
            {
                ILGenerator.Emit(OpCodes.Br, doneLabel);
            }

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
        if (normalizedCaseType is RefTypeSymbol refType)
            normalizedCaseType = refType.ElementType;

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

    private static IMethodSymbol? FindTryGetValueMethod(INamedTypeSymbol type, INamedTypeSymbol? caseType)
    {
        if (caseType is null)
            return null;

        var caseTypePlain = caseType.GetPlainType();

        return type.GetMembers("TryGetValue")
            .OfType<IMethodSymbol>()
            .FirstOrDefault(method =>
                method.Parameters.Length == 1 &&
                method.Parameters[0].RefKind == RefKind.Out &&
                method.ReturnType.SpecialType == SpecialType.System_Boolean &&
                SymbolEqualityComparer.Default.Equals(
                    method.Parameters[0].GetByRefElementType().GetPlainType(),
                    caseTypePlain));
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
            var asyncStateMachineClrType = ResolveClrType(asyncStateMachine);
            ILGenerator.Emit(OpCodes.Ldarg_0);
            ILGenerator.Emit(OpCodes.Initobj, asyncStateMachineClrType);
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

        if (iteratorType.DisposeMethod is not null &&
            SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.DisposeMethod))
            return iteratorType.DisposeBody;

        if (iteratorType.AsyncDisposeMethod is not null &&
            SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.AsyncDisposeMethod))
            return iteratorType.AsyncDisposeBody;

        if (iteratorType.ResetMethod is not null &&
            SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.ResetMethod))
            return iteratorType.ResetBody;

        if (SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.CurrentProperty.GetMethod))
            return iteratorType.CurrentGetterBody;

        if (iteratorType.NonGenericCurrentProperty?.GetMethod is not null &&
            SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.NonGenericCurrentProperty.GetMethod))
            return iteratorType.NonGenericCurrentGetterBody;

        if (iteratorType.GenericGetEnumeratorMethod is not null &&
            SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.GenericGetEnumeratorMethod))
            return iteratorType.GenericGetEnumeratorBody;

        if (iteratorType.NonGenericGetEnumeratorMethod is not null &&
            SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.NonGenericGetEnumeratorMethod))
            return iteratorType.NonGenericGetEnumeratorBody;

        if (iteratorType.AsyncMoveNextMethod is not null &&
            SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.AsyncMoveNextMethod))
            return iteratorType.AsyncMoveNextBody;

        if (iteratorType.AsyncGetEnumeratorMethod is not null &&
            SymbolEqualityComparer.Default.Equals(MethodSymbol, iteratorType.AsyncGetEnumeratorMethod))
            return iteratorType.AsyncGetEnumeratorBody;

        return null;
    }

    public void EmitLambda(
        BoundFunctionExpression lambda,
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

    private static BoundBlockStatement CreateLambdaBody(BoundFunctionExpression lambda)
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
            if (field.Initializer is BoundParameterAccess parameterAccess &&
                MethodSymbol.Parameters.All(p => !SymbolEqualityComparer.Default.Equals(p, parameterAccess.Parameter)))
            {
                continue;
            }

            BoundExpression assignment = new BoundFieldAssignmentExpression(
                isStatic ? null : new BoundSelfExpression(MethodSymbol.ContainingType!),
                field,
                field.Initializer!,
                Compilation.GetSpecialType(SpecialType.System_Unit));

            var statement = new BoundAssignmentStatement((BoundAssignmentExpression)assignment);
            new StatementGenerator(baseGenerator, statement).Emit();
        }
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

    private static SourceNamedTypeSymbol? TryGetSourceTypeDefinition(INamedTypeSymbol? typeSymbol)
    {
        return typeSymbol switch
        {
            SourceNamedTypeSymbol source => source,
            ConstructedNamedTypeSymbol { OriginalDefinition: SourceNamedTypeSymbol source } => source,
            _ => null,
        };
    }

    private static SourceNamedTypeSymbol? TryGetSourceUnionCaseTypeDefinition(INamedTypeSymbol? typeSymbol)
    {
        return typeSymbol switch
        {
            SourceUnionCaseTypeSymbol sourceCase => sourceCase,
            ConstructedNamedTypeSymbol { OriginalDefinition: SourceUnionCaseTypeSymbol sourceCase } => sourceCase,
            _ => null,
        };
    }

    private void EmitDiscriminatedUnionCarrierConstructorPrelude(SourceUnionSymbol unionSymbol)
    {
        var unionClrType = Generator.InstantiateType(
            MethodGenerator.TypeGenerator.TypeBuilder
                ?? ResolveClrType(MethodSymbol.ContainingType!));

        if (unionSymbol.TypeKind == TypeKind.Struct)
        {
            ILGenerator.Emit(OpCodes.Ldarg_0);
            ILGenerator.Emit(OpCodes.Initobj, unionClrType);
            return;
        }

        var objectCtor = typeof(object).GetConstructor(Type.EmptyTypes)
            ?? throw new InvalidOperationException("Missing System.Object constructor.");
        ILGenerator.Emit(OpCodes.Ldarg_0);
        ILGenerator.Emit(OpCodes.Call, objectCtor);
    }

    private void EmitRecordCopyConstructorPrelude(SourceNamedTypeSymbol recordType)
    {
        var baseType = recordType.BaseType;
        var baseCopyCtorSymbol = baseType is SourceNamedTypeSymbol { IsRecord: true } sourceBase
            ? sourceBase.Constructors.FirstOrDefault(c =>
                !c.IsStatic &&
                c.Parameters.Length == 1 &&
                SymbolEqualityComparer.Default.Equals(c.Parameters[0].Type, sourceBase))
            : null;

        ILGenerator.Emit(OpCodes.Ldarg_0);

        if (baseCopyCtorSymbol is not null)
        {
            var baseCopyCtor = MethodGenerator.TypeGenerator.CodeGen.RuntimeSymbolResolver.GetConstructorInfo(baseCopyCtorSymbol);
            ILGenerator.Emit(OpCodes.Ldarg_1);
            ILGenerator.Emit(OpCodes.Call, baseCopyCtor);
            return;
        }

        var baseCtor = GetBaseConstructor();
        ILGenerator.Emit(OpCodes.Call, baseCtor);
    }

    private void EmitSynthesizedConstructorPrelude(SourceNamedTypeSymbol containingType)
    {
        if (containingType.TypeKind == TypeKind.Struct)
        {
            var clrType = Generator.InstantiateType(
                MethodGenerator.TypeGenerator.TypeBuilder
                    ?? ResolveClrType(MethodSymbol.ContainingType!));
            ILGenerator.Emit(OpCodes.Ldarg_0);
            ILGenerator.Emit(OpCodes.Initobj, clrType);
            return;
        }

        ILGenerator.Emit(OpCodes.Ldarg_0);
        ILGenerator.Emit(OpCodes.Call, GetBaseConstructor());
    }

    private static SourceUnionSymbol? TryGetSourceDiscriminatedUnionDefinition(INamedTypeSymbol? typeSymbol)
    {
        switch (typeSymbol)
        {
            case SourceUnionSymbol sourceUnion:
                return sourceUnion;
            case ConstructedNamedTypeSymbol { OriginalDefinition: SourceUnionSymbol sourceUnion }:
                return sourceUnion;
            default:
                return null;
        }
    }

    private void EmitExpressionBody(BoundExpression expression, bool includeReturn = true)
    {
        var returnType = GetEffectiveReturnTypeForEmission();

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

    private BoundBlockStatement? GetLoweredArrowExpressionBody(SemanticModel semanticModel, ArrowExpressionClauseSyntax expressionBody)
    {
        if (semanticModel.GetBoundNode(expressionBody, BoundTreeView.Original) is not BoundBlockStatement originalBody)
            return null;

        if (MethodSymbol is SourceMethodSymbol sourceMethod &&
            AsyncLowerer.ShouldRewrite(sourceMethod, originalBody))
        {
            return AsyncLowerer.Rewrite(sourceMethod, originalBody);
        }

        return Lowerer.LowerBlock(MethodSymbol, originalBody);
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

        var sourceMethod = methodSymbol switch
        {
            SourceMethodSymbol direct => direct,
            _ when methodSymbol.UnderlyingSymbol is SourceMethodSymbol underlying => underlying,
            _ => null
        };

        if (MethodGenerator.TypeGenerator.HasMethodGenerator(methodSymbol))
        {
            // The MethodGenerator was pre-created by DefineMembers. Closure assignment was
            // intentionally deferred so that DeclareLocals could create a single shared
            // DisplayClass. Assign it now if it hasn't been set yet.
            if (sourceMethod is not null)
            {
                var existingGen = MethodGenerator.TypeGenerator.GetMethodGenerator(methodSymbol);
                if (existingGen is not null && existingGen.LambdaClosure is null)
                {
                    var capturedVariables = sourceMethod.CapturedVariables;
                    if (capturedVariables.IsDefaultOrEmpty)
                        capturedVariables = Compilation.GetSemanticModel(localFunctionStmt.SyntaxTree).GetCapturedVariables(localFunctionStmt);

                    if (!capturedVariables.IsDefaultOrEmpty &&
                        !Compilation.IsEntryPointCandidate(sourceMethod) &&
                        !sourceMethod.IsImplicitlyDeclared)
                    {
                        var closure = MethodGenerator.TypeGenerator.EnsureMethodClosure(sourceMethod, capturedVariables);
                        existingGen.SetLambdaClosure(closure);
                    }
                }
            }
            return;
        }

        var methodGenerator = new MethodGenerator(MethodGenerator.TypeGenerator, methodSymbol, MethodGenerator.ILBuilderFactory);

        if (sourceMethod is not null)
        {
            var capturedVariables = sourceMethod.CapturedVariables;
            if (capturedVariables.IsDefaultOrEmpty)
                capturedVariables = Compilation.GetSemanticModel(localFunctionStmt.SyntaxTree).GetCapturedVariables(localFunctionStmt);

            if (!capturedVariables.IsDefaultOrEmpty &&
                !Compilation.IsEntryPointCandidate(sourceMethod) &&
                !sourceMethod.IsImplicitlyDeclared)
            {
                var closure = MethodGenerator.TypeGenerator.EnsureMethodClosure(sourceMethod, capturedVariables);
                methodGenerator.SetLambdaClosure(closure);
            }
        }

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

        // === SHARED CLOSURE HOISTING (reference-based capture) ===
        // On the very first call for an outer method (not itself a lambda or state machine),
        // scan for lambdas that capture outer locals and build a single shared closure so
        // that both the outer method and its lambdas read/write the same heap field.
        if (_lambdaClosure is null && _outerMethodClosure is null &&
            MethodSymbol is not ILambdaSymbol &&
            MethodSymbol.ContainingType is not SynthesizedAsyncStateMachineTypeSymbol &&
            MethodSymbol.ContainingType is not SynthesizedIteratorTypeSymbol)
        {
            var hoistCollector = new HoistedLocalsCollector();
            hoistCollector.Visit(block);

            if (hoistCollector.CapturedSymbols.Count > 0)
            {
                _hoistedSymbols = hoistCollector.CapturedSymbols;
                var allCaptured = hoistCollector.CapturedSymbols.ToImmutableArray();

                _outerMethodClosure = MethodGenerator.TypeGenerator.EnsureSharedMethodClosure(
                    MethodSymbol, allCaptured, hoistCollector.LambdaSymbols, hoistCollector.LocalFunctionSymbols);

                var runtimeClosureType = _outerMethodClosure.GetRuntimeType(MethodGenerator.TypeGenerator.CodeGen);
                var runtimeClosureCtor = runtimeClosureType == _outerMethodClosure.TypeBuilder
                    ? _outerMethodClosure.Constructor
                    : TypeBuilder.GetConstructor(runtimeClosureType, _outerMethodClosure.Constructor);

                // Allocate the shared closure at the very start of the method body.
                _outerMethodClosureLocal = ILGenerator.DeclareLocal(runtimeClosureType);
                ILGenerator.Emit(OpCodes.Newobj, runtimeClosureCtor);
                ILGenerator.Emit(OpCodes.Stloc, _outerMethodClosureLocal);

                // Eagerly copy non-local captures (parameters, self/'this') into the
                // closure so the lambda can read them even before any outer-method code runs.
                foreach (var captured in hoistCollector.CapturedSymbols)
                {
                    if (captured is ILocalSymbol)
                        continue; // initialized lazily by the hoisted assignment

                    if (!_outerMethodClosure.TryGetField(captured, out var initField))
                        continue;

                    ILGenerator.Emit(OpCodes.Ldloc, _outerMethodClosureLocal);
                    switch (captured)
                    {
                        case IParameterSymbol paramSymbol:
                            if (string.Equals(paramSymbol.Name, "self", StringComparison.Ordinal))
                            {
                                if (MethodSymbol.IsStatic)
                                    ILGenerator.Emit(OpCodes.Ldnull);
                                else
                                    ILGenerator.Emit(OpCodes.Ldarg_0);
                                break;
                            }

                            var paramBuilder = MethodGenerator.GetParameterBuilder(paramSymbol);
                            var position = paramBuilder.Position;
                            if (MethodSymbol.IsStatic)
                                position -= 1;
                            ILGenerator.Emit(OpCodes.Ldarg, position);
                            break;
                        case ITypeSymbol: // self / this
                            if (MethodSymbol.IsStatic)
                                ILGenerator.Emit(OpCodes.Ldnull);
                            else
                                ILGenerator.Emit(OpCodes.Ldarg_0);
                            break;
                    }
                    var runtimeField = runtimeClosureType == _outerMethodClosure.TypeBuilder
                        ? initField
                        : TypeBuilder.GetField(runtimeClosureType, initField);
                    ILGenerator.Emit(OpCodes.Stfld, runtimeField);
                }
            }
        }
        // === END SHARED CLOSURE HOISTING ===

        foreach (var localSymbol in collector.Locals)
        {
            if (localSymbol is SourceFunctionValueSymbol)
                continue;

            // Skip locals without a type. This can occur when the initializer
            // contains an early return, making the declaration unreachable.
            if (localSymbol.Type is null)
                continue;

            // Skip locals that have been hoisted into the shared closure: they are stored
            // as fields on the closure object, not as IL stack locals.
            if (_hoistedSymbols?.Contains(localSymbol) == true)
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

            var isPinned = collector.PinnedLocals.Contains(localSymbol);
            var builder = ILGenerator.DeclareLocal(clrType, isPinned);
            if (ShouldEmitDebugLocalName(localSymbol))
                builder.SetLocalSymInfo(localSymbol.Name);
            targetScope.AddLocal(localSymbol, builder);
        }
    }

    private static bool ShouldEmitDebugLocalName(ILocalSymbol localSymbol)
    {
        if (localSymbol.Name.StartsWith("<", StringComparison.Ordinal))
            return false;

        return localSymbol.Locations.Any(static location => location.IsInSource);
    }

    private void EmitMethodBlock(BoundBlockStatement block, bool includeImplicitReturn = true)
    {
        EmitMethodEntrySequencePointOnce();
        EmitBlock(block, treatAsMethodBody: true, includeImplicitReturn);
    }

    private void EmitBoundBlock(BoundBlockStatement block)
    {
        EmitBlock(block, treatAsMethodBody: false, includeImplicitReturn: false);
    }

    private void EmitMethodEntrySequencePointOnce()
    {
        if (_emittedMethodEntrySequencePoint)
            return;

        var syntax = GetMethodEntrySequencePointSyntax();
        if (syntax is null)
            return;

        _emittedMethodEntrySequencePoint = true;
        EmitHiddenEntrySequencePoint(syntax);
    }

    private void EmitHiddenEntrySequencePoint(SyntaxNode syntax)
    {
        if (syntax.SyntaxTree is null)
            return;

        var document = GetOrAddDocument(syntax.SyntaxTree);

        ILGenerator.Emit(OpCodes.Nop);
        ILGenerator.MarkSequencePoint(document, HiddenSequencePointLine, 0, HiddenSequencePointLine, 0);
    }

    private SyntaxNode? GetMethodEntrySequencePointSyntax()
    {
        static SyntaxNode? SelectBodySyntax(SyntaxNode? syntax)
        {
            static SyntaxNode SelectFirstStatementOr(SyntaxNode fallback, SyntaxList<StatementSyntax> statements)
                => statements.Count > 0 ? statements[0] : fallback;

            return syntax switch
            {
                MethodDeclarationSyntax method => method.Body is { } methodBody
                    ? SelectFirstStatementOr(methodBody, methodBody.Statements)
                    : (SyntaxNode?)method.ExpressionBody?.Expression ?? method,
                FunctionStatementSyntax function => function.Body is { } functionBody
                    ? SelectFirstStatementOr(functionBody, functionBody.Statements)
                    : (SyntaxNode?)function.ExpressionBody?.Expression ?? function,
                AccessorDeclarationSyntax accessor => (SyntaxNode?)accessor.Body ?? (SyntaxNode?)accessor.ExpressionBody?.Expression ?? accessor,
                BaseConstructorDeclarationSyntax ctor => (SyntaxNode?)ctor.Body ?? (SyntaxNode?)ctor.ExpressionBody?.Expression ?? ctor,
                ParameterlessConstructorDeclarationSyntax initDecl => (SyntaxNode?)initDecl.Body ?? (SyntaxNode?)initDecl.ExpressionBody?.Expression ?? initDecl,
                FinallyDeclarationSyntax finalDecl => finalDecl.Body,
                ArrowExpressionClauseSyntax arrow => arrow.Expression,
                FunctionExpressionSyntax lambda => (SyntaxNode?)lambda.Body ?? lambda.ExpressionBody?.Expression ?? lambda,
                CompilationUnitSyntax compilationUnit => GetTopLevelStatements(compilationUnit).FirstOrDefault() is { } topLevelStatement
                    ? topLevelStatement
                    : compilationUnit,
                _ => syntax
            };
        }

        var syntax = MethodSymbol.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax();
        if (syntax is not null)
            return SelectBodySyntax(syntax);

        if (MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol asyncStateMachine)
        {
            var asyncSyntax = asyncStateMachine.AsyncMethod.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax();
            if (asyncSyntax is not null)
                return SelectBodySyntax(asyncSyntax);
        }

        if (MethodSymbol.ContainingType is SynthesizedIteratorTypeSymbol iteratorType)
        {
            var iteratorSyntax = iteratorType.IteratorMethod.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax();
            if (iteratorSyntax is not null)
                return SelectBodySyntax(iteratorSyntax);
        }

        return null;
    }

    private void EmitBlock(BoundBlockStatement block, bool treatAsMethodBody, bool includeImplicitReturn)
    {
        var statements = block.Statements as IReadOnlyList<BoundStatement> ?? block.Statements.ToArray();
        var blockScope = new Scope(scope, block.LocalsToDispose);
        var emitExplicitIlScope = !treatAsMethodBody && block.IntroduceILScope;

        if (emitExplicitIlScope)
            ILGenerator.BeginScope();

        try
        {
            // Locals synthesized during lowering (e.g., iterator state machines) won't
            // be present in the original bound body we used for the initial declaration
            // pass. Ensure we register builders for any newly introduced locals so
            // downstream emitters can load and store them.
            DeclareLocals(blockScope, block);

            var effectiveReturnType = GetEffectiveReturnTypeForEmission();
            var unitType = Compilation.GetSpecialType(SpecialType.System_Unit);

            for (var i = 0; i < statements.Count; i++)
            {
                var statement = statements[i];

                // If this is the last statement in the block and the method expects a
                // value, treat a bare expression statement as an implicit return. This
                // allows functions to omit an explicit `return` for the final
                // expression, while still emitting any required boxing.
                if (treatAsMethodBody && includeImplicitReturn &&
                    i == statements.Count - 1 &&
                    ImplicitReturnRewriter.IsImplicitReturnCandidate(effectiveReturnType, unitType, block, out var implicitReturnStmt))
                {
                    var returnStatement = new BoundReturnStatement(implicitReturnStmt.Expression);
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
                IsTerminatingStatement(statements[^1]);

            if (!endsWithTerminator && ShouldEmitImplicitReturn())
            {
                ILGenerator.Emit(OpCodes.Nop);
                ILGenerator.Emit(OpCodes.Ret);
                return;
            }

            if (!endsWithTerminator &&
                GetEffectiveReturnTypeForEmission().SpecialType is not SpecialType.System_Void and not SpecialType.System_Unit)
            {
                // Defensive fallback: keep emitted IL verifiable even when branch-shape analysis
                // cannot prove all paths in a non-void method return.
                var invalidOperationCtor = typeof(InvalidOperationException)
                    .GetConstructor(new[] { typeof(string) })!;
                ILGenerator.Emit(OpCodes.Ldstr, "Control reached end of non-void member without returning a value.");
                ILGenerator.Emit(OpCodes.Newobj, invalidOperationCtor);
                ILGenerator.Emit(OpCodes.Throw);
            }
        }
        finally
        {
            if (emitExplicitIlScope)
                ILGenerator.EndScope();
        }
    }

    private static bool IsTerminatingStatement(BoundStatement statement)
    {
        return statement switch
        {
            BoundReturnStatement => true,
            BoundThrowStatement => true,
            BoundExpressionStatement { Expression: BoundReturnExpression or BoundThrowExpression } => true,
            BoundBlockStatement block when block.Statements.Any() => IsTerminatingStatement(block.Statements.Last()),
            BoundIfStatement { ElseNode: not null } ifStatement =>
                IsTerminatingStatement(ifStatement.ThenNode) &&
                IsTerminatingStatement(ifStatement.ElseNode!),
            _ => false,
        };
    }

    private sealed class LocalCollector : Raven.CodeAnalysis.BoundTreeWalker
    {
        private readonly ISymbol _containingSymbol;

        public LocalCollector(ISymbol containingSymbol)
        {
            _containingSymbol = containingSymbol;
        }

        public List<ILocalSymbol> Locals { get; } = new();
        public HashSet<ILocalSymbol> PinnedLocals { get; } = new(SymbolEqualityComparer.Default);

        public override void VisitLocalAccess(BoundLocalAccess node)
        {
            if (!Locals.Any(l => SymbolEqualityComparer.Default.Equals(l, node.Local)))
                Locals.Add(node.Local);

            base.VisitLocalAccess(node);
        }

        public override void VisitLocalDeclarationStatement(BoundLocalDeclarationStatement node)
        {
            base.VisitLocalDeclarationStatement(node);
        }

        public override void VisitVariableDeclarator(BoundVariableDeclarator node)
        {
            Locals.Add(node.Local);

            if (node.FixedPinnedLocal is not null)
            {
                Locals.Add(node.FixedPinnedLocal);
                PinnedLocals.Add(node.FixedPinnedLocal);
            }

            base.VisitVariableDeclarator(node);
        }

        public override void VisitAssignmentStatement(BoundAssignmentStatement node)
        {
            if (node.Expression is BoundPatternAssignmentExpression patternAssignment &&
                patternAssignment.Pattern is { } pattern)
            {
                AddPatternDesignatorLocals(pattern);
            }

            base.VisitAssignmentStatement(node);
        }

        public override void VisitExpressionStatement(BoundExpressionStatement node)
        {
            if (node.Expression is BoundPatternAssignmentExpression { Pattern: { } pattern })
                AddPatternDesignatorLocals(pattern);

            base.VisitExpressionStatement(node);
        }

        public override void VisitIfStatement(BoundIfStatement node)
        {
            if (node.Condition is BoundIsPatternExpression { Pattern: { } pattern })
                AddPatternDesignatorLocals(pattern);

            base.VisitIfStatement(node);
        }

        public override void VisitConditionalGotoStatement(BoundConditionalGotoStatement node)
        {
            if (node.Condition is BoundIsPatternExpression { Pattern: { } pattern })
                AddPatternDesignatorLocals(pattern);

            base.VisitConditionalGotoStatement(node);
        }

        private void AddPatternDesignatorLocals(BoundPattern pattern)
        {
            foreach (var designator in EnumerateDesignators(pattern))
            {
                if (designator is BoundSingleVariableDesignator single &&
                    !Locals.Any(l => SymbolEqualityComparer.Default.Equals(l, single.Local)))
                {
                    Locals.Add(single.Local);
                }
            }
        }

        private static IEnumerable<BoundDesignator> EnumerateDesignators(BoundPattern pattern)
        {
            switch (pattern)
            {
                case BoundNotPattern notPattern:
                    foreach (var designator in EnumerateDesignators(notPattern.Pattern))
                        yield return designator;
                    yield break;

                case BoundAndPattern andPattern:
                    foreach (var designator in EnumerateDesignators(andPattern.Left))
                        yield return designator;
                    foreach (var designator in EnumerateDesignators(andPattern.Right))
                        yield return designator;
                    yield break;

                case BoundOrPattern orPattern:
                    foreach (var designator in EnumerateDesignators(orPattern.Left))
                        yield return designator;
                    foreach (var designator in EnumerateDesignators(orPattern.Right))
                        yield return designator;
                    yield break;
            }

            foreach (var designator in pattern.GetDesignators())
                yield return designator;
        }

    }

    /// <summary>
    /// Collects all symbols captured by immediately-nested lambdas without recursing into
    /// nested lambda bodies. Used to determine which outer-method variables should be
    /// hoisted into a shared closure for reference-based capture semantics.
    /// </summary>
    private sealed class HoistedLocalsCollector : BoundTreeWalker
    {
        public HashSet<ISymbol> CapturedSymbols { get; } = new(SymbolEqualityComparer.Default);
        public List<ILambdaSymbol> LambdaSymbols { get; } = new();
        public List<SourceMethodSymbol> LocalFunctionSymbols { get; } = new();

        public override void VisitFunctionExpression(BoundFunctionExpression node)
        {
            if (node.Symbol is ILambdaSymbol lambdaSymbol)
                LambdaSymbols.Add(lambdaSymbol);

            foreach (var captured in node.CapturedVariables)
            {
                if (captured is not null)
                    CapturedSymbols.Add(captured);
            }

            // Intentionally do NOT recurse into the lambda body: its locals belong to a
            // separate generated method and must not be hoisted into the outer method's closure.
        }

        public override void VisitFunctionStatement(BoundFunctionStatement node)
        {
            if (node.Method is SourceMethodSymbol sourceMethod)
            {
                LocalFunctionSymbols.Add(sourceMethod);
                foreach (var captured in sourceMethod.CapturedVariables)
                {
                    if (captured is not null)
                        CapturedSymbols.Add(captured);
                }
            }

            // Intentionally do NOT recurse into the local function body.
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
            var boundNode = semanticModel.GetBoundNode(statement, BoundTreeView.Lowered) as BoundStatement;

            if (boundNode is null)
                continue;

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
        var returnType = GetEffectiveReturnTypeForEmission();
        if (returnType is null)
            return true;

        return returnType.SpecialType is SpecialType.System_Void or SpecialType.System_Unit;
    }

    private ITypeSymbol GetEffectiveReturnTypeForEmission()
    {
        if (!Compilation.Options.UseRuntimeAsync || !MethodSymbol.IsAsync)
            return MethodSymbol.ReturnType;

        return AsyncReturnTypeUtilities.ExtractAsyncResultType(Compilation, MethodSymbol.ReturnType)
            ?? MethodSymbol.ReturnType;
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
        if (MethodSymbol.ContainingType is INamedTypeSymbol containingType)
        {
            if (containingType is SynthesizedAsyncStateMachineTypeSymbol asyncStateMachine)
            {
                typeSymbol = RemapMethodTypeParametersToContainingType(typeSymbol, asyncStateMachine);
            }
            else if (containingType is ConstructedNamedTypeSymbol { ConstructedFrom: SynthesizedAsyncStateMachineTypeSymbol constructedAsyncStateMachine })
            {
                typeSymbol = RemapMethodTypeParametersToContainingType(typeSymbol, containingType);
            }
            else if (typeSymbol is ITypeParameterSymbol { OwnerKind: TypeParameterOwnerKind.Method } methodTypeParameter &&
                     (containingType is SynthesizedIteratorTypeSymbol
                      || (containingType is ConstructedNamedTypeSymbol constructedContaining &&
                          constructedContaining.ConstructedFrom is SynthesizedIteratorTypeSymbol)))
            {
                if (containingType is ConstructedNamedTypeSymbol constructed &&
                    (uint)methodTypeParameter.Ordinal < (uint)constructed.TypeArguments.Length)
                {
                    typeSymbol = constructed.TypeArguments[methodTypeParameter.Ordinal];
                }
                else if ((uint)methodTypeParameter.Ordinal < (uint)containingType.TypeParameters.Length)
                {
                    typeSymbol = containingType.TypeParameters[methodTypeParameter.Ordinal];
                }
            }
        }

        return TypeSymbolExtensionsForCodeGen.GetClrType(typeSymbol, MethodGenerator.TypeGenerator.CodeGen);
    }

    private ITypeSymbol RemapMethodTypeParametersToContainingType(ITypeSymbol typeSymbol, INamedTypeSymbol containingType)
    {
        return Rewrite(typeSymbol);

        ITypeSymbol Rewrite(ITypeSymbol symbol)
        {
            if (symbol is ITypeParameterSymbol { OwnerKind: TypeParameterOwnerKind.Method } methodTypeParameter)
            {
                if (containingType is ConstructedNamedTypeSymbol constructedContaining &&
                    (uint)methodTypeParameter.Ordinal < (uint)constructedContaining.TypeArguments.Length)
                {
                    return constructedContaining.TypeArguments[methodTypeParameter.Ordinal];
                }

                if ((uint)methodTypeParameter.Ordinal < (uint)containingType.TypeParameters.Length)
                    return containingType.TypeParameters[methodTypeParameter.Ordinal];

                return symbol;
            }

            if (symbol is RefTypeSymbol refType)
            {
                var elementType = Rewrite(refType.ElementType);
                return SymbolEqualityComparer.Default.Equals(elementType, refType.ElementType)
                    ? symbol
                    : new RefTypeSymbol(elementType);
            }

            if (symbol is IAddressTypeSymbol addressType)
            {
                var referencedType = Rewrite(addressType.ReferencedType);
                return SymbolEqualityComparer.Default.Equals(referencedType, addressType.ReferencedType)
                    ? symbol
                    : new AddressTypeSymbol(referencedType);
            }

            if (symbol is IArrayTypeSymbol arrayType)
            {
                var elementType = Rewrite(arrayType.ElementType);
                return SymbolEqualityComparer.Default.Equals(elementType, arrayType.ElementType)
                    ? symbol
                    : Compilation.CreateArrayTypeSymbol(elementType, arrayType.Rank, arrayType.FixedLength);
            }

            if (symbol is IPointerTypeSymbol pointerType)
            {
                var pointedAtType = Rewrite(pointerType.PointedAtType);
                return SymbolEqualityComparer.Default.Equals(pointedAtType, pointerType.PointedAtType)
                    ? symbol
                    : Compilation.CreatePointerTypeSymbol(pointedAtType);
            }

            if (symbol is INamedTypeSymbol namedType && namedType.IsGenericType && !namedType.IsUnboundGenericType)
            {
                var typeArguments = namedType.TypeArguments;
                var rewrittenArguments = new ITypeSymbol[typeArguments.Length];
                var changed = false;

                for (var i = 0; i < typeArguments.Length; i++)
                {
                    rewrittenArguments[i] = Rewrite(typeArguments[i]);
                    if (!SymbolEqualityComparer.Default.Equals(rewrittenArguments[i], typeArguments[i]))
                        changed = true;
                }

                if (!changed)
                    return symbol;

                var definition = namedType.ConstructedFrom as INamedTypeSymbol ?? namedType;
                return definition.Construct(rewrittenArguments);
            }

            return symbol;
        }
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
        return Generator.InstantiateType(ResolveClrType(caseTypeSymbol));
    }

}

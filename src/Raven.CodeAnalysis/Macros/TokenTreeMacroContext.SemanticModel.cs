using System;
using System.Collections.Immutable;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

public partial class TokenTreeMacroContext
{
    public TokenTreeMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroExpressionSyntax syntax,
        CancellationToken cancellationToken = default)
        : this(
            compilation,
            semanticModel,
            FreestandingMacroInvocation.Create(syntax),
            tokenStreamProvider: null,
            keywords: ImmutableArray<MacroKeyword>.Empty,
            cancellationToken)
    {
    }

    public TokenTreeMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroMemberDeclarationSyntax syntax,
        CancellationToken cancellationToken = default)
        : this(
            compilation,
            semanticModel,
            FreestandingMacroInvocation.Create(syntax),
            tokenStreamProvider: null,
            keywords: ImmutableArray<MacroKeyword>.Empty,
            cancellationToken)
    {
    }

    public TokenTreeMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroDeclarationSyntax syntax,
        CancellationToken cancellationToken = default)
        : this(
            compilation,
            semanticModel,
            FreestandingMacroInvocation.Create(syntax),
            tokenStreamProvider: null,
            keywords: ImmutableArray<MacroKeyword>.Empty,
            cancellationToken)
    {
    }

    internal TokenTreeMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroExpressionSyntax syntax,
        IMacroDefinition macro,
        CancellationToken cancellationToken = default)
        : this(
            compilation,
            semanticModel,
            FreestandingMacroInvocation.Create(syntax),
            macro as IMacroTokenStreamProvider,
            macro is IMacroKeywordProvider keywordProvider
                ? keywordProvider.Keywords
                : ImmutableArray<MacroKeyword>.Empty,
            cancellationToken)
    {
        ArgumentNullException.ThrowIfNull(macro);
    }

    internal TokenTreeMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroInvocation invocation,
        IMacroDefinition macro,
        CancellationToken cancellationToken = default)
        : this(
            compilation,
            semanticModel,
            invocation,
            macro as IMacroTokenStreamProvider,
            macro is IMacroKeywordProvider keywordProvider
                ? keywordProvider.Keywords
                : ImmutableArray<MacroKeyword>.Empty,
            cancellationToken)
    {
        ArgumentNullException.ThrowIfNull(macro);
    }

    private TokenTreeMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroInvocation invocation,
        IMacroTokenStreamProvider? tokenStreamProvider,
        ImmutableArray<MacroKeyword> keywords,
        CancellationToken cancellationToken)
        : this(CreateServices(compilation, semanticModel, invocation), invocation, tokenStreamProvider, keywords, cancellationToken)
    {
    }

    public Compilation Compilation => HostServices.Compilation;

    public SemanticModel SemanticModel => HostServices.SemanticModel;

    private SemanticModelFragmentServices HostServices => _services as SemanticModelFragmentServices
        ?? throw new InvalidOperationException("This internal context has no semantic-model host adapter.");

    private static MacroFragmentServices CreateServices(Compilation compilation, SemanticModel semanticModel, FreestandingMacroInvocation invocation)
    {
        ArgumentNullException.ThrowIfNull(invocation.Syntax, "invocationSyntax");
        ArgumentNullException.ThrowIfNull(compilation);
        ArgumentNullException.ThrowIfNull(semanticModel);
        return new SemanticModelFragmentServices(compilation, semanticModel, MacroArgument.CreateServices(invocation.Syntax, semanticModel));
    }

    private sealed class SemanticModelFragmentServices(Compilation compilation, SemanticModel semanticModel,
        MacroArgumentServices argumentServices) : MacroFragmentServices
    {
        public Compilation Compilation => compilation;
        public SemanticModel SemanticModel => semanticModel;
        public MacroArgumentServices ArgumentServices => argumentServices;
        public TypeSymbol ErrorType => compilation.ErrorTypeSymbol;
        public TypeSymbol CharacterType => ((SequenceTypeServices)compilation).CharacterType;
        public NamedTypeSymbol? GetSequenceDefinition(bool asynchronous)
            => ((SequenceTypeServices)compilation).GetSequenceDefinition(asynchronous);
        public TypeSymbol? GetExpressionType(SyntaxNode invocation, ExpressionSyntax expression)
            => semanticModel.GetMacroFragmentExpressionType(invocation, expression);
        public TypeInfo GetTypeInfo(SyntaxNode invocation, ExpressionSyntax expression)
            => semanticModel.GetMacroFragmentTypeInfo(invocation, expression);
        public SymbolInfo GetSymbolInfo(SyntaxNode invocation, ExpressionSyntax expression)
            => semanticModel.GetMacroFragmentSymbolInfo(invocation, expression);
    }
}

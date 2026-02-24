using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Diagnostics;

public static class BuiltInAnalyzers
{
    public static Project AddBuiltInAnalyzers(this Project project, bool enableSuggestions = false)
    {
        project = project
            .AddAnalyzerReference(new AnalyzerReference(new MissingReturnTypeAnnotationAnalyzer()))
            .AddAnalyzerReference(new AnalyzerReference(new EventDelegateMustBeNullableAnalyzer()))
            .AddAnalyzerReference(new AnalyzerReference(new NonNullDeclarationsAnalyzer()))
            .AddAnalyzerReference(new AnalyzerReference(new VarCanBeValAnalyzer()))
            .AddAnalyzerReference(new AnalyzerReference(new MatchExhaustivenessAnalyzer()))
            .AddAnalyzerReference(new AnalyzerReference(new PreferValInsteadOfLetAnalyzer()))
            .AddAnalyzerReference(new AnalyzerReference(new AutoPropertyInitializationAnalyzer()))
            .AddAnalyzerReference(new AnalyzerReference(new PreferNewLineBetweenDeclarationsAnalyzer()))
            .AddAnalyzerReference(new AnalyzerReference(new ThrowStatementUseResultAnalyzer()))
            .AddAnalyzerReference(new AnalyzerReference(new MemberCanBePrivateAnalyzer()))
            .AddAnalyzerReference(new AnalyzerReference(new MemberCanBeStaticAnalyzer()))
            .AddAnalyzerReference(new AnalyzerReference(new PreferDuLinqExtensionsAnalyzer()));

        if (!enableSuggestions)
            return project;

        return project
            .AddAnalyzerReference(new AnalyzerReference(new PreferTargetTypedUnionCaseAnalyzer()))
            .AddAnalyzerReference(new AnalyzerReference(new PreferTargetTypedUnionCaseInTargetTypedContextAnalyzer()));
    }
}

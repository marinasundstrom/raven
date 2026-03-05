using System.Linq;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Diagnostics;

public static class BuiltInAnalyzers
{
    public static Project AddBuiltInAnalyzers(this Project project, bool enableSuggestions = false)
    {
        project = AddAnalyzerIfMissing<MissingReturnTypeAnnotationAnalyzer>(project);
        project = AddAnalyzerIfMissing<EventDelegateMustBeNullableAnalyzer>(project);
        project = AddAnalyzerIfMissing<NonNullDeclarationsAnalyzer>(project);
        project = AddAnalyzerIfMissing<VarCanBeValAnalyzer>(project);
        project = AddAnalyzerIfMissing<MatchExhaustivenessAnalyzer>(project);
        project = AddAnalyzerIfMissing<PreferValInsteadOfLetAnalyzer>(project);
        project = AddAnalyzerIfMissing<AutoPropertyInitializationAnalyzer>(project);
        project = AddAnalyzerIfMissing<PreferNewLineBetweenDeclarationsAnalyzer>(project);
        project = AddAnalyzerIfMissing<ThrowStatementUseResultAnalyzer>(project);
        project = AddAnalyzerIfMissing<MemberCanBePrivateAnalyzer>(project);
        project = AddAnalyzerIfMissing<MemberCanBeStaticAnalyzer>(project);
        project = AddAnalyzerIfMissing<UnusedPropertyAnalyzer>(project);
        project = AddAnalyzerIfMissing<UnusedMethodAnalyzer>(project);
        project = AddAnalyzerIfMissing<PreferDuLinqExtensionsAnalyzer>(project);
        project = AddAnalyzerIfMissing<RedundantAccessorDeclarationAnalyzer>(project);
        project = AddAnalyzerIfMissing<StringConcatenationAnalyzer>(project);
        project = AddAnalyzerIfMissing<ConstructorParameterNamingAnalyzer>(project);

        if (!enableSuggestions)
            return project;

        project = AddAnalyzerIfMissing<PreferTargetTypedUnionCaseAnalyzer>(project);
        return AddAnalyzerIfMissing<PreferTargetTypedUnionCaseInTargetTypedContextAnalyzer>(project);
    }

    private static Project AddAnalyzerIfMissing<TAnalyzer>(Project project)
        where TAnalyzer : DiagnosticAnalyzer, new()
    {
        var exists = project.AnalyzerReferences
            .SelectMany(static reference => reference.GetAnalyzers())
            .Any(analyzer => analyzer is TAnalyzer);

        return exists
            ? project
            : project.AddAnalyzerReference(new AnalyzerReference(new TAnalyzer()));
    }
}

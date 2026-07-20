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
        project = AddAnalyzerIfMissing<UninitializedPropertyAnalyzer>(project);
        project = AddAnalyzerIfMissing<UninitializedFieldAnalyzer>(project);
        project = AddAnalyzerIfMissing<PreferNewLineBetweenDeclarationsAnalyzer>(project);
        project = AddAnalyzerIfMissing<ThrowStatementUseResultAnalyzer>(project);
        project = AddAnalyzerIfMissing<MemberCanBePrivateAnalyzer>(project);
        project = AddAnalyzerIfMissing<MemberCanBeStaticAnalyzer>(project);
        project = AddAnalyzerIfMissing<UnusedPropertyAnalyzer>(project);
        project = AddAnalyzerIfMissing<UnusedMethodAnalyzer>(project);
        project = AddAnalyzerIfMissing<UnusedLocalAnalyzer>(project);
        project = AddAnalyzerIfMissing<UnusedParameterAnalyzer>(project);
        project = AddAnalyzerIfMissing<UnusedImportDirectiveAnalyzer>(project);
        project = AddAnalyzerIfMissing<UnusedExpressionResultAnalyzer>(project);
        project = AddAnalyzerIfMissing<UnhandledMemberReturnValueAnalyzer>(project);
        project = AddAnalyzerIfMissing<DisposableObjectAnalyzer>(project);
        project = AddAnalyzerIfMissing<ImmutableCollectionOperationResultAnalyzer>(project);
        project = AddAnalyzerIfMissing<PreferDuLinqExtensionsAnalyzer>(project);
        project = AddAnalyzerIfMissing<ConstructorParameterNamingAnalyzer>(project);
        project = AddAnalyzerIfMissing<UnnecessaryTrailingSeparatorAnalyzer>(project);

        if (!enableSuggestions)
            return project;

        return project;
    }

    private static Project AddAnalyzerIfMissing<TAnalyzer>(Project project)
        where TAnalyzer : DiagnosticAnalyzer, new()
    {
        if (AnalyzerOptionUtilities.IsAnalyzerDisabled(typeof(TAnalyzer), project.CompilationOptions?.DisabledAnalyzers ?? []))
            return project;

        var exists = project.AnalyzerReferences
            .SelectMany(static reference => reference.GetAnalyzers())
            .Any(analyzer => analyzer is TAnalyzer);

        return exists
            ? project
            : project.AddAnalyzerReference(new AnalyzerReference(new TAnalyzer()));
    }
}

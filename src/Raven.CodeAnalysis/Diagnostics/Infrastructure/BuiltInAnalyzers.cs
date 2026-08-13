using System.Linq;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Diagnostics;

public static class BuiltInAnalyzers
{
    private const string TypingCategory = "typing";
    private const string InitializationCategory = "initialization";
    private const string ImmutabilityCategory = "immutability";
    private const string UsageCategory = "usage";
    private const string ErrorHandlingCategory = "errorhandling";
    private const string DesignCategory = "design";

    public static Project AddBuiltInAnalyzers(this Project project, bool enableSuggestions = false)
    {
        project = AddTypingAnalyzers(project);
        project = AddInitializationAnalyzers(project);
        project = AddImmutabilityAnalyzers(project);
        project = AddUsageAnalyzers(project);
        project = AddErrorHandlingAnalyzers(project);
        project = AddDesignAnalyzers(project);

        if (!enableSuggestions)
            return project;

        return project;
    }

    private static Project AddTypingAnalyzers(Project project)
    {
        project = AddOptionalAnalyzerIfEnabled<MissingReturnTypeAnnotationAnalyzer>(project, TypingCategory);
        project = AddAnalyzerIfMissing<EventDelegateMustBeNullableAnalyzer>(project);
        project = AddOptionalAnalyzerIfEnabled<VarCanBeLetAnalyzer>(project, TypingCategory);
        project = AddOptionalAnalyzerIfEnabled<NonNullDeclarationsAnalyzer>(project, TypingCategory);
        project = AddAnalyzerIfMissing<PreferIsNullOverEqualityAnalyzer>(project);

        return project;
    }

    private static Project AddInitializationAnalyzers(Project project)
    {
        project = AddAnalyzerIfMissing<UninitializedPropertyAnalyzer>(project);
        project = AddAnalyzerIfMissing<UninitializedFieldAnalyzer>(project);

        return project;
    }

    private static Project AddImmutabilityAnalyzers(Project project)
    {
        project = AddAnalyzerIfMissing<ImmutableCollectionOperationResultAnalyzer>(project);

        return project;
    }

    private static Project AddUsageAnalyzers(Project project)
    {
        project = AddOptionalAnalyzerIfEnabled<UnusedPropertyAnalyzer>(project, UsageCategory);
        project = AddOptionalAnalyzerIfEnabled<UnusedMethodAnalyzer>(project, UsageCategory);
        project = AddAnalyzerIfMissing<UnusedLocalAnalyzer>(project);
        project = AddOptionalAnalyzerIfEnabled<UnusedParameterAnalyzer>(project, UsageCategory);
        project = AddAnalyzerIfMissing<UnusedImportDirectiveAnalyzer>(project);
        project = AddAnalyzerIfMissing<DisposableObjectAnalyzer>(project);
        project = AddAnalyzerIfMissing<UnusedExpressionResultAnalyzer>(project);

        return project;
    }

    private static Project AddErrorHandlingAnalyzers(Project project)
    {
        project = AddOptionalAnalyzerIfEnabled<ThrowStatementUseResultAnalyzer>(project, ErrorHandlingCategory);
        project = AddOptionalAnalyzerIfEnabled<PreferDuLinqExtensionsAnalyzer>(project, ErrorHandlingCategory);

        return project;
    }

    private static Project AddDesignAnalyzers(Project project)
    {
        project = AddOptionalAnalyzerIfEnabled<MemberCanBePrivateAnalyzer>(project, DesignCategory);
        project = AddOptionalAnalyzerIfEnabled<MemberCanBeStaticAnalyzer>(project, DesignCategory);

        return project;
    }

    private static Project AddOptionalAnalyzerIfEnabled<TAnalyzer>(
        Project project,
        string category)
        where TAnalyzer : DiagnosticAnalyzer, new()
    {
        var options = project.CompilationOptions;
        if (!AnalyzerOptionUtilities.IsAnalyzerEnabled(typeof(TAnalyzer), options?.EnabledAnalyzers ?? [], category))
        {
            return project;
        }

        return AddAnalyzerIfMissing<TAnalyzer>(project);
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

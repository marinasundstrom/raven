using System.Reflection;

using Raven.CodeAnalysis.Diagnostics;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class AnalyzerDiagnosticIdUniquenessTests
{
    [Fact]
    public void BuiltInAnalyzers_UseUniqueDiagnosticIds_AndDoNotOverrideCompilerDiagnostics()
    {
        var analyzerAssembly = typeof(DiagnosticAnalyzer).Assembly;
        var analyzerTypes = analyzerAssembly.GetTypes()
            .Where(t => typeof(DiagnosticAnalyzer).IsAssignableFrom(t) && !t.IsAbstract)
            .ToArray();

        var ids = new List<(string AnalyzerName, string Id)>();

        foreach (var analyzerType in analyzerTypes)
        {
            var descriptorFields = analyzerType.GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static)
                .Where(f => typeof(DiagnosticDescriptor).IsAssignableFrom(f.FieldType));

            foreach (var field in descriptorFields)
            {
                if (field.GetValue(null) is not DiagnosticDescriptor descriptor)
                    continue;

                ids.Add((analyzerType.FullName ?? analyzerType.Name, descriptor.Id));
            }
        }

        ids.Count.ShouldBeGreaterThan(0);

        var duplicateIds = ids
            .GroupBy(x => x.Id, StringComparer.Ordinal)
            .Where(g => g.Count() > 1)
            .Select(g => g.Key)
            .ToArray();

        duplicateIds.ShouldBeEmpty();

        var overriddenCompilerIds = ids
            .Where(x => CompilerDiagnostics.IsDiagnosticDefined(x.Id))
            .Select(x => x.Id)
            .Distinct(StringComparer.Ordinal)
            .ToArray();

        overriddenCompilerIds.ShouldBeEmpty();
    }
}

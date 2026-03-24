using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using System.Collections.Immutable;
using Raven.LanguageServer;

using CodeDiagnostic = Raven.CodeAnalysis.Diagnostic;
using CodeDiagnosticSeverity = Raven.CodeAnalysis.DiagnosticSeverity;
using CodeLocation = Raven.CodeAnalysis.Location;

namespace Raven.Editor.Tests;

public class LanguageServerDiagnosticTagTests
{
    [Fact]
    public void MapTags_UnreachableCode_ReturnsUnnecessaryTag()
    {
        var descriptor = DiagnosticDescriptor.Create(
            "RAV0162",
            "Unreachable code",
            null,
            string.Empty,
            "Unreachable code",
            "Compiler",
            CodeDiagnosticSeverity.Warning);
        var diagnostic = CodeDiagnostic.Create(descriptor, CodeLocation.None);

        var tags = DocumentStore.MapTags(diagnostic);

        tags.ShouldNotBeNull();
        tags!.ShouldContain(DiagnosticTag.Unnecessary);
    }

    [Fact]
    public void MapTags_UnusedVariable_ReturnsUnnecessaryTag()
    {
        var descriptor = DiagnosticDescriptor.Create(
            Raven.CodeAnalysis.Diagnostics.UnusedVariableAnalyzer.DiagnosticId,
            "Variable is never used",
            null,
            string.Empty,
            "Variable '{0}' is never used.",
            "Usage",
            CodeDiagnosticSeverity.Warning);
        var diagnostic = CodeDiagnostic.Create(descriptor, CodeLocation.None, "count");

        var tags = DocumentStore.MapTags(diagnostic);

        tags.ShouldNotBeNull();
        tags!.ShouldContain(DiagnosticTag.Unnecessary);
    }

    [Fact]
    public void MapTags_LocalFunctionDiagnostic_ReturnsUnnecessaryTag()
    {
        var descriptor = DiagnosticDescriptor.Create(
            Raven.CodeAnalysis.Diagnostics.UnusedMethodAnalyzer.DiagnosticId,
            "Method is never invoked",
            null,
            string.Empty,
            "Method '{0}' is never invoked.",
            "Usage",
            CodeDiagnosticSeverity.Warning);
        var diagnostic = new CodeDiagnostic(
            descriptor,
            CodeLocation.None,
            ["Helper"],
            properties: ImmutableDictionary<string, string?>.Empty.Add(
                Raven.CodeAnalysis.Diagnostics.UnusedMethodAnalyzer.UnnecessaryDiagnosticProperty,
                bool.TrueString));

        var tags = DocumentStore.MapTags(diagnostic);

        tags.ShouldNotBeNull();
        tags!.ShouldContain(DiagnosticTag.Unnecessary);
    }

    [Fact]
    public void MapTags_OtherDiagnostic_ReturnsNull()
    {
        var descriptor = DiagnosticDescriptor.Create(
            "RAV0103",
            "Name not found",
            null,
            string.Empty,
            "'{0}' is not in scope.",
            "Compiler",
            CodeDiagnosticSeverity.Error);
        var diagnostic = CodeDiagnostic.Create(descriptor, CodeLocation.None, "missing");

        var tags = DocumentStore.MapTags(diagnostic);

        tags.ShouldBeNull();
    }

    [Fact]
    public void MapTags_MemberUnusedMethodDiagnostic_ReturnsNull()
    {
        var descriptor = DiagnosticDescriptor.Create(
            Raven.CodeAnalysis.Diagnostics.UnusedMethodAnalyzer.DiagnosticId,
            "Method is never invoked",
            null,
            string.Empty,
            "Method '{0}' is never invoked.",
            "Usage",
            CodeDiagnosticSeverity.Warning);
        var diagnostic = CodeDiagnostic.Create(descriptor, CodeLocation.None, "Helper");

        var tags = DocumentStore.MapTags(diagnostic);

        tags.ShouldBeNull();
    }
}

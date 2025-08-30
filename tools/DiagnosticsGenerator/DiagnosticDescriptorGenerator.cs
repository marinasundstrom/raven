using System.Text;

namespace Raven.Generators
{
    internal static class DiagnosticDescriptorGenerator
    {
        public static string GenerateCompilerDiagnostics(List<DiagnosticDescriptorModel> diagnostics)
        {
            var sb = new StringBuilder();
            sb.AppendLine("namespace Raven.CodeAnalysis;");
        sb.AppendLine();
        sb.AppendLine("internal static partial class CompilerDiagnostics");
        sb.AppendLine("{");
        sb.AppendLine("    private static DiagnosticDescriptor[]? _allDescriptors;");
        foreach (var d in diagnostics)
        {
            var fieldName = "_" + char.ToLowerInvariant(d.Identifier[0]) + d.Identifier[1..];
            sb.AppendLine($"    private static DiagnosticDescriptor? {fieldName};");
        }
        sb.AppendLine();
        foreach (var d in diagnostics)
        {
            var fieldName = "_" + char.ToLowerInvariant(d.Identifier[0]) + d.Identifier[1..];
            sb.AppendLine("    /// <summary>");
            sb.AppendLine($"    /// {d.Id}: {d.Message}");
            sb.AppendLine("    /// </summary>");
            sb.AppendLine($"    public static DiagnosticDescriptor {d.Identifier} => {fieldName} ??= DiagnosticDescriptor.Create(");
            sb.AppendLine($"        id: \"{d.Id}\",");
            sb.AppendLine($"        title: \"{d.Title}\",");
            sb.AppendLine($"        description: \"{d.Description}\",");
            sb.AppendLine($"        helpLinkUri: \"{d.HelpLinkUri}\",");
            sb.AppendLine($"        messageFormat: \"{d.Message}\",");
            sb.AppendLine($"        category: \"{d.Category}\",");
            sb.AppendLine($"        DiagnosticSeverity.{d.Severity},");
            sb.AppendLine($"        isEnabledByDefault: {d.EnabledByDefault.ToString().ToLowerInvariant()});");
            sb.AppendLine();
        }
        sb.AppendLine("    public static DiagnosticDescriptor[] AllDescriptors => _allDescriptors ??=");
        sb.AppendLine("    [");
        foreach (var d in diagnostics)
        {
            sb.AppendLine($"        {d.Identifier},");
        }
        sb.AppendLine("    ];");
        sb.AppendLine();
        sb.AppendLine("    public static DiagnosticDescriptor? GetDescriptor(string diagnosticId) => diagnosticId switch");
        sb.AppendLine("    {");
        foreach (var d in diagnostics)
        {
            sb.AppendLine($"        \"{d.Id}\" => {d.Identifier},");
        }
        sb.AppendLine("        _ => null");
        sb.AppendLine("    };");
        sb.AppendLine("}");
            return sb.ToString();
        }
    }

    internal record DiagnosticDescriptorModel(
        string Id,
        string Identifier,
        string Title,
        string Message,
        string Category,
        string Severity,
        bool EnabledByDefault,
        string Description,
        string HelpLinkUri);
}

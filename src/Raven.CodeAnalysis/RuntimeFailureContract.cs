using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

/// <summary>Selects the NeoCLR namespace function that terminates guest execution.</summary>
/// <param name="AssemblyName">Explicit source or native-library owner.</param>
/// <param name="NamespaceName">Namespace containing the function.</param>
/// <param name="FunctionName">Public static non-generic function taking one string and returning unit/void.</param>
/// <remarks>This is a host assertion about runtime behavior, not an inference from a method name.</remarks>
public sealed record RuntimeFailureContract(string AssemblyName, string NamespaceName = "System", string FunctionName = "Fail")
{
    internal bool Matches(IMethodSymbol method) =>
        method.ContainingAssembly?.Name == AssemblyName &&
        method.ContainingNamespace?.ToMetadataName() == NamespaceName &&
        method.Name == FunctionName && method.IsStatic && !method.IsGenericMethod &&
        method.MethodKind == MethodKind.Ordinary && method.DeclaredAccessibility == Accessibility.Public &&
        method.Parameters is [var parameter] && parameter.RefKind == RefKind.None &&
        parameter.Type.SpecialType == SpecialType.System_String &&
        method.ReturnType.SpecialType is SpecialType.System_Void or SpecialType.System_Unit &&
        (method.ContainingType is null || method.ContainingType is SynthesizedNamespaceMembersClassSymbol);

    internal static bool IsTerminal(IMethodSymbol method, CompilationOptions? options) =>
        options is { TargetPlatform: TargetPlatform.NeoCLR, RuntimeFailureContract: { } contract } && contract.Matches(method);
}

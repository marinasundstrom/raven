using System.Collections.Immutable;
using System.Reflection;
using System.Runtime.Loader;
using System.Text;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

/// <summary>
/// Compiles Raven expression syntax into strongly typed delegates at runtime.
/// </summary>
public static class RavenCompiler
{
    /// <summary>
    /// Compiles a delegate-valued Raven expression into <typeparamref name="TDelegate"/>.
    /// </summary>
    /// <typeparam name="TDelegate">The delegate type produced by the expression.</typeparam>
    /// <param name="expression">A Raven expression that evaluates to the requested delegate type.</param>
    /// <param name="references">
    /// Additional metadata references required by the expression. Platform and
    /// currently loaded runtime assemblies are referenced automatically.
    /// </param>
    /// <returns>The compiled delegate.</returns>
    /// <exception cref="RavenCompilationException">
    /// The generated Raven compilation contains errors.
    /// </exception>
    public static TDelegate Compile<TDelegate>(
        ExpressionSyntax expression,
        IEnumerable<MetadataReference>? references = null)
        where TDelegate : Delegate
    {
        ArgumentNullException.ThrowIfNull(expression);

        var delegateType = FormatType(typeof(TDelegate));
        var source = $$"""
            class __RavenCompiledExpression {
                public static func Create() -> {{delegateType}} {
                    return {{expression.ToFullString()}}
                }
            }
            """;
        var syntaxTree = SyntaxTree.ParseText(source, path: "<runtime-compile>");
        var compilation = Compilation.Create(
                $"Raven.RuntimeCompilation.{Guid.NewGuid():N}",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(CreateReferences(references));

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        if (!emitResult.Success)
            throw new RavenCompilationException(emitResult.Diagnostics);

        peStream.Position = 0;
        var assembly = AssemblyLoadContext.Default.LoadFromStream(peStream);
        var createMethod = assembly
            .GetType("__RavenCompiledExpression", throwOnError: true)!
            .GetMethod("Create", BindingFlags.Public | BindingFlags.Static)
            ?? throw new MissingMethodException(
                "__RavenCompiledExpression",
                "Create");
        var compiled = createMethod.Invoke(null, null);
        return compiled is TDelegate result
            ? result
            : throw new InvalidCastException(
                $"The compiled Raven expression produced '{compiled?.GetType()}', not '{typeof(TDelegate)}'.");
    }

    private static MetadataReference[] CreateReferences(IEnumerable<MetadataReference>? additionalReferences)
    {
        var references = new Dictionary<string, MetadataReference>(StringComparer.OrdinalIgnoreCase);

        void AddPath(string? path)
        {
            if (string.IsNullOrWhiteSpace(path) || !File.Exists(path))
                return;

            var fullPath = Path.GetFullPath(path);
            references.TryAdd(fullPath, MetadataReference.CreateFromFile(fullPath));
        }

        if (AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") is string platformAssemblies)
        {
            foreach (var path in platformAssemblies.Split(
                         Path.PathSeparator,
                         StringSplitOptions.RemoveEmptyEntries))
            {
                AddPath(path);
            }
        }

        foreach (var assembly in AppDomain.CurrentDomain.GetAssemblies())
        {
            if (!assembly.IsDynamic)
                AddPath(assembly.Location);
        }

        if (additionalReferences is not null)
        {
            foreach (var reference in additionalReferences.OfType<PortableExecutableReference>())
                AddPath(reference.FilePath);
        }

        return references.Values.ToArray();
    }

    private static string FormatType(Type type)
    {
        if (type.IsArray)
            return $"{FormatType(type.GetElementType()!)}[{new string(',', type.GetArrayRank() - 1)}]";

        if (type.IsByRef)
            return FormatType(type.GetElementType()!);

        if (type.IsPointer)
            return $"{FormatType(type.GetElementType()!)}*";

        if (type.IsGenericParameter)
            return type.Name;

        if (!type.IsGenericType)
            return (type.FullName ?? type.Name).Replace('+', '.');

        var definitionName = type.GetGenericTypeDefinition().FullName
            ?? type.GetGenericTypeDefinition().Name;
        var arityMarker = definitionName.IndexOf('`');
        if (arityMarker >= 0)
            definitionName = definitionName[..arityMarker];

        var arguments = string.Join(", ", type.GetGenericArguments().Select(FormatType));
        return $"{definitionName.Replace('+', '.')}<{arguments}>";
    }
}

/// <summary>
/// Reports diagnostics produced while compiling Raven syntax at runtime.
/// </summary>
public sealed class RavenCompilationException : Exception
{
    internal RavenCompilationException(ImmutableArray<Diagnostic> diagnostics)
        : base(CreateMessage(diagnostics))
    {
        Diagnostics = diagnostics;
    }

    /// <summary>
    /// Gets the diagnostics produced by the failed runtime compilation.
    /// </summary>
    public ImmutableArray<Diagnostic> Diagnostics { get; }

    private static string CreateMessage(ImmutableArray<Diagnostic> diagnostics)
    {
        var builder = new StringBuilder("The Raven expression could not be compiled.");
        foreach (var diagnostic in diagnostics.Where(static diagnostic =>
                     diagnostic.Severity == DiagnosticSeverity.Error))
        {
            builder.AppendLine();
            builder.Append(diagnostic.GetDescription());
        }

        return builder.ToString();
    }
}

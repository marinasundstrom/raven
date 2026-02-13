using System;
using System.Linq;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ResultPropagationImplicitErrorConversionDiagnosticTests : CompilationTestBase
{
    [Fact]
    public void ResultPropagation_UserDefinedImplicitErrorConversion_ReportsInfoDiagnostic()
    {
        const string source = """
public union Result<T, E> {
    Ok(value: T)
    Error(data: E)
}

public extension ResultExtensions<T, E> for Result<T, E> {
    public UnwrapError() -> E {
        return default
    }
}

public class ParserError { }
public class DomainError { }

public extension ErrorConverters for ParserError {
    public static implicit operator(value: ParserError) -> DomainError {
        return default
    }
}

public class Pipeline {
    public static Parse(text: string) -> Result<int, ParserError> {
        return .Error(default)
    }

    public static Execute(text: string) -> Result<int, DomainError> {
        val value = Pipeline.Parse(text)?
        return .Ok(value)
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();

        var conversionDiagnostic = Assert.Single(
            diagnostics.Where(diagnostic => diagnostic.Descriptor == CompilerDiagnostics.ResultPropagationImplicitErrorConversion));
        Assert.Equal(DiagnosticSeverity.Info, conversionDiagnostic.Severity);
        Assert.Contains("ParserError", conversionDiagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Contains("DomainError", conversionDiagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void ResultPropagation_IdentityErrorType_DoesNotReportInfoDiagnostic()
    {
        const string source = """
public union Result<T, E> {
    Ok(value: T)
    Error(data: E)
}

public extension ResultExtensions<T, E> for Result<T, E> {
    public UnwrapError() -> E {
        return default
    }
}

public class ParserError { }

public class Pipeline {
    public static Parse(text: string) -> Result<int, ParserError> {
        return .Error(default)
    }

    public static Execute(text: string) -> Result<int, ParserError> {
        val value = Pipeline.Parse(text)?
        return .Ok(value)
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.ResultPropagationImplicitErrorConversion);
    }

    [Fact]
    public void CarrierConditionalAccess_UserDefinedImplicitErrorConversion_ReportsInfoDiagnostic()
    {
        const string source = """
public union Result<T, E> {
    Ok(value: T)
    Error(data: E)
}

public class ParserError { }
public class DomainError { }

public extension ErrorConverters for ParserError {
    public static implicit operator(value: ParserError) -> DomainError {
        return default
    }
}

public class Pipeline {
    public static Parse(text: string) -> Result<int, ParserError> {
        return .Error(default)
    }

    public static Execute(text: string) -> Result<string, DomainError> {
        return Parse(text)?.ToString()
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotConvertFromTypeToType);

        var conversionDiagnostic = Assert.Single(
            diagnostics.Where(diagnostic => diagnostic.Descriptor == CompilerDiagnostics.ResultPropagationImplicitErrorConversion));
        Assert.Equal(DiagnosticSeverity.Info, conversionDiagnostic.Severity);
        Assert.Contains("ParserError", conversionDiagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Contains("DomainError", conversionDiagnostic.GetMessage(), StringComparison.Ordinal);
    }

    [Fact]
    public void CarrierConditionalAccess_IdentityErrorType_DoesNotReportInfoDiagnostic()
    {
        const string source = """
public union Result<T, E> {
    Ok(value: T)
    Error(data: E)
}

public class ParserError { }

public class Pipeline {
    public static Parse(text: string) -> Result<int, ParserError> {
        return .Error(default)
    }

    public static Execute(text: string) -> Result<string, ParserError> {
        return Parse(text)?.ToString()
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.ResultPropagationImplicitErrorConversion);
    }
}

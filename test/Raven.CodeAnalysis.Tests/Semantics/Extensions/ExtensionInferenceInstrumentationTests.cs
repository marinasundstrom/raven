using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ExtensionInferenceInstrumentationTests : CompilationTestBase
{
    [Fact]
    public void ChainedExtensions_Baseline_DoesNotShowNestedArrayTypeArguments()
    {
        const string source = """
namespace System

union Result<T, E> {
    Ok(value: T)
    Error(data: E)
}

namespace System.Linq

import System.*
import System.Linq.*
import System.Collections.Generic.*

extension EnumerableExt<T> for IEnumerable<T> {
    func ToArrayOrException() -> Result<T[], Exception> {
        try self.ToArray()
    }

    func Step1() -> Result<T[], Exception> {
        self.ToArrayOrException()
    }

    func Step2() -> Result<T[], Exception> {
        self.Step1()
    }
}
""";

        using var writer = new StringWriter();
        using var log = new OverloadResolutionLog(writer, ownsWriter: false);
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithOverloadResolutionLogger(log);

        var (compilation, _) = CreateCompilation(source, options: options);
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();

        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var text = writer.ToString();
        Assert.Contains("ToArrayOrException<T>(self: IEnumerable<T>)", text, StringComparison.Ordinal);
        Assert.DoesNotContain("T[][]", text, StringComparison.Ordinal);
    }

    [Fact]
    public void ChainedExtensions_MapError_DoesNotDuplicateConstructedTypeArguments()
    {
        const string source = """
namespace System

union Result<T, E> {
    Ok(value: T)
    Error(data: E)
}

extension ResultExtensions<T, E> for Result<T, E> {
    func MapError<E2>(mapper: func E -> E2) -> Result<T, E2> {
        self match {
            .Ok(val value) => .Ok(value)
            .Error(val error) => .Error(mapper(error))
        }
    }
}

namespace System.Linq

import System.*
import System.Linq.*
import System.Collections.Generic.*

extension EnumerableExt<T> for IEnumerable<T> {
    func ToArrayOrException() -> Result<T[], Exception> {
        try self.ToArray()
    }

    func Step1() -> Result<T[], Exception> {
        self.ToArrayOrException()
    }

    func Step2<E>(errorFactory: func Exception -> E) -> Result<T[], E> {
        self.Step1().MapError(errorFactory)
    }
}
""";

        using var writer = new StringWriter();
        using var log = new OverloadResolutionLog(writer, ownsWriter: false);
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithOverloadResolutionLogger(log);

        var (compilation, _) = CreateCompilation(source, options: options);
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();

        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var text = writer.ToString();
        Assert.Contains("MapError<T[], Exception, E>", text, StringComparison.Ordinal);
        Assert.DoesNotContain("MapError<T[][]", text, StringComparison.Ordinal);
        Assert.DoesNotContain("List<List<T>>", text, StringComparison.Ordinal);
        Assert.DoesNotContain("Dictionary<string, Dictionary<string, T>>", text, StringComparison.Ordinal);
        Assert.DoesNotContain("ConversionFailed", text, StringComparison.Ordinal);
    }

    [Fact]
    public void ChainedExtensions_MapError_DoesNotDuplicateListOrDictionaryTypeArguments()
    {
        const string source = """
namespace System

union Result<T, E> {
    Ok(value: T)
    Error(data: E)
}

extension ResultExtensions<T, E> for Result<T, E> {
    func MapError<E2>(mapper: func E -> E2) -> Result<T, E2> {
        self match {
            .Ok(val value) => .Ok(value)
            .Error(val error) => .Error(mapper(error))
        }
    }
}

namespace System.Linq

import System.*
import System.Linq.*
import System.Collections.Generic.*

extension EnumerableExt<T> for IEnumerable<T> {
    func ToListOrException() -> Result<List<T>, Exception> {
        try List<T>()
    }

    func ToDictionaryOrException() -> Result<Dictionary<string, T>, Exception> {
        try Dictionary<string, T>()
    }

    func ListStep<E>(errorFactory: func Exception -> E) -> Result<List<T>, E> {
        self.ToListOrException().MapError(errorFactory)
    }

    func DictionaryStep<E>(errorFactory: func Exception -> E) -> Result<Dictionary<string, T>, E> {
        self.ToDictionaryOrException().MapError(errorFactory)
    }
}
""";

        using var writer = new StringWriter();
        using var log = new OverloadResolutionLog(writer, ownsWriter: false);
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithOverloadResolutionLogger(log);

        var (compilation, _) = CreateCompilation(source, options: options);
        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();

        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var text = writer.ToString();
        Assert.Contains("MapError<List<T>, Exception, E>", text, StringComparison.Ordinal);
        Assert.Contains("MapError<Dictionary<string, T>, Exception, E>", text, StringComparison.Ordinal);
        Assert.DoesNotContain("MapError<List<List<T>>", text, StringComparison.Ordinal);
        Assert.DoesNotContain("MapError<Dictionary<string, Dictionary<string, T>>", text, StringComparison.Ordinal);
        Assert.DoesNotContain("ConversionFailed", text, StringComparison.Ordinal);
    }
}

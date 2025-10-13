using System;
using System.IO;

using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Tests.Utilities;

internal static class IlVerifyTestHelper
{
    public static bool TryResolve(ITestOutputHelper output)
    {
        return TryResolveExecutable(output);
    }

    private static bool TryResolveExecutable(ITestOutputHelper? output)
    {
        if (IlVerifyRunner.TryResolveExecutable(null, out _))
            return true;

        output?.WriteLine(
            $"ilverify executable was not found. Install it via 'dotnet tool install --global ilverify', run 'dotnet tool restore', or set {IlVerifyRunner.PathEnvironmentVariableName}.");
        return false;
    }
}

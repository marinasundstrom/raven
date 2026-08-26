# Raven.Sdk

`Raven.Sdk` is the MSBuild project SDK used by Raven applications. It builds on
the standard .NET SDK and supplies the Raven compiler, core library, standard
macros, and Raven build targets.

Create a project with the matching `Raven.Templates` package instead of writing
the project file by hand:

```console
dotnet new install Raven.Templates@VERSION
dotnet new raven-console -n HelloRaven
cd HelloRaven
dotnet run
```

The generated project pins the matching SDK version so that normal .NET restore
and build commands can resolve the complete Raven toolchain.

## Implicit imports

`Raven.Sdk` follows the standard .NET SDK project pattern while using Raven's
language terminology. `ImplicitImports` defaults to `enable`, and the SDK
contributes its defaults as ordinary `Import` items:

```xml
<PropertyGroup>
  <ImplicitImports>disable</ImplicitImports>
</PropertyGroup>

<ItemGroup>
  <Import Include="MyApplication.Models" />
  <Import Remove="System.Net.Http" />
</ItemGroup>
```

Future SDKs such as `Raven.Sdk.Web` can add their own `Import` items through
their `Sdk.props`. The compiler consumes the evaluated item collection and does
not need to recognize individual SDK names.

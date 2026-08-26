# Raven.Sdk.Web

`Raven.Sdk.Web` is the MSBuild project SDK for Raven ASP.NET Core applications.
It composes Raven's compiler and standard libraries with
`Microsoft.NET.Sdk.Web`, including the shared ASP.NET Core framework, Web SDK
build and publish defaults, static web assets, and Web-specific implicit
imports. Use this SDK for the Web application model; use the base `Raven.Sdk`
for general-purpose applications.

```xml
<Project Sdk="Raven.Sdk.Web/VERSION">
  <PropertyGroup>
    <TargetFramework>net11.0</TargetFramework>
    <OutputType>Exe</OutputType>
  </PropertyGroup>
</Project>
```

The SDK supplies the standard Raven imports plus the ASP.NET Core and
`Microsoft.Extensions` imports used by the .NET Web SDK. Projects can customize
the evaluated import set with ordinary MSBuild item operations:

```xml
<ItemGroup>
  <Import Remove="Microsoft.AspNetCore.Hosting" />
  <Import Include="MyApplication.Endpoints" />
</ItemGroup>
```

Set `ImplicitImports` to `disable` to suppress all SDK-provided imports.

The SDK is also the natural future home for optional Web-specific analyzer
presets. Analyzers are not added by this initial package slice.

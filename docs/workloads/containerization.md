# Containerize a Raven application

Raven projects can use the .NET SDK's built-in container publishing support to
create an OCI container image without a Dockerfile. Raven compiles the
application, and the .NET SDK selects a base image, stages the published files,
and creates the image.

Use this deployment model for a project-based application that will run as a
service, scheduled job, command-line process, or other container workload. A
single file run with `rvn run` is intended for development and scripting; move
the application into a `.rvnproj` before publishing it.

## Prerequisites

You need:

- an executable Raven project that targets modern .NET;
- .NET SDK 8.0.200 or newer for built-in console application container
  support; and
- network access to the selected base-image registry.

Docker is not required to build the image. Docker, Podman, or another
OCI-compatible runtime is needed only when publishing to a local daemon or
running the resulting image locally. Publishing an image archive or pushing
directly to a registry does not require a local container daemon.

The examples below use `MyApp.rvnproj`. Run them from the project directory, or
replace that name with the path to your project.

## Publish to a local container runtime

The most direct development workflow publishes the application and loads the
image into the available local container runtime:

```bash
dotnet publish MyApp.rvnproj \
  -c Release \
  --os linux \
  --arch x64 \
  /t:PublishContainer
```

The default repository name comes from the project's `AssemblyName`, and the
default tag is `latest`. For example, if the assembly is named `MyApp`, run it
with:

```bash
docker run --rm myapp:latest
```

Repository names are normalized according to the container tooling's naming
rules. Set `ContainerRepository` explicitly when the image must have a stable,
known name.

## Publish an image archive

An archive is convenient for CI artifacts, security scanning, transfer to
another machine, or any environment without a local container daemon:

```bash
dotnet publish MyApp.rvnproj \
  -c Release \
  --os linux \
  --arch x64 \
  /t:PublishContainer \
  -p:ContainerArchiveOutputPath=./artifacts/myapp.tar.gz
```

Load the archive later with Docker or Podman:

```bash
docker load -i ./artifacts/myapp.tar.gz
```

## Publish directly to a registry

Set `ContainerRegistry` to push the generated image directly to a registry:

```bash
dotnet publish MyApp.rvnproj \
  -c Release \
  --os linux \
  --arch x64 \
  /t:PublishContainer \
  -p:ContainerRegistry=ghcr.io \
  -p:ContainerRepository=example/myapp \
  -p:ContainerImageTag=1.0.0
```

Authenticate with the registry before publishing. The .NET container tooling
uses the credentials available through the normal local container/registry
configuration.

## Configure the image in the project

Standard .NET SDK container properties and items work in a `.rvnproj`. Keeping
stable image metadata in the project makes local and CI publishing consistent:

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <OutputType>Exe</OutputType>

    <ContainerRepository>example/myapp</ContainerRepository>
    <ContainerImageTag>1.0.0</ContainerImageTag>
  </PropertyGroup>

  <ItemGroup>
    <ContainerPort Include="8080" />
    <ContainerEnvironmentVariable Include="DOTNET_ENVIRONMENT" Value="Production" />
  </ItemGroup>
</Project>
```

Command-line `-p:` values can override project properties for a particular
publish. Common settings include:

- `ContainerBaseImage` to choose a specific base image;
- `ContainerRepository` and `ContainerImageTag` to name and version the image;
- `ContainerRegistry` to select a remote registry;
- `ContainerArchiveOutputPath` to write an archive instead of pushing; and
- `ContainerRuntimeIdentifier` or `--os` and `--arch` to select the target
  platform.

Use `ContainerPort` and `ContainerEnvironmentVariable` for image metadata and
defaults. They do not publish secrets. Supply secrets through the deployment
platform at runtime rather than embedding them in the project or image.

## How Raven fits into the publish pipeline

A `.rvnproj` uses `Microsoft.NET.Sdk`. Raven replaces the language compilation
step while leaving the standard SDK publish and container targets in place.
Consequently, container settings are standard MSBuild properties rather than
Raven-specific command-line switches.

There is currently no `rvn publish` convenience command. Use `dotnet publish`
for folder, Native AOT, and container publishing. The resulting image contains
the same application and runtime dependencies produced by an ordinary Raven
project publish.

For Raven's build and publish output model, see the [project-system
documentation](../compiler/project-system.md#build-vs-publish-outputs). For the
complete container property reference, base-image selection rules, registry
authentication, and multi-platform publishing, see Microsoft's [.NET SDK
container documentation](https://learn.microsoft.com/dotnet/core/containers/).

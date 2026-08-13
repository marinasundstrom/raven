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

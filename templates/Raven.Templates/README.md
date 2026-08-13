# Raven project templates

Install the package and create a project with the standard .NET CLI:

```bash
dotnet new install Raven.Templates@VERSION
dotnet new raven-console -n HelloRaven
```

Replace `VERSION` with the Raven prerelease version to install.

Available short names are `raven-console`, `raven-classlib`, `raven-web`, and
`raven-nano`. Console, class-library, and Web templates default to `net11.0`;
Nano targets `netnano1.0`. Override a desktop target with the standard option:

```bash
dotnet new raven-web -n RavenWeb --framework net10.0
```

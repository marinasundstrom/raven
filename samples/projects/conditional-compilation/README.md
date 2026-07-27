# Conditional Compilation

This project demonstrates compiler-integrated `#if`, `#elif`, `#else`, and
`#endif` directives. Its project file defines `DEBUG` and `DESKTOP`:

```xml
<DefineConstants>DEBUG;DESKTOP</DefineConstants>
```

Open `src/Program.rvn` in VS Code. The Raven language server evaluates those
project symbols just like the compiler, so the `TRACE`, release, and non-desktop
branches are shown as inactive code.

Change `DefineConstants` to `TRACE`, remove it for the release branches, or add
other symbols to see both compilation and inactive-code highlighting follow the
project configuration.

## Build and run

From this folder:

```bash
dotnet run --project ConditionalCompilation.rvnproj --property WarningLevel=0
```

With the checked-in symbols, the program prints:

```text
Build mode: desktop debug
Debug diagnostics are enabled.
Desktop integrations are enabled.
```

# Embed File Content Macro

This project embeds a UTF-8 text file into the compiled application:

```raven
import Raven.Macros.*

let message = embedFileContent!("assets/message.txt")
```

The path is relative to `src/Main.rvn`, the source file containing the macro
invocation. The macro verifies that `src/assets/message.txt` exists during
compilation and replaces the invocation with a string literal containing the
file's text. The resulting application does not open or deploy the text file
at runtime.

Run the sample:

```bash
dotnet run --project samples/projects/embed-file-content/EmbedFileContent.rvnproj \
  --property WarningLevel=0
```

Expected output:

```text
This text was embedded while the Raven program was compiled.
```

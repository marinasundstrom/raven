# SHA-256 Digest Macro

This project computes a SHA-256 digest during compilation:

```raven
import Raven.Macros.*

let digest = sha256Digest!("hello")
```

The macro replaces the invocation with a lowercase hexadecimal string literal.
The resulting application does not perform hashing at runtime.

Run the sample:

```bash
dotnet run --project samples/projects/sha256-digest/Sha256Digest.rvnproj \
  --property WarningLevel=0
```

Expected output:

```text
2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824
```

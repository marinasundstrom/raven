---
name: raven-lsp-debug
description: Troubleshooting workflow for Raven language service and editor failures. Use when investigating hover, completion, definition, diagnostics, or other LSP and editor integration problems. Covers required log capture, repro reduction, and cross-checking client and server behavior.
---

# Raven LSP Debugging

Use this skill for language service and editor failures.

## Required Logs

Capture both sides:

- server logs from `logs/raven-lsp.log`
- client lifecycle and request logs from the VS Code `Raven` output channel

Include relevant excerpts when reporting or fixing hover, completion, or definition failures.

## Workflow

1. Reduce the issue to a minimal source example.
2. Record the exact editor action that fails.
3. Check the client log to confirm the request lifecycle and parameters.
4. Check the server log to see request handling, failures, or missing symbol results.
5. Correlate the failing request with syntax, symbol lookup, binding, and diagnostics behavior in the compiler or language service code.
6. Add focused regression coverage if the failure is fixed in code.

## Notes

- Do not report only one side of the logs.
- If the compiler model is clearly wrong, fix the underlying compiler or language-service behavior rather than encoding client-side workarounds.

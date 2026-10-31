# Raven syntax highlighting and GitHub Linguist

[`raven.tmLanguage.json`](raven.tmLanguage.json) is Raven's canonical TextMate
grammar. It uses scope `source.raven`, recognizes `.rvn` and legacy `.rav` files,
and is covered by the repository's [MIT license](../../../LICENSE). The VS Code
extension registers it in [`package.json`](../package.json).

## GitHub status

As checked on 2026-10-26, Raven and its extensions are absent from
[Linguist's language registry](https://github.com/github-linguist/linguist/blob/main/lib/linguist/languages.yml).
Committing a grammar to a repository does not install it on GitHub.com.
Likewise, `linguist-language=Raven` in `.gitattributes` cannot register a new
language; [overrides select an existing Linguist language](https://github.com/github-linguist/linguist/blob/main/docs/overrides.md).

One upstream Raven registration would cover both
[Raven](https://github.com/marinasundstrom/raven) and
[neoCLR](https://github.com/marinasundstrom/neoCLR), as well as other repositories
containing Raven source. neoCLR is a runtime target, not a separate source
language for highlighting. Keep one grammar here rather than maintaining a
second copy in neoCLR. Once GitHub deploys support, `.rvn` files in both
repositories can be detected automatically. Legacy `.rav` support needs its own
usage evidence if included in the submission.

## Upstream submission

Follow the current [Linguist contribution requirements](https://github.com/github-linguist/linguist/blob/main/CONTRIBUTING.md)
before submitting. At the time of writing, extensions used multiple times per
repository need at least 2,000 indexed files in the last year, excluding forks,
with a reasonable distribution across owners and repositories. The maintainers
may filter out the language author's repositories. Files in Raven and neoCLR
alone therefore do not establish eligibility. No upstream registration or
eligibility assessment has been completed by adding this document.

When eligible:

1. Add a Raven entry to `lib/linguist/languages.yml` in a Linguist checkout.
   Start with the primary extension below; include `.rav` only if it separately
   satisfies the current requirements. The lowercase language name is an
   implicit alias, so a redundant `raven` alias is unnecessary.

   ```yaml
   Raven:
     type: programming
     extensions:
     - ".rvn"
     tm_scope: source.raven
     ace_mode: text
   ```

2. Import the canonical grammar using Linguist's tooling:

   ```sh
   script/add-grammar https://github.com/marinasundstrom/raven
   ```

   Verify that the importer finds the nested grammar under
   `src/Raven.VSCode/syntaxes/`. Resolve any importer or grammar validation
   failures before submission; this import has not yet been validated.
3. Add representative real-world Raven samples, with source links and their
   individual licenses. Choose code demonstrating the language, rather than
   hello-world or tutorial examples. Samples may come from either repository
   when their licensing permits it.
4. Generate the language ID with `script/update-ids`, add the required detection
   tests, and run Linguist's tests following its contribution guide.
5. Submit the language entry, imported grammar, samples, tests, and public usage
   evidence together. GitHub.com support still depends on upstream acceptance
   and deployment of an updated Linguist release.

Until then, the Raven VS Code extension supplies Raven highlighting locally.
Selecting an existing language such as Swift in `.gitattributes` would provide
only approximate highlighting and would classify those files as that language
in GitHub statistics; it is not Raven registration.

## Target-exclusive syntax

Keep one shared highlighting grammar. Document target-exclusive forms as
**.NET only** or **neoCLR only** in the EBNF and owning language-reference section;
shared forms need no target label. For example, `func` in a generic category
constraint is neoCLR only, while `func` in a function declaration is shared.
TextMate and the website highlighter recognize syntax without project target
context; they must not present recognition as proof that the selected target
supports it. Compiler/LSP diagnostics enforce target validity. Experimental backend
gaps are support limitations, not automatically exclusive language rules.

(() => {
    const ravenKeywords = new Set([
        "abstract", "as", "async", "await", "base", "break", "case", "catch",
        "class", "const", "continue", "default", "delegate", "do", "else",
        "enum", "extension", "false", "field", "finally", "for", "foreach",
        "from", "func", "get", "if", "implements", "import", "in", "init",
        "interface", "internal", "is", "let", "macro", "match", "namespace",
        "new", "null", "out", "override", "private", "protected", "public",
        "record", "ref", "return", "sealed", "set", "static", "struct", "this",
        "throw", "trait", "true", "try", "union", "val", "var", "virtual",
        "when", "where", "while", "with", "yield"
    ]);
    const tokenPattern = /\/\/[^\n]*|\/\*[\s\S]*?\*\/|"(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*'|\b(?:0x[\da-fA-F]+|\d+(?:\.\d+)?)\b|\b[A-Za-z_][A-Za-z0-9_]*\b/g;
    const escapeHtml = value => value
        .replaceAll("&", "&amp;")
        .replaceAll("<", "&lt;")
        .replaceAll(">", "&gt;");

    for (const code of document.querySelectorAll(
        "pre code.language-raven, pre code.language-rvn, pre code.language-rav")) {
        const source = code.textContent;
        let cursor = 0;
        let highlighted = "";

        for (const match of source.matchAll(tokenPattern)) {
            highlighted += escapeHtml(source.slice(cursor, match.index));
            const token = match[0];
            let kind = "";

            if (token.startsWith("//") || token.startsWith("/*"))
                kind = "comment";
            else if (token.startsWith("\"") || token.startsWith("'"))
                kind = "string";
            else if (/^(?:0x[\da-fA-F]+|\d)/.test(token))
                kind = "number";
            else if (ravenKeywords.has(token))
                kind = "keyword";

            highlighted += kind
                ? `<span class="syntax-${kind}">${escapeHtml(token)}</span>`
                : escapeHtml(token);
            cursor = match.index + token.length;
        }

        code.innerHTML = highlighted + escapeHtml(source.slice(cursor));
        code.dataset.highlighted = "raven";
    }

    const outline = document.querySelector("#page-outline-links");
    if (!outline)
        return;

    const headings = [...document.querySelectorAll(".api-content h2, .api-content h3")];
    for (const heading of headings) {
        if (!heading.id)
            continue;

        const link = document.createElement("a");
        link.href = `#${heading.id}`;
        link.textContent = heading.textContent?.trim() ?? "";
        link.dataset.level = heading.tagName === "H3" ? "3" : "2";
        outline.append(link);
    }

    if (outline.childElementCount === 0)
        document.querySelector(".page-outline")?.remove();
})();

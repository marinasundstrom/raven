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

    const navigation = document.querySelector(".reference-navigation");
    const filter = document.querySelector("#navigation-filter");
    for (const link of navigation?.querySelectorAll("a") ?? []) {
        if (new URL(link.href).pathname === window.location.pathname)
            link.setAttribute("aria-current", "page");
    }
    filter?.addEventListener("input", () => {
        const query = filter.value.trim().toLocaleLowerCase();
        const items = [...navigation.querySelectorAll("li")];
        for (const item of items.reverse()) {
            const label = item.querySelector(":scope > a, :scope > span, :scope > details > summary");
            const matches = label?.textContent.toLocaleLowerCase().includes(query);
            const childMatches = [...item.querySelectorAll(":scope > details > ul > li")].some(child => !child.hidden);
            item.hidden = !matches && !childMatches;
            const group = item.querySelector(":scope > details");
            if (group && query) {
                if (!group.hasAttribute("data-before-filter")) group.dataset.beforeFilter = String(group.open);
                if (!item.hidden) group.open = true;
            } else if (group?.hasAttribute("data-before-filter")) {
                group.open = group.dataset.beforeFilter === "true";
                delete group.dataset.beforeFilter;
            }
            if (matches) for (const child of item.querySelectorAll("li")) child.hidden = false;
        }
        document.querySelector("#navigation-empty").hidden = items.some(item => !item.hidden);
    });

    const apiBrowser = document.querySelector("#api-browser");
    const apiToggle = document.querySelector(".api-browser-toggle");
    if (apiBrowser && apiToggle) {
        const smallScreen = window.matchMedia("(max-width: 760px)");
        const closeBrowser = () => {
            apiBrowser.close();
            apiToggle.setAttribute("aria-expanded", "false");
        };
        const syncBrowser = () => {
            closeBrowser();
            if (!smallScreen.matches) apiBrowser.setAttribute("open", "");
        };
        document.body.classList.add("api-navigation-ready");
        syncBrowser();
        smallScreen.addEventListener("change", syncBrowser);
        apiToggle.addEventListener("click", () => {
            apiBrowser.showModal();
            apiToggle.setAttribute("aria-expanded", "true");
        });
        apiBrowser.querySelector(".api-browser-close").addEventListener("click", closeBrowser);
        apiBrowser.addEventListener("keydown", event => {
            if (event.key === "Escape" && smallScreen.matches) {
                event.preventDefault();
                closeBrowser();
            }
        });
        apiBrowser.addEventListener("close", () => apiToggle.setAttribute("aria-expanded", "false"));
        apiBrowser.addEventListener("click", event => {
            if (event.target !== apiBrowser || !smallScreen.matches) return;
            const bounds = apiBrowser.getBoundingClientRect();
            if (event.clientX < bounds.left || event.clientX > bounds.right ||
                event.clientY < bounds.top || event.clientY > bounds.bottom) closeBrowser();
        });
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

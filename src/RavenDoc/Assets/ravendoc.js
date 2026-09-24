import hljs from './highlight-core.js';
import raven from './raven-language.js';

hljs.registerLanguage('raven', raven);
for (const code of document.querySelectorAll(
    'pre code.language-raven, pre code.language-rvn, pre code.language-rav')) {
    hljs.highlightElement(code);
}

(() => {
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

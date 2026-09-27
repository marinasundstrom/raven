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

    const updateOutline = () => {
        outline.replaceChildren();
        for (const heading of document.querySelectorAll(".api-content h2, .api-content h3")) {
            if (!heading.id || heading.closest("[hidden]")) continue;
            const link = document.createElement("a");
            link.href = `#${heading.id}`;
            link.textContent = heading.textContent?.trim() ?? "";
            link.dataset.level = heading.tagName === "H3" ? "3" : "2";
            outline.append(link);
        }
    };
    document.addEventListener("ravendoc:members-grouped", updateOutline);
    updateOutline();

    if (outline.childElementCount === 0)
        document.querySelector(".page-outline")?.remove();
})();


(() => {
    const container = document.querySelector("#member-groups");
    const control = document.querySelector("#member-grouping");
    if (!container || !control) return;
    const inherited = document.querySelector("#show-inherited-members");
    try { inherited.checked = localStorage.getItem("raven-show-inherited") !== "false"; } catch { }
    inherited.closest("label").hidden = false;
    const original = [...container.children].filter(section => section.classList.contains("member-section"));
    const cards = [...container.querySelectorAll(".member-card")];
    const storageKey = "raven-member-grouping";
    let preferred = container.dataset.defaultGrouping || "kind";
    try { preferred = localStorage.getItem(storageKey) || preferred; } catch { }
    control.value = preferred === "declaringType" ? preferred : "kind";
    control.closest("label").hidden = false;
    const render = () => {
        container.replaceChildren();
        for (const card of cards) card.hidden = !inherited.checked && card.dataset.memberInherited === "true";
        if (control.value === "kind") {
            container.append(...original);
            for (const section of original) {
                const title = section.querySelector("h2").textContent;
                const entries = cards.filter(card => card.dataset.memberKind === title);
                section.querySelector(".member-list").append(...entries);
                const count = entries.filter(card => !card.hidden).length;
                section.hidden = count === 0;
                section.querySelector(".section-heading > span").textContent = `${count} ${count === 1 ? "member" : "members"}`;
            }
        } else {
            const groups = new Map();
            for (const card of cards) {
                if (card.hidden) continue;
                const origin = card.dataset.memberOrigin || "Other members";
                if (!groups.has(origin)) groups.set(origin, []);
                groups.get(origin).push(card);
            }
            let index = 0;
            for (const [origin, entries] of [...groups].sort(([a], [b]) => a.localeCompare(b))) {
                const section = document.createElement("section");
                section.className = "member-section";
                const header = document.createElement("div");
                header.className = "section-heading";
                const heading = document.createElement("h2");
                heading.id = `members-origin-${++index}`;
                heading.textContent = origin;
                section.setAttribute("aria-labelledby", heading.id);
                const count = document.createElement("span");
                count.textContent = `${entries.length} ${entries.length === 1 ? "member" : "members"}`;
                header.append(heading, count);
                const list = document.createElement("div");
                list.className = "member-list";
                list.append(...entries.sort((a, b) => a.dataset.memberName.localeCompare(b.dataset.memberName)));
                section.append(header, list);
                container.append(section);
            }
        }
        document.dispatchEvent(new Event("ravendoc:members-grouped"));
    };
    control.addEventListener("change", () => {
        try { localStorage.setItem(storageKey, control.value); } catch { }
        render();
    });
    inherited.addEventListener("change", () => {
        try { localStorage.setItem("raven-show-inherited", String(inherited.checked)); } catch { }
        render();
    });
    // Preserve bookmarks into the server-rendered member-kind sections.
    if (original.some(section => `#${section.querySelector("h2").id}` === location.hash)) control.value = "kind";
    render();
})();


// A notice or a wrapped header can move the sidebar below its sticky position.
// Measure the actual available space so its final entries remain reachable even
// before the article has scrolled past the notice.
(() => {
    const header = document.querySelector(".site-header");
    const sidebar = document.querySelector(".api-sidebar");
    if (!header) return;
    let pending = false;
    const measure = () => {
        pending = false;
        document.documentElement.style.setProperty("--site-header-height", `${header.getBoundingClientRect().height}px`);
        if (!sidebar || window.matchMedia("(max-width: 760px)").matches) return;
        const available = Math.max(0, window.innerHeight - sidebar.getBoundingClientRect().top - 16);
        sidebar.style.setProperty("--sidebar-available-height", `${available}px`);
    };
    const schedule = () => {
        if (pending) return;
        pending = true;
        requestAnimationFrame(measure);
    };
    window.addEventListener("scroll", schedule, { passive: true });
    window.addEventListener("resize", schedule);
    const observer = new ResizeObserver(schedule);
    observer.observe(header);
    const notice = document.querySelector(".release-notice");
    if (notice) observer.observe(notice);
    measure();
})();

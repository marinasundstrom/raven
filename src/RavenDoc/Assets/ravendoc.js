import hljs from './highlight-core.js';
import raven from './raven-language.js';

hljs.registerLanguage('raven', raven);
for (const code of document.querySelectorAll(
    'pre code.language-raven, pre code.language-rvn, pre code.language-rav, pre code.lang-raven')) {
    hljs.highlightElement(code);
}

(() => {
    const navigation = document.querySelector(".reference-navigation");
    (async () => {
        const panel = navigation?.querySelector("[data-navigation-src]");
        if (panel) {
            try {
                const source = new URL(panel.dataset.navigationSrc, location.href);
                const response = await fetch(source);
                if (!response.ok) throw new Error("Navigation could not load");
                const shared = new DOMParser().parseFromString(await response.text(), "text/html")
                    .querySelector(".api-navigation-panel");
                if (!shared) throw new Error("Navigation is missing");
                for (const link of shared.querySelectorAll("a[href]"))
                    link.href = new URL(link.getAttribute("href"), source).href;
                const expanded = new Set([...panel.querySelectorAll("details[open] > summary")].map(summary => summary.title));
                panel.replaceChildren(...shared.childNodes);
                for (const summary of panel.querySelectorAll("details > summary"))
                    if (expanded.has(summary.title)) summary.parentElement.open = true;
                const current = [...panel.querySelectorAll("a")].filter(link => {
                    const url = new URL(link.href);
                    return url.origin === location.origin && url.pathname.endsWith("/index.html") &&
                        location.pathname.startsWith(url.pathname.slice(0, -"index.html".length));
                }).sort((a, b) => b.pathname.length - a.pathname.length)[0];
                if (current) {
                    current.setAttribute("aria-current", "location");
                    for (let parent = current.parentElement; parent && parent !== panel; parent = parent.parentElement)
                        if (parent.tagName === "DETAILS") parent.open = true;
                }
            } catch {
                // Keep the generated namespace links when offline or when an asset fails.
            }
            panel.dataset.navigationLoaded = "true";
        }
        const filter = document.querySelector("#navigation-filter");
        const links = [...navigation?.querySelectorAll("a") ?? []];
        const pageLinks = links.filter(link => new URL(link.href).pathname === window.location.pathname);
        if (pageLinks.length) {
            for (const link of links) link.removeAttribute("aria-current");
            for (const link of pageLinks) link.setAttribute("aria-current", "page");
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
        if (filter?.value) filter.dispatchEvent(new Event("input"));
    })();

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
    if (inherited) inherited.closest("label").hidden = false;
    const original = [...container.children].filter(section => section.classList.contains("member-section"));
    const cards = [...container.querySelectorAll(".member-card")];
    const extensions = document.querySelector("#show-extension-members");
    extensions.closest("label").hidden = !cards.some(card => card.dataset.memberExtension === "true");
    const storageKey = "raven-member-grouping";
    const readSelection = () => {
        let preferred = container.dataset.defaultGrouping || "kind";
        if (inherited) inherited.checked = true;
        extensions.checked = true;
        try {
            preferred = localStorage.getItem(storageKey) || preferred;
            if (inherited) inherited.checked = localStorage.getItem("raven-show-inherited") !== "false";
            extensions.checked = localStorage.getItem("raven-show-extensions") !== "false";
        } catch { }
        const query = new URL(location.href).searchParams;
        const group = query.get("groupBy");
        const explicitGroup = group === "kind" || group === "declaringType";
        if (explicitGroup) preferred = group;
        // A shared selection wins over the legacy member-kind bookmark fallback.
        else if (original.some(section => `#${section.querySelector("h2").id}` === location.hash)) preferred = "kind";
        control.value = preferred === "declaringType" ? preferred : "kind";
        for (const [key, checkbox] of [["inherited", inherited], ["extensions", extensions]]) {
            const value = query.get(key);
            if (checkbox && (value === "true" || value === "false")) checkbox.checked = value === "true";
        }
    };
    const shareSelection = () => {
        const url = new URL(location.href);
        url.searchParams.set("groupBy", control.value);
        if (inherited) url.searchParams.set("inherited", String(inherited.checked));
        else url.searchParams.delete("inherited");
        url.searchParams.set("extensions", String(extensions.checked));
        history.replaceState(history.state, "", url);
    };
    control.closest("label").hidden = false;
    readSelection();
    const render = () => {
        container.replaceChildren();
        for (const card of cards) card.hidden =
            (inherited && !inherited.checked && card.dataset.memberInherited === "true") ||
            (!extensions.checked && card.dataset.memberExtension === "true");
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
        shareSelection();
        render();
    });
    inherited?.addEventListener("change", () => {
        try { localStorage.setItem("raven-show-inherited", String(inherited.checked)); } catch { }
        shareSelection();
        render();
    });
    extensions.addEventListener("change", () => {
        try { localStorage.setItem("raven-show-extensions", String(extensions.checked)); } catch { }
        shareSelection();
        render();
    });
    window.addEventListener("popstate", () => { readSelection(); render(); });
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


// Keep the same navigation links on mobile, behind a keyboard-accessible disclosure.
(() => {
    const navigation = document.querySelector('.site-navigation');
    const header = navigation?.closest('.site-header');
    if (!header || !navigation.querySelector('a')) return;
    const mobile = matchMedia('(max-width: 980px)');
    const toggle = document.createElement('button');
    toggle.type = 'button';
    toggle.className = 'site-menu-toggle';
    toggle.setAttribute('aria-label', 'Main menu');
    toggle.setAttribute('aria-expanded', 'false');
    navigation.id ||= 'main-navigation';
    toggle.setAttribute('aria-controls', navigation.id);
    toggle.innerHTML = '<svg viewBox="0 0 24 24" fill="currentColor" aria-hidden="true"><circle cx="4" cy="12" r="2"/><circle cx="12" cy="12" r="2"/><circle cx="20" cy="12" r="2"/></svg>';
    navigation.before(toggle);
    header.classList.add('has-navigation-menu');
    const close = () => {
        navigation.classList.remove('is-open');
        toggle.setAttribute('aria-expanded', 'false');
    };
    toggle.addEventListener('click', () => {
        const open = toggle.getAttribute('aria-expanded') !== 'true';
        const searchToggle = header.querySelector('.site-search-toggle');
        if (open && searchToggle?.getAttribute('aria-expanded') === 'true') searchToggle.click();
        navigation.classList.toggle('is-open', open);
        toggle.setAttribute('aria-expanded', String(open));
        if (open) header.querySelectorAll('details[open]').forEach(details => {
            if (!navigation.contains(details)) details.open = false;
        });
    });
    document.addEventListener('click', event => {
        if (!navigation.contains(event.target) && !toggle.contains(event.target)) close();
    });
    document.addEventListener('keydown', event => {
        if (event.key !== 'Escape' || toggle.getAttribute('aria-expanded') !== 'true') return;
        close();
        toggle.focus();
    });
    navigation.addEventListener('click', event => {
        if (mobile.matches && event.target.closest('a')) close();
    });
    mobile.addEventListener('change', close);
})();

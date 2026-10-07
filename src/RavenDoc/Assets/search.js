const root = new URL('.', import.meta.url);
const control = document.querySelector('.site-search');
if (control) {
    const toggle = control.querySelector('button');
    const panel = control.querySelector('section');
    const input = control.querySelector('input');
    const status = control.querySelector('[role="status"]');
    const results = control.querySelector('ol');
    let indexPromise;
    let request = 0;
    const setOpen = open => {
        panel.hidden = !open;
        toggle.setAttribute('aria-expanded', String(open));
        if (open) input.focus();
    };
    toggle.addEventListener('click', () => setOpen(panel.hidden));
    control.addEventListener('keydown', event => {
        if (event.key === 'Escape') {
            event.preventDefault();
            setOpen(false);
            toggle.focus();
        }
    });
    document.addEventListener('click', event => {
        if (!control.contains(event.target) && !input.value.trim()) setOpen(false);
    });
    input.addEventListener('input', async () => {
        const current = ++request;
        const terms = input.value.trim().toLocaleLowerCase().split(/\s+/).filter(Boolean);
        results.replaceChildren();
        if (!terms.length) { status.textContent = ''; return; }
        status.textContent = 'Searching…';
        try {
            indexPromise ??= fetch(new URL('search-index.json', root)).then(response => {
                if (!response.ok) throw new Error('Search index unavailable');
                return response.json();
            }).catch(error => { indexPromise = undefined; throw error; });
            const entries = await indexPromise;
            if (current !== request) return;
            const matches = entries.map(entry => {
                const title = entry.title.toLocaleLowerCase();
                const text = entry.text.toLocaleLowerCase();
                return { ...entry, score: terms.reduce((score, term) => score + (title.includes(term) ? 5 : 0), 0),
                    matches: terms.every(term => title.includes(term) || text.includes(term)) };
            }).filter(entry => entry.matches).sort((a, b) => b.score - a.score || a.title.localeCompare(b.title));
            status.textContent = matches.length ? `${matches.length} results${matches.length > 30 ? ' (showing first 30)' : ''}.` : 'No results.';
            for (const entry of matches.slice(0, 30)) {
                const item = document.createElement('li');
                const link = document.createElement('a');
                link.href = new URL(entry.url, root).href;
                link.textContent = entry.title;
                const excerpt = document.createElement('p');
                const firstMatch = entry.text.toLocaleLowerCase().indexOf(terms[0]);
                const start = Math.max(0, firstMatch - 55);
                excerpt.textContent = (start ? '…' : '') + entry.text.slice(start, start + 180) + '…';
                item.append(link, excerpt);
                results.append(item);
            }
        } catch {
            if (current === request) status.textContent = 'Search could not load. Edit your query to retry.';
        }
    });
}

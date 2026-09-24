(() => {
    const root = document.documentElement;
    const media = window.matchMedia('(prefers-color-scheme: dark)');
    const key = 'ravendoc-theme';
    const normalize = value => ['light', 'dark'].includes(value) ? value : 'system';
    let preference = 'system';
    try { preference = normalize(localStorage.getItem(key)); } catch { }
    const apply = () => {
        root.dataset.theme = preference === 'system' ? (media.matches ? 'dark' : 'light') : preference;
        root.dataset.themePreference = preference;
        for (const control of document.querySelectorAll('[data-theme-choice]'))
            control.setAttribute('aria-checked', String(control.dataset.themeChoice === preference));
    };
    apply();
    document.addEventListener('DOMContentLoaded', apply);
    media.addEventListener('change', apply);
    document.addEventListener('click', event => {
        const choice = event.target.closest('[data-theme-choice]');
        if (choice) {
            preference = normalize(choice.dataset.themeChoice);
            try { localStorage.setItem(key, preference); } catch { }
            apply();
            const menu = choice.closest('.theme-menu');
            menu.open = false;
            menu.querySelector('summary').focus();
        }
        for (const menu of document.querySelectorAll('.theme-menu[open]'))
            if (!menu.contains(event.target)) menu.open = false;
    });
    document.addEventListener('keydown', event => {
        const menu = event.target.closest('.theme-menu');
        if (!menu) return;
        const choices = [...menu.querySelectorAll('[data-theme-choice]')];
        const index = choices.indexOf(document.activeElement);
        if (event.key === 'Escape') {
            menu.open = false;
            menu.querySelector('summary').focus();
        } else if (['ArrowDown', 'ArrowUp', 'Home', 'End'].includes(event.key)) {
            menu.open = true;
            const next = event.key === 'Home' ? 0 : event.key === 'End' ? choices.length - 1
                : (index + (event.key === 'ArrowUp' ? -1 : 1) + choices.length) % choices.length;
            choices[next].focus();
        } else return;
        event.preventDefault();
    });
    window.addEventListener('storage', event => {
        if (event.key === key) { preference = normalize(event.newValue); apply(); }
    });
})();

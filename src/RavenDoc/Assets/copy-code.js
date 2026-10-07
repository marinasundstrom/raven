for (const code of document.querySelectorAll('article pre > code')) {
    const pre = code.parentElement;
    if (pre.querySelector('.copy-code')) continue;
    pre.classList.add('with-copy-code');
    const button = document.createElement('button');
    button.type = 'button';
    button.className = 'copy-code';
    button.textContent = 'Copy';
    button.setAttribute('aria-label', 'Copy code');
    const status = document.createElement('span');
    status.className = 'visually-hidden';
    status.setAttribute('role', 'status');
    let reset;
    button.addEventListener('click', async () => {
        clearTimeout(reset);
        try {
            await navigator.clipboard.writeText(code.textContent);
            button.textContent = 'Copied';
            status.textContent = 'Code copied to clipboard.';
        } catch {
            button.textContent = 'Retry';
            status.textContent = 'Could not copy. Select the code and copy it manually, or retry.';
        }
        reset = setTimeout(() => { button.textContent = 'Copy'; status.textContent = ''; }, 3000);
    });
    pre.append(button, status);
}

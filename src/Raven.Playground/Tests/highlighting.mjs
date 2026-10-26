import assert from 'node:assert/strict';
import { createServer } from 'node:http';
import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { chromium } from 'playwright';

const root = new URL('../wwwroot/', import.meta.url);
const source = 'func `Calculate total price`(price: int, quantity: int) -> int {\n    return price * quantity\n}\nlet result = `Calculate total price`(10, 5)\nlet @if = 1\nlet `if /* comment */` = @if';
const html = `<!doctype html><html data-theme="light"><link rel="stylesheet" href="/js/editor.css"><div id="editor" style="height:500px"></div><script type="module">
import { createEditor } from '/js/editor.js';
await createEditor(document.querySelector('#editor'), ${JSON.stringify(source)}, { invokeMethodAsync: async () => [] });
</script></html>`;
const server = createServer(async (req, res) => {
  try {
    if (req.url === '/') { res.setHeader('Content-Type', 'text/html'); res.end(html); return; }
    const file = new URL('.' + req.url, root);
    if (!fileURLToPath(file).startsWith(fileURLToPath(root))) { res.writeHead(403).end(); return; }
    res.setHeader('Content-Type', file.pathname.endsWith('.js') ? 'text/javascript' : file.pathname.endsWith('.css') ? 'text/css' : 'application/wasm');
    res.end(await readFile(file));
  } catch { res.writeHead(404).end(); }
});
await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
const browser = await chromium.launch({ headless: true });
try {
  const page = await browser.newPage();
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  await page.goto(`http://127.0.0.1:${server.address().port}/`);
  await page.locator('.view-lines').waitFor();
  for (const theme of ['light', 'dark']) {
    await page.evaluate(theme => { document.documentElement.dataset.theme = theme; window.dispatchEvent(new Event('raven-theme-change')); }, theme);
    await page.waitForFunction(theme => {
      const expected = theme === 'dark' ? 'rgb(220, 220, 170)' : 'rgb(121, 94, 38)';
      const names = [...document.querySelectorAll('.view-lines span')].filter(e => e.children.length === 0 && e.textContent?.replaceAll('\u00a0', ' ').includes('`Calculate total price`'));
      return names.length === 2 && names.every(e => getComputedStyle(e).color === expected);
    }, theme);
    const tokens = await page.locator('.view-lines span.mtk1, .view-lines span[class]').evaluateAll(elements => elements.filter(e => e.children.length === 0).map(e => ({ text: e.textContent.replaceAll('\u00a0', ' '), color: getComputedStyle(e).color })));
    const functions = tokens.filter(t => t.text.includes('`Calculate total price`'));
    assert.equal(functions.length, 2, JSON.stringify(tokens));
    assert.equal(functions[0].color, functions[1].color, JSON.stringify(tokens));
    assert.ok(tokens.some(t => t.text.includes('`if /* comment */`')), JSON.stringify(tokens));
    assert.ok(tokens.some(t => t.text.includes('@if')), JSON.stringify(tokens));
  }
  assert.deepEqual(errors, []);
  console.log('Playground browser: whole escaped identifiers render in light and dark themes without semantic information.');
} finally {
  await browser.close();
  server.close();
}

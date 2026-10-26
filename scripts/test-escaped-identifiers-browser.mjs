import assert from 'node:assert/strict';
import { createReadStream } from 'node:fs';
import { createServer } from 'node:http';
import { resolve, extname } from 'node:path';
import { chromium } from '../src/Raven.Playground/node_modules/playwright/index.mjs';

const root = resolve(process.argv[2] ?? '_site');
const types = { '.html': 'text/html', '.css': 'text/css', '.js': 'text/javascript', '.svg': 'image/svg+xml', '.json': 'application/json' };
const server = createServer((req, res) => {
  const path = resolve(root, '.' + new URL(req.url, 'http://localhost').pathname);
  if (!path.startsWith(root + '/')) { res.writeHead(403).end(); return; }
  res.setHeader('Content-Type', types[extname(path)] ?? 'application/octet-stream');
  const file = createReadStream(path);
  file.on('error', () => res.writeHead(404).end());
  file.pipe(res);
});
await new Promise(done => server.listen(0, '127.0.0.1', done));
const browser = await chromium.launch();
try {
  const page = await browser.newPage();
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  await page.goto(`http://127.0.0.1:${server.address().port}/lang/spec/lexical-structure.html`);
  const functions = page.locator('pre code .hljs-title.function_').filter({ hasText: '`Calculate total price`' });
  await functions.first().waitFor();
  assert.equal(await functions.count(), 2);
  for (const theme of ['light', 'dark']) {
    await page.evaluate(theme => document.documentElement.dataset.theme = theme, theme);
    const colors = await functions.evaluateAll(elements => elements.map(element => getComputedStyle(element).color));
    assert.deepEqual(colors, Array(2).fill(theme === 'dark' ? 'rgb(220, 220, 170)' : 'rgb(121, 94, 38)'));
  }
  assert.equal(await page.locator('pre code .hljs-title.function_').filter({ hasText: '@match' }).count(), 1);
  assert.equal(await page.locator('pre code .hljs-type').filter({ hasText: '@int' }).count(), 1);
  assert.equal(await page.locator('pre code .hljs-keyword').filter({ hasText: /^@(match|int|return|and)$/ }).count(), 0);
  assert.deepEqual(errors, []);
  console.log('Website escaped identifiers and function colors passed in light and dark themes.');
} finally {
  await browser.close();
  server.close();
}

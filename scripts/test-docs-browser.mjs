#!/usr/bin/env node
import assert from 'node:assert/strict';
import { createReadStream, existsSync, statSync, readdirSync } from 'node:fs';
import { createServer } from 'node:http';
import { extname, resolve, sep } from 'node:path';
import { chromium } from '../src/Raven.Playground/node_modules/playwright/index.mjs';

const root = resolve(process.argv[2] ?? '_site');
assert.ok(existsSync(resolve(root, 'introduction.html')), 'Build the documentation first.');
const siteBytes = directory => readdirSync(directory, { withFileTypes: true }).reduce((total, entry) => {
  const path = resolve(directory, entry.name);
  return total + (entry.isDirectory() ? siteBytes(path) : statSync(path).size);
}, 0);
assert.ok(siteBytes(root) < 1_000_000_000, 'The published Raven site must fit GitHub Pages’ 1 GB limit.');
const types = { '.html': 'text/html', '.js': 'text/javascript', '.css': 'text/css', '.json': 'application/json', '.svg': 'image/svg+xml' };
const server = createServer((req, res) => {
  const pathname = new URL(req.url, 'http://localhost').pathname.replace(/^\/preview\//, '/');
  const path = resolve(root, '.' + decodeURIComponent(pathname));
  const file = path.endsWith(sep) || (existsSync(path) && statSync(path).isDirectory()) ? resolve(path, 'index.html') : path;
  if (!file.startsWith(root + sep) || !existsSync(file)) { res.writeHead(404).end(); return; }
  res.setHeader('Content-Type', types[extname(file)] ?? 'application/octet-stream');
  createReadStream(file).pipe(res);
});
await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
const base = `http://127.0.0.1:${server.address().port}`;
const browser = await chromium.launch();
let page = await browser.newPage();
const errors = [];
page.on('pageerror', error => errors.push(error.message));
function contrast(a, b) {
  const luminance = value => {
    const rgb = value.match(/[\d.]+/g).slice(0, 3).map(Number).map(n => n / 255).map(n => n <= .04045 ? n / 12.92 : ((n + .055) / 1.055) ** 2.4);
    return rgb[0] * .2126 + rgb[1] * .7152 + rgb[2] * .0722;
  };
  const x = luminance(a), y = luminance(b);
  return (Math.max(x, y) + .05) / (Math.min(x, y) + .05);
}
try {
  for (const width of [390, 1280]) {
    await page.setViewportSize({ width, height: 900 });
    for (const path of ['index.html', 'lang/spec/index.html', 'introduction.html', 'raven-for-csharp-developers.html']) {
      await page.close();
      page = await browser.newPage({ viewport: { width, height: 900 } });
      page.on('pageerror', error => errors.push(error.message));
      await page.goto(`${base}/${path}`);
      await page.locator('.skip-link').waitFor({ state: 'attached' });
      for (const theme of ['light', 'dark']) {
        await page.evaluate(theme => document.documentElement.dataset.theme = theme, theme);
        await page.waitForTimeout(250); // Allow theme styles to settle.
        assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth), false, `${path}: overflow at ${width}px`);
        if (path === 'index.html') {
          const colors = await page.locator('.raven-button-primary').first().evaluate(e => ({ fg: getComputedStyle(e).color, bg: getComputedStyle(e).backgroundColor }));
          assert.ok(contrast(colors.fg, colors.bg) >= 4.5, `${theme}: primary button contrast`);
        }

      }
      await page.keyboard.press('Tab');
      assert.equal(await page.locator('.skip-link').evaluate(e => e === document.activeElement), true, `${width} ${path}: first focus ${await page.evaluate(() => document.activeElement.outerHTML.slice(0,160))}`);
      await page.keyboard.press('Enter');
      assert.equal(await page.locator('main').evaluate(e => e === document.activeElement), true);
      const samples = await page.locator('a.raven-playground-link[href*="playground/?source="]').evaluateAll(links => links.map(a => ({ href: a.href, source: a.parentElement.previousElementSibling?.querySelector('code')?.textContent })));
      for (const { href, source } of samples) {
        assert.ok(source?.trim(), `${path}: sample source is visible`);
        assert.equal(Buffer.from(new URL(href).searchParams.get('source'), 'base64url').toString(), source, `${path}: playground receives the displayed code`);
      }
    }
  }
  await page.setViewportSize({ width: 390, height: 900 });
  await page.goto(`${base}/lang/spec/index.html`);
  const browse = page.getByRole('button', { name: 'Browse documentation', exact: true });
  await browse.click();
  await page.locator('#api-browser').waitFor({ state: 'visible' });
  await page.keyboard.press('Escape');
  assert.equal(await browse.getAttribute('aria-expanded'), 'false');
  const query = page.locator('#reference-query');
  await query.fill('nullable');
  assert.ok(await page.locator('[data-reference-topic]:visible').count() > 0);
  assert.equal(new URL(page.url()).searchParams.get('q'), 'nullable');
  await query.fill('no-such-raven-feature');
  assert.equal(await page.locator('[data-reference-topic]:visible').count(), 0);
  const clear = page.getByRole('button', { name: 'Clear', exact: true });
  await clear.focus();
  await page.keyboard.press('Enter');
  assert.equal(await query.inputValue(), '');
  assert.equal(await query.evaluate(e => e === document.activeElement), true);
  assert.equal(await page.locator('[data-reference-topic]:visible').count(), 49);
  await page.goto(`${base}/index.html`);
  const searchToggle = page.getByRole('button', { name: 'Search site', exact: true });
  await searchToggle.click();
  const search = page.getByRole('searchbox', { name: 'Search documentation and APIs' });
  assert.equal(await search.evaluate(e => e === document.activeElement), true);
  await search.fill('Option');
  await page.locator('.site-search-results a').first().waitFor();
  assert.ok(await page.locator('.site-search-results a[href*="libraries/raven-core/"]').count() > 0);
  await page.mouse.click(3, 890);
  assert.equal(await searchToggle.getAttribute('aria-expanded'), 'true');
  await search.focus();
  await page.keyboard.press('Escape');
  assert.equal(await searchToggle.getAttribute('aria-expanded'), 'false');
  await searchToggle.click();
  assert.equal(await search.inputValue(), 'Option');
  await search.fill('no-such-raven-site-search-result');
  await page.getByText('No results.', { exact: true }).waitFor();
  await page.keyboard.press('Escape');
  await page.evaluate(() => {
    window.copiedText = undefined;
    Object.defineProperty(navigator, 'clipboard', { configurable: true, value: { writeText: async text => { window.copiedText = text; } } });
  });
  const sample = page.locator('article pre > code').first();
  const expectedCode = await sample.textContent();
  const copy = page.getByRole('button', { name: 'Copy code', exact: true }).first();
  await copy.click();
  assert.equal(await page.evaluate(() => window.copiedText), expectedCode);
  assert.equal(await copy.textContent(), 'Copied');
  await page.evaluate(() => Object.defineProperty(navigator, 'clipboard', { configurable: true, value: { writeText: async () => { throw new Error('denied'); } } }));
  await copy.click();
  assert.equal(await copy.textContent(), 'Retry');
  for (const path of ['libraries/raven-core/System/Option%601/index.html', 'libraries/raven-macros/Raven/Macros/index.html']) {
    await page.goto(`${base}/${path}`);
    assert.ok(await page.locator('header a').evaluateAll(links => links.some(a => new URL(a.href).pathname === '/libraries/index.html')), 'API shares website navigation');
    assert.equal(await page.locator('.api-namespace .api-namespace').count(), 0, 'Namespaces are flat');
    const block = page.locator('pre.with-copy-code').first();
    if (await block.count()) {
      const layout = await block.evaluate(pre => {
        const code = pre.querySelector('code');
        const before = code.getBoundingClientRect().top;
        const position = getComputedStyle(pre.parentElement.querySelector('.copy-code')).position;
        pre.classList.remove('with-copy-code');
        const after = code.getBoundingClientRect().top;
        pre.classList.add('with-copy-code');
        return { before, after, position };
      });
      assert.equal(layout.position, 'absolute');
      assert.equal(layout.before, layout.after, 'Copy control does not push code downward');
    }
  }
  // Copy controls stay at the visible edge while the code itself scrolls.
  await page.setViewportSize({ width: 390, height: 900 });
  await page.goto(`${base}/raven-for-csharp-developers.html`);
  const scrollingCode = page.locator('pre.with-copy-code').first();
  const copyBounds = await scrollingCode.evaluate(pre => {
    pre.querySelector('code').textContent += '\n' + 'long example '.repeat(60);
    const button = pre.parentElement.querySelector('.copy-code');
    const before = button.getBoundingClientRect();
    pre.scrollLeft = 120;
    const after = button.getBoundingClientRect();
    return { before: before.x, after: after.x, scroll: pre.scrollLeft, right: after.right, edge: pre.getBoundingClientRect().right };
  });
  assert.ok(copyBounds.scroll > 0, 'Code sample scrolled horizontally');
  assert.equal(copyBounds.before, copyBounds.after, 'Copy button stays fixed while code scrolls');
  assert.ok(copyBounds.edge - copyBounds.right < 16, 'Copy button stays at the visible right edge');
  for (const width of [390, 768, 980]) {
    await page.setViewportSize({ width, height: 900 });
    const menu = page.getByRole('button', { name: 'Main menu', exact: true });
    assert.equal(await menu.isVisible(), true);
    assert.equal(await page.locator('.site-navigation').isVisible(), false);
    assert.equal(await page.getByRole('button', { name: 'Search site', exact: true }).isVisible(), true);
    assert.equal(await page.locator('.theme-menu > summary').isVisible(), true);
    await menu.focus();
    await page.keyboard.press('Enter');
    assert.equal(await menu.getAttribute('aria-expanded'), 'true');
    await page.keyboard.press('Tab');
    assert.equal(await page.locator('.site-navigation').evaluate(nav => nav.contains(document.activeElement)), true);
    await page.keyboard.press('Escape');
    assert.equal(await menu.getAttribute('aria-expanded'), 'false');
    assert.equal(await menu.evaluate(e => e === document.activeElement), true);
    await menu.click();
    await page.locator('footer').click();
    assert.equal(await menu.getAttribute('aria-expanded'), 'false');
  }
  await page.setViewportSize({ width: 1280, height: 900 });
  assert.equal(await page.getByRole('button', { name: 'Main menu', exact: true }).isVisible(), false);
  assert.equal(await page.locator('.site-navigation').isVisible(), true);
  await page.goto(`${base}/libraries/raven-macros/index.html`);
  const macroNavigation = page.locator('.api-navigation-panel');
  assert.ok(await macroNavigation.locator('a[href$="macro_Quote.html"]').count() > 0, 'Macro-only library is navigable');
  assert.ok(await macroNavigation.locator('summary[title="Raven.Macros"]').count() > 0, 'Macro namespace is listed');
  await page.setViewportSize({ width: 1280, height: 900 });
  await page.goto(`${base}/libraries/raven-core/System/Linq/EnumerableOption%601/index.html?inherited=false`);
  assert.ok((await page.locator('article pre code').first().textContent()).includes('static class EnumerableOption'));
  assert.equal(await page.locator('#show-inherited-members').count(), 0, 'Static containers have no inherited-member toggle');
  assert.ok(!(await page.locator('article').textContent()).includes('Inheritance:'));
  await page.locator('#member-grouping').selectOption('declaringType');
  assert.ok(await page.locator('.member-card:visible').count() > 0, 'Static member grouping works without inheritance controls');
  await page.goto(`${base}/raven-for-csharp-developers.html`);
  const currentSection = page.locator('.documentation-nav-section').filter({ has: page.locator('a[aria-current="page"]') }).first();
  assert.equal(await currentSection.evaluate(e => e.open), true, 'Current reading section opens automatically');
  await currentSection.locator(':scope > summary').click();
  assert.equal(await currentSection.evaluate(e => e.open), false);
  await currentSection.locator(':scope > summary').focus();
  await page.keyboard.press('Enter');
  assert.equal(await currentSection.evaluate(e => e.open), true, 'Reading sections support keyboard toggling');
  // Follow real sidebar links across physical folders: the reading hierarchy stays stable.
  const readingRoutes = [
    '/learn.html', '/getting-started.html', '/raven-for-csharp-developers.html',
    '/lang/features/macros.html', '/lang/domain-modeling.html',
    '/workloads/web-api.html', '/showcases/web-api.html',
    '/compiler/raven-compiler.html', '/compiler/analyzers/configuration.html',
    '/libraries/index.html', '/compiler/raven-core-library.html', '/macro-authoring.html',
    '/status.html'
  ];
  const followLink = async link => {
    const destination = await link.evaluate(anchor => anchor.href);
    // evaluateAll does not wait for elements: await the new document before
    // comparing navigation trees, including on slower CI/browser responses.
    await Promise.all([
      page.waitForURL(destination, { waitUntil: 'domcontentloaded' }),
      link.click()
    ]);
  };
  const readingLinks = async () => page.locator('.documentation-navigation a').evaluateAll(links =>
    links.map(a => new URL(a.href).pathname + new URL(a.href).hash));
  const hierarchy = await readingLinks();
  for (const route of readingRoutes) {
    const href = await page.locator('.documentation-navigation a').evaluateAll((links, path) =>
      links.find(a => new URL(a.href).pathname === path && !new URL(a.href).hash)?.getAttribute('href'), route);
    assert.ok(href, `Page belongs to the reading hierarchy: ${route}`);
    const target = page.locator(`.documentation-navigation a[href="${href}"]`).first();
    for (const group of await target.locator('xpath=ancestor::details').all()) {
      if (!await group.evaluate(e => e.open)) await group.locator(':scope > summary').click();
    }
    await followLink(target);
    assert.equal(new URL(page.url()).pathname, route);
    assert.deepEqual(await readingLinks(), hierarchy, `Sidebar stays consistent on ${route}`);
    assert.ok(await page.locator('.documentation-navigation a[aria-current="page"]').count() > 0);
  }
  await page.locator('.site-navigation summary').filter({ hasText: 'API Reference' }).click();
  await followLink(page.locator('.site-navigation').getByRole('link', { name: 'Macros', exact: true }));
  assert.equal(await page.locator('h1').textContent(), 'Macros');
  assert.ok(await page.locator('.api-sidebar:not(.documentation-sidebar)').count() > 0, 'API entry intentionally changes to symbol navigation');
  await page.locator('.api-navigation-panel summary[title="Raven.Macros"]').click();
  await followLink(page.locator('.api-navigation-panel a[href$="macro_Quote.html"]'));
  assert.ok((await page.locator('article').textContent()).includes('Raven.Macros.dll'));
  const compilerType = page.locator('article a[href$="/Raven/CodeAnalysis/Macros/TokenTreeMacroContext/index.html"]').first();
  await followLink(compilerType);
  assert.equal(await page.locator('h1').textContent(), 'TokenTreeMacroContext');
  assert.ok((await page.locator('article').textContent()).includes('Raven.CodeAnalysis.dll'));
  await page.locator('.site-navigation summary').filter({ hasText: 'API Reference' }).click();
  await followLink(page.locator('.site-navigation').getByRole('link', { name: 'Compiler APIs', exact: true }));
  assert.equal(await page.locator('h1').textContent(), 'Compiler APIs');
  await page.goto(`${base}/libraries/raven-codeanalysis/Raven/CodeAnalysis/Compilation/index.html`);
  assert.equal(await page.locator('h1').textContent(), 'Compilation');
  assert.ok(await page.locator('article a[href*="/blob/main/src/Raven.CodeAnalysis/Compilation"]').count(),
    'Compiler API pages link to their C# source files');
  await page.locator('[data-navigation-loaded="true"]').waitFor();
  assert.equal(await page.locator('#api-browser a[href*="BinderReentryInstrumentation/Snapshot/"]').count(), 0,
    'Nested types are reached through the containing type, not sidebar branches');
  assert.ok((await page.locator('article').textContent()).includes('Raven.CodeAnalysis.dll'));
  await page.evaluate(() => sessionStorage.clear());
  let releaseNavigation;
  let navigationStarted;
  let navigationRequests = 0;
  const navigationPending = new Promise(resolve => { navigationStarted = resolve; });
  const navigationRelease = new Promise(resolve => { releaseNavigation = resolve; });
  await page.route('**/api-navigation.html*', async route => {
    navigationRequests++;
    navigationStarted();
    await navigationRelease;
    await route.continue();
  });
  await page.goto(`${base}/preview/libraries/raven-codeanalysis/Raven/CodeAnalysis/Compilation/index.html`);
  await navigationPending;
  const pendingPanel = page.locator('[data-navigation-src]');
  assert.equal(await pendingPanel.evaluate(element => getComputedStyle(element).visibility), 'hidden',
    'Do not flash the fallback menu while the shared tree is loading');
  const sidebarHeight = (await page.locator('#api-browser').boundingBox()).height;
  releaseNavigation();
  await page.locator('[data-navigation-loaded="true"]').waitFor();
  assert.equal(await pendingPanel.evaluate(element => getComputedStyle(element).visibility), 'visible');
  assert.ok(Math.abs((await page.locator('#api-browser').boundingBox()).height - sidebarHeight) < 1,
    'Loading the menu preserves the sidebar height');
  assert.ok(await page.locator('#api-browser details[open] a[aria-current]').count(), 'Shared tree opens the active namespace');
  await page.locator('#navigation-filter').fill('SemanticModel');
  const sharedType = page.locator('#api-browser a[title="SemanticModel"]');
  assert.ok((await sharedType.getAttribute('href')).includes('/preview/libraries/raven-codeanalysis/'));
  await followLink(sharedType);
  await page.locator('[data-navigation-loaded="true"]').waitFor();
  assert.equal(await page.locator('h1').textContent(), 'SemanticModel');
  assert.equal(navigationRequests, 1, 'Subsequent pages restore the shared tree from session cache');
  await page.unroute('**/api-navigation.html*');
  await page.evaluate(() => sessionStorage.clear());
  await page.route('**/api-navigation.html*', route => route.abort());
  await page.goto(`${base}/libraries/raven-codeanalysis/Raven/CodeAnalysis/Compilation/index.html`);
  await page.locator('[data-navigation-loaded="true"]').waitFor();
  assert.ok(await page.locator('#api-browser a[title="Raven.CodeAnalysis"]').count(), 'Namespace fallback survives a failed navigation request');
  assert.equal(await page.locator('#api-browser a[title="SemanticModel"]').count(), 0);
  assert.equal(await page.locator('[data-navigation-src]').evaluate(element => getComputedStyle(element).visibility), 'visible');
  await page.unroute('**/api-navigation.html*');
  await followLink(page.locator('.site-navigation').getByRole('link', { name: 'Getting started', exact: true }));
  assert.deepEqual(await readingLinks(), hierarchy, 'Returning from API restores the documentation hierarchy');
  await followLink(page.locator('.site-navigation').getByRole('link', { name: 'Language reference', exact: true }));
  const referenceHierarchy = await readingLinks();
  assert.notDeepEqual(referenceHierarchy, hierarchy, 'Reference has an intentional separate hierarchy');
  assert.equal(await page.locator('#api-browser-heading').textContent(), 'Language reference');
  for (const route of ['/lang/spec/functions.html', '/lang/spec/type-system.html']) {
    const href = await page.locator('.documentation-navigation a').evaluateAll((links, path) =>
      links.find(a => new URL(a.href).pathname === path && !new URL(a.href).hash)?.getAttribute('href'), route);
    const target = page.locator(`.documentation-navigation a[href="${href}"]`).first();
    for (const group of await target.locator('xpath=ancestor::details').all())
      if (!await group.evaluate(e => e.open)) await group.locator(':scope > summary').click();
    await followLink(target);
    assert.deepEqual(await readingLinks(), referenceHierarchy, 'Reference hierarchy stays stable');
  }
  await followLink(page.locator('.site-navigation').getByRole('link', { name: 'Getting started', exact: true }));
  assert.deepEqual(await readingLinks(), hierarchy);
  assert.equal(await page.locator('#api-browser-heading').textContent(), 'Getting started');
  await page.setViewportSize({ width: 390, height: 900 });
  await page.goto(`${base}/raven-for-csharp-developers.html`);
  await page.addStyleTag({ content: 'article table { font-family: monospace; font-size: 18px; }' });
  assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth), false, 'Wide table typography stays inside the mobile article');
  const table = page.getByRole('table').first();
  assert.ok(await table.evaluate(e => { e.scrollLeft = 30; return e.scrollLeft > 0; }), 'Wide tables remain scrollable');
  await page.goto(`${base}/libraries/raven-codeanalysis/Raven/CodeAnalysis/Compilation/index.html`);
  await page.locator('[data-navigation-loaded="true"]').waitFor({ state: 'attached' });
  await page.locator('.api-browser-toggle').click();
  for (const height of [900, 650]) {
    await page.setViewportSize({ width: 390, height });
    const drawer = await page.locator('#api-browser').boundingBox();
    assert.ok(Math.abs(drawer.y) < 1 && Math.abs(drawer.height - height) < 1,
      'The shared API drawer fills the mobile viewport, including after resizing');
  }
  await page.keyboard.press('Escape');
  assert.deepEqual(errors, []);
  console.log('Documentation browser checks passed: responsive layout, contrast, keyboard navigation, reference search, and example links.');
} finally {
  await browser.close();
  await new Promise(resolve => server.close(resolve));
}

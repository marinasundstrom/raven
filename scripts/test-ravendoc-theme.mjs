import assert from 'node:assert/strict'
import fs from 'node:fs/promises'
import vm from 'node:vm'

const source = await fs.readFile(new URL('../src/RavenDoc/Assets/theme.js', import.meta.url), 'utf8')
function page(saved, systemDark = false, storageBlocked = false) {
  const events = {}, windowEvents = {}, root = { dataset: {} }, control = { dataset: { themeChoice: 'system' }, setAttribute(key, value) { this[key] = value } }
  const media = { matches: systemDark, addEventListener(name, fn) { this.changed = fn } }
  const storage = { value: saved, getItem() { if (storageBlocked) throw Error(); return this.value }, setItem(key, value) { if (storageBlocked) throw Error(); this.value = value } }
  vm.runInNewContext(source, {
    document: { documentElement: root, querySelectorAll: selector => selector === '[data-theme-choice]' ? [control] : [], addEventListener(name, fn) { events[name] = fn } },
    window: { matchMedia: () => media, addEventListener(name, fn) { windowEvents[name] = fn } },
    localStorage: storage
  })
  const choose = value => events.click({ target: { closest: () => ({ dataset: { themeChoice: value }, closest: () => ({ open: true, querySelector: () => ({ focus() {} }) }) }) } })
  return { root, control, storage, media, choose, windowEvents }
}
const first = page(null, true)
assert.equal(first.root.dataset.theme, 'dark')
assert.equal(first.control['aria-checked'], 'true')
first.choose('light')
assert.equal(first.root.dataset.theme, 'light')
assert.equal(first.storage.value, 'light')
first.media.changed()
assert.equal(first.root.dataset.theme, 'light')
assert.equal(page(first.storage.value, true).root.dataset.theme, 'light')
first.choose('system')
first.media.matches = false
first.media.changed()
assert.equal(first.root.dataset.theme, 'light')
first.media.matches = true
first.media.changed()
assert.equal(first.root.dataset.theme, 'dark')
first.windowEvents.storage({ key: 'ravendoc-theme', newValue: 'light' })
assert.equal(first.root.dataset.theme, 'light')
const blocked = page(null, false, true)
blocked.choose('dark')
assert.equal(blocked.root.dataset.theme, 'dark')
assert.equal(page('invalid', true).control['aria-checked'], 'true')
console.log('RavenDoc initial, persisted, live-system and storage-disabled theme checks passed.')

import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import { createRequire } from 'node:module';

const require = createRequire(new URL('../src/Raven.Playground/package.json', import.meta.url));
const { Registry } = require('vscode-textmate');
const { loadWASM, OnigScanner, OnigString } = require('vscode-oniguruma');
await loadWASM(await fs.readFile(require.resolve('vscode-oniguruma/release/onig.wasm')));
const grammarPath = process.argv[2] ?? new URL('../src/Raven.VSCode/syntaxes/raven.tmLanguage.json', import.meta.url);
const definition = JSON.parse(await fs.readFile(grammarPath, 'utf8'));
const registry = new Registry({
  onigLib: Promise.resolve({ createOnigScanner: p => new OnigScanner(p), createOnigString: s => new OnigString(s) }),
  loadGrammar: async name => name === 'source.raven' ? definition : null
});
const grammar = await registry.loadGrammar('source.raven');
for (const [source, name, scope] of [
  ['let @if = 1', '@if', 'variable.other.identifier.raven'],
  ['func @class() {}', '@class', 'entity.name.function.raven'],
  ['object.@class()', '@class', 'entity.name.function.raven'],
  ['let @数据 = 1', '@数据', 'variable.other.identifier.raven'],
  ['func `Calculate total price`(price: int) -> int', '`Calculate total price`', 'entity.name.function.raven'],
  ['`Calculate total price`(10, 5)', '`Calculate total price`', 'entity.name.function.raven'],
  ['calculator.`Calculate total price`(10, 5)', '`Calculate total price`', 'entity.name.function.raven'],
  ['class `Price model` {}', '`Price model`', 'entity.name.type.raven'],
  ['let `if /* <tag> */ 42` = 1', '`if /* <tag> */ 42`', 'variable.other.identifier.raven'],
  ['let `é é 😀` = 1', '`é é 😀`', 'variable.other.identifier.raven'],
  ['let s = "${`a}b`}"', '`a}b`', 'variable.other.identifier.raven'],
  ['let s = "$`total price`"', '`total price`', 'variable.other.identifier.raven']
]) {
  const tokens = grammar.tokenizeLine(source).tokens;
  assert.ok(tokens.some(t => source.slice(t.startIndex, t.endIndex) === name && t.scopes.includes(scope)), source);
}
const unfinished = grammar.tokenizeLine('let `unterminated');
const nextLine = grammar.tokenizeLine('let value = 42', unfinished.ruleStack);
assert.ok(nextLine.tokens.some(t => t.scopes.includes('constant.numeric.raven')));
for (const source of ['// `if`', 'let s = "`if`"']) {
  assert.ok(!grammar.tokenizeLine(source).tokens.some(t => t.scopes.includes('variable.other.identifier.raven')));
}
console.log('TextMate backtick identifiers passed (VS Code / Playground fallback).');

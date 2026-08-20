#!/usr/bin/env node

import fs from 'node:fs/promises'
import path from 'node:path'

const repositoryRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..')
const textMatePath = path.join(repositoryRoot, 'src/Raven.VSCode/syntaxes/raven.tmLanguage.json')
const docsLexerPath = path.join(repositoryRoot, 'docs/template/public/main.js')
const textMate = JSON.parse(await fs.readFile(textMatePath, 'utf8'))
const docsLexerSource = await fs.readFile(docsLexerPath, 'utf8')

globalThis.document = {
  readyState: 'loading',
  addEventListener() {}
}

const docsLexerUrl = `data:text/javascript;base64,${Buffer.from(docsLexerSource).toString('base64')}`
const docsLexerModule = await import(docsLexerUrl)
let ravenFactory
docsLexerModule.default.configureHljs({
  registerLanguage(name, factory) {
    if (name === 'raven') ravenFactory = factory
  }
})

if (!ravenFactory) throw new Error('The documentation site did not register its Raven lexer.')

const mode = Object.freeze({})
const raven = ravenFactory({
  COMMENT: () => mode,
  C_LINE_COMMENT_MODE: mode,
  C_BLOCK_COMMENT_MODE: mode,
  QUOTE_STRING_MODE: mode,
  C_NUMBER_MODE: mode,
  BACKSLASH_ESCAPE: mode
})

const docsKeywords = new Set([
  ...raven.keywords.keyword.split(/\s+/),
  ...raven.keywords.literal.split(/\s+/)
])
const textMateKeywords = new Set()

const hasKeywordScope = rule => {
  if (typeof rule.name === 'string' && /(?:^|\s)(?:keyword|storage\.modifier)/.test(rule.name)) return true
  return Object.values(rule.captures ?? {}).some(capture =>
    typeof capture.name === 'string' && /(?:^|\s)(?:keyword|storage\.modifier)/.test(capture.name))
}

for (const rule of textMate.repository.keywords.patterns) {
  if (!hasKeywordScope(rule) || typeof rule.match !== 'string') continue

  const wordAlternation = rule.match.match(/^\\b\(([a-z]+(?:\|[a-z]+)*)\)\\b/)
  if (wordAlternation) {
    for (const keyword of wordAlternation[1].split('|')) textMateKeywords.add(keyword)
  }
}

// `on` is contextual and has a dedicated mode instead of being a global keyword.
textMateKeywords.delete('on')
const missingKeywords = [...textMateKeywords].filter(keyword => !docsKeywords.has(keyword)).sort()
if (missingKeywords.length > 0) {
  throw new Error(`Documentation lexer is missing TextMate keywords: ${missingKeywords.join(', ')}`)
}

const matchesMode = (scope, source) => raven.contains.some(candidate => {
  if (candidate.scope !== scope || !(candidate.begin instanceof RegExp)) return false
  candidate.begin.lastIndex = 0
  return candidate.begin.test(source)
})

if (!matchesMode('title.function.invoke', 'Double!(21)') ||
    !matchesMode('title.function.invoke', 'Typed<string>! { value }')) {
  throw new Error('Documentation lexer does not classify Name! macro invocations.')
}
if (!matchesMode('meta', '#[Component]')) {
  throw new Error('Documentation lexer does not classify attached macro attributes.')
}
if (!matchesMode('keyword', 'on property: Property')) {
  throw new Error('Documentation lexer does not classify attached macro target clauses.')
}

for (const keyword of ['macro', 'expand', 'replace', 'introduce', 'fragment', 'token']) {
  if (!docsKeywords.has(keyword)) {
    throw new Error(`Documentation lexer is missing the macro keyword ${keyword}.`)
  }
}

const unionCaseDeclarationMode = raven.contains.find(candidate => candidate.beginKeywords === 'case')
if (unionCaseDeclarationMode?.contains?.[0]?.scope !== 'type') {
  throw new Error('Documentation lexer does not classify all union-case declaration names as types.')
}

const unionCasePatternMode = raven.contains.find(candidate =>
  candidate.begin instanceof RegExp && candidate.begin.source.startsWith('\\.'))
if (unionCasePatternMode?.contains?.[0]?.scope !== 'type') {
  throw new Error('Documentation lexer does not classify payload union-case patterns as types.')
}

const functionMode = raven.contains.find(candidate => candidate.scope === 'title.function')
if (!functionMode?.begin || functionMode.begin.test('private (Value: int)')) {
  throw new Error('Documentation lexer misclassifies a primary-constructor access modifier as a function.')
}

const freestandingRules = JSON.stringify(textMate.repository.freestandingMacros)
if (!freestandingRules.includes('entity.name.function.macro.raven') ||
    !freestandingRules.includes('punctuation.definition.macro.raven')) {
  throw new Error('TextMate grammar does not classify both the Name and ! of Name! macro invocations.')
}

const freestandingMacroRule = textMate.repository.freestandingMacros.patterns.find(rule =>
  rule.captures?.[3]?.name === 'punctuation.definition.macro.raven')
const genericMacroMatch = freestandingMacroRule && new RegExp(freestandingMacroRule.match).exec('Typed<string>! { value }')
if (genericMacroMatch?.[1] !== 'Typed' || genericMacroMatch[3] !== '!') {
  throw new Error('TextMate grammar does not preserve generic Name<T>! macro invocations.')
}

const keywordRules = JSON.stringify(textMate.repository.keywords)
for (const requiredScope of [
  'keyword.declaration.macro.raven',
  'keyword.declaration.macro-target.raven',
  'keyword.control.flow.raven'
]) {
  if (!keywordRules.includes(requiredScope)) {
    throw new Error(`TextMate grammar is missing ${requiredScope}.`)
  }
}

const macroDeclarationRule = textMate.repository.keywords.patterns.find(rule =>
  rule.name === 'meta.declaration.macro.raven')
const macroDeclarationMatch = macroDeclarationRule &&
  new RegExp(macroDeclarationRule.match).exec('macro Html<T>(context: T)')
if (macroDeclarationMatch?.[1] !== 'macro' || macroDeclarationMatch[3] !== 'Html') {
  throw new Error('TextMate grammar does not classify macro Name(...) declarations.')
}

if (!keywordRules.includes('entity.name.type.union-case.raven')) {
  throw new Error('TextMate grammar does not give union-case declarations a stable type scope.')
}

const unionCaseRule = textMate.repository.keywords.patterns.find(rule =>
  rule.captures?.[3]?.name === 'entity.name.type.union-case.raven')
for (const declaration of ['case Healthy', 'case TooCold(actual: decimal)']) {
  if (!unionCaseRule || new RegExp(unionCaseRule.match).exec(declaration)?.[3] === undefined) {
    throw new Error(`TextMate grammar does not classify ${declaration} as a union-case type.`)
  }
}

console.log(`Raven highlighting parity passed (${textMateKeywords.size} shared keywords).`)

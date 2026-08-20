// Keep this lightweight browser lexer aligned with the full TextMate grammar in
// src/Raven.VSCode/syntaxes/raven.tmLanguage.json.
const raven = (hljs) => ({
  name: 'Raven',
  aliases: ['rav', 'rvn'],
  keywords: {
    keyword: [
      'abstract', 'add', 'alias', 'and', 'as', 'assembly', 'async', 'await',
      'base', 'break', 'by', 'case', 'catch', 'class', 'const', 'continue',
      'default', 'delegate', 'do', 'else', 'enum', 'event', 'explicit',
      'extension', 'extern', 'field', 'fileprivate', 'final', 'finally', 'fixed',
      'for', 'func', 'get', 'global', 'goto', 'if', 'implicit', 'import', 'in',
      'init', 'interface', 'internal', 'is', 'let', 'loop', 'macro', 'match',
      'method', 'module', 'namespace', 'new', 'nameof', 'not', 'notnull', 'open',
      'operator', 'or', 'out', 'override', 'param', 'parameter', 'params',
      'partial', 'permits', 'private', 'property', 'protected', 'public',
      'readonly', 'record', 'ref', 'remove', 'required', 'return', 'sealed',
      'scoped', 'self', 'set', 'sizeof', 'stackalloc', 'static', 'struct',
      'throw', 'try', 'type', 'typeof', 'union', 'unsafe', 'unmanaged', 'use',
      'val', 'var', 'lock',
      'virtual', 'when', 'where', 'while', 'with', 'yield',
      'expand', 'replace', 'introduce', 'fragment', 'token'
    ].join(' '),
    type: [
      'bool', 'byte', 'char', 'decimal', 'double', 'float', 'int', 'long',
      'nint', 'nuint', 'object', 'sbyte', 'short', 'string', 'uint', 'ulong',
      'unit', 'ushort', 'void', 'Option', 'Result', 'Task', 'ValueTask'
    ].join(' '),
    literal: 'true false null'
  },
  contains: [
    {
      scope: 'meta',
      begin: /^\s*#\s*pragma\b/,
      end: /$/,
      keywords: { keyword: 'warning disable-next-line disable restore' }
    },
    {
      scope: 'meta',
      begin: /#\[/,
      end: /\]/,
      contains: [
        { scope: 'title.function.invoke', begin: /[A-Za-z_][A-Za-z0-9_]*/ },
        hljs.QUOTE_STRING_MODE,
        hljs.C_NUMBER_MODE
      ]
    },
    hljs.COMMENT('///', '$', { contains: [{ scope: 'doctag', begin: /@[A-Za-z]+/ }] }),
    hljs.C_LINE_COMMENT_MODE,
    hljs.C_BLOCK_COMMENT_MODE,
    {
      scope: 'string',
      begin: /"""/,
      end: /"""(?:u8|ascii)?/,
      contains: [{ scope: 'subst', begin: /\$\{/, end: /\}/ }]
    },
    {
      scope: 'string',
      variants: [
        { begin: /"/, end: /"(?:u8|ascii)?/ },
        { begin: /'/, end: /'/ }
      ],
      contains: [
        hljs.BACKSLASH_ESCAPE,
        { scope: 'subst', begin: /\$\{/, end: /\}/ },
        { scope: 'subst', begin: /\$[A-Za-z_][A-Za-z0-9_]*/ }
      ]
    },
    {
      scope: 'number',
      variants: [
        { begin: /\b0[xX][0-9A-Fa-f](?:[0-9A-Fa-f_]*[0-9A-Fa-f])?\b/ },
        { begin: /\b0[bB][01](?:[01_]*[01])?\b/ },
        { begin: /\b\d+(?:_\d+)*(?:\.\d+(?:_\d+)*)?(?:[eE][+-]?\d+(?:_\d+)*)?[A-Za-z]*\b/ }
      ],
      relevance: 0
    },
    {
      scope: 'keyword',
      begin: /\bon(?=\s+(?:(?:[A-Za-z_][A-Za-z0-9_]*)\s*:\s*)?[A-Z_][A-Za-z0-9_.]*)/,
      relevance: 0
    },
    {
      // The static documentation renderer cannot resolve macro aliases. Keep
      // the aliases used by the public showcase visually aligned with their
      // contextual-keyword treatment in compiler-backed editors.
      scope: 'keyword',
      begin: /\b(?:component|markup)(?=\s*!)/,
      relevance: 0
    },
    {
      scope: 'title.function.invoke',
      begin: /\b[A-Za-z_][A-Za-z0-9_]*(?=\s*(?:<[^\r\n{}]*>)?\s*!\s*(?:\(|\{))/,
      relevance: 0
    },
    {
      beginKeywords: 'case',
      end: /(?=\s*(?:\(|$))/,
      contains: [
        { scope: 'type', begin: /[A-Za-z_][A-Za-z0-9_]*/ }
      ],
      relevance: 0
    },
    {
      begin: /\.(?=[A-Z][A-Za-z0-9_]*\s*\()/,
      end: /(?=\s*\()/,
      contains: [
        { scope: 'type', begin: /[A-Z][A-Za-z0-9_]*/ }
      ],
      relevance: 0
    },
    {
      scope: 'title.function',
      begin: /\b(?!(?:if|while|for|match|catch|typeof|nameof|sizeof|default|func|let|val|var|is|as|return|throw|new|init|public|internal|protected|private|fileprivate)\b)[A-Za-z_][A-Za-z0-9_]*(?=\s*\()/,
      relevance: 0
    },
    {
      scope: 'type',
      begin: /\b[A-Z][A-Za-z0-9_]*\b/,
      relevance: 0
    }
  ]
})

const initializeCarousels = () => {
  document.querySelectorAll('[data-raven-carousel]').forEach((carousel) => {
    const tabs = [...carousel.querySelectorAll('[role="tab"]')]
    const slides = tabs.map((tab) => document.getElementById(tab.getAttribute('aria-controls')))
    const reduceMotion = window.matchMedia('(prefers-reduced-motion: reduce)')
    let activeIndex = 0
    let interval

    const select = (index, moveFocus = false) => {
      activeIndex = (index + tabs.length) % tabs.length
      tabs.forEach((tab, tabIndex) => {
        const isActive = tabIndex === activeIndex
        tab.setAttribute('aria-selected', String(isActive))
        tab.tabIndex = isActive ? 0 : -1
        slides[tabIndex].hidden = !isActive
      })

      if (moveFocus) tabs[activeIndex].focus()
    }

    const stop = () => window.clearInterval(interval)
    const start = () => {
      stop()
      if (!reduceMotion.matches) {
        interval = window.setInterval(() => select(activeIndex + 1), 7000)
      }
    }

    tabs.forEach((tab, index) => {
      tab.addEventListener('click', () => {
        select(index)
        start()
      })
      tab.addEventListener('keydown', (event) => {
        if (event.key === 'ArrowRight' || event.key === 'ArrowLeft') {
          event.preventDefault()
          select(activeIndex + (event.key === 'ArrowRight' ? 1 : -1), true)
          start()
        }
      })
    })

    carousel.addEventListener('mouseenter', stop)
    carousel.addEventListener('mouseleave', start)
    carousel.addEventListener('focusin', stop)
    carousel.addEventListener('focusout', (event) => {
      if (!carousel.contains(event.relatedTarget)) start()
    })
    reduceMotion.addEventListener('change', start)
    start()
  })
}

const encodePlaygroundSource = (source) => {
  const bytes = new TextEncoder().encode(source)
  let binary = ''
  bytes.forEach((byte) => { binary += String.fromCharCode(byte) })
  return window.btoa(binary)
    .replace(/=+$/, '')
    .replace(/\+/g, '-')
    .replace(/\//g, '_')
}

const initializePlaygroundSamples = () => {
  const docRoot = new URL(
    document.querySelector('meta[name="docfx:rel"]')?.content ?? '',
    document.baseURI)
  const playgroundBase = new URL('playground/', docRoot)

  document.querySelectorAll('[data-raven-playground]').forEach((marker) => {
    const codeBlock = marker.nextElementSibling
    if (codeBlock?.tagName !== 'PRE') return

    const example = marker.dataset.example
    const snippet = marker.dataset.snippet
    const useDisplayedSource = marker.dataset.ravenPlayground === 'source'
    if (!example && !snippet && !useDisplayedSource) return

    const playgroundUrl = new URL(playgroundBase)
    if (example) {
      playgroundUrl.searchParams.set('example', example)
      if (marker.dataset.run === 'true') playgroundUrl.searchParams.set('run', 'true')
    } else if (snippet) {
      playgroundUrl.searchParams.set('snippet', snippet)
      if (marker.dataset.run === 'true') playgroundUrl.searchParams.set('run', 'true')
    } else {
      playgroundUrl.searchParams.set(
        'source',
        encodePlaygroundSource(codeBlock.querySelector('code')?.textContent ?? ''))
    }

    const actions = document.createElement('div')
    actions.className = 'raven-sample-actions'

    if (marker.dataset.sourceUrl) {
      const sourceLink = document.createElement('a')
      sourceLink.href = marker.dataset.sourceUrl
      sourceLink.textContent = 'View source'
      actions.append(sourceLink)
    }

    const playgroundLink = document.createElement('a')
    playgroundLink.className = 'raven-playground-link'
    playgroundLink.href = playgroundUrl.href
    playgroundLink.target = '_blank'
    playgroundLink.rel = 'noopener'
    playgroundLink.textContent = example || snippet ? 'Try the complete example' : 'Try this code'
    actions.append(playgroundLink)

    codeBlock.insertAdjacentElement('afterend', actions)
  })
}

const initializeRavenSite = () => {
  initializeCarousels()
  initializePlaygroundSamples()
}

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', initializeRavenSite)
} else {
  initializeRavenSite()
}

export default {
  defaultTheme: 'auto',
  configureHljs(hljs) {
    hljs.registerLanguage('raven', raven)
  }
}

// Manual tabs keep code and installation commands stable while readers use them.
const initializeCarousels = () => {
  document.querySelectorAll('[data-raven-carousel], [data-raven-tabs]').forEach((group) => {
    const tabs = [...group.querySelectorAll('[role="tab"]')]
    const panels = tabs.map((tab) => document.getElementById(tab.getAttribute('aria-controls')))
    const select = (index, focus = false) => {
      tabs.forEach((tab, i) => {
        tab.setAttribute('aria-selected', String(i === index))
        tab.tabIndex = i === index ? 0 : -1
        panels[i].hidden = i !== index
      })
      if (focus) tabs[index].focus()
    }
    tabs.forEach((tab, index) => {
      tab.addEventListener('click', () => select(index))
      tab.addEventListener('keydown', (event) => {
        const next = {
          ArrowRight: (index + 1) % tabs.length,
          ArrowLeft: (index - 1 + tabs.length) % tabs.length,
          Home: 0,
          End: tabs.length - 1
        }[event.key]
        if (next !== undefined) {
          event.preventDefault()
          select(next, true)
        }
      })
    })
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
    document.querySelector('.raven-brand')?.getAttribute('href') ?? './index.html',
    document.baseURI)
  const playgroundBase = new URL('playground/', new URL('.', docRoot))

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

const initializeReferenceFinder = () => {
  const finder = document.querySelector('[data-reference-finder]')
  if (!finder) return
  const input = finder.querySelector('input')
  const clear = finder.querySelector('[data-reference-clear]')
  const status = finder.querySelector('[data-reference-count]')
  const empty = document.querySelector('[data-reference-empty]')
  const shortcuts = document.querySelector('[data-reference-shortcuts]')
  const groups = [...document.querySelectorAll('.raven-reference-group')]
  const topics = [...document.querySelectorAll('[data-reference-topic]')].map((element) => ({
    element,
    text: `${element.textContent} ${element.dataset.keywords} ${element.closest('section').querySelector('h2').textContent}`.toLowerCase()
  }))
  const filter = () => {
    const terms = input.value.trim().toLowerCase().split(/\s+/).filter(Boolean)
    let visible = 0
    topics.forEach(({ element, text }) => {
      element.hidden = !terms.every((term) => text.includes(term))
      if (!element.hidden) visible++
    })
    groups.forEach((group) => {
      group.hidden = !group.querySelector('[data-reference-topic]:not([hidden])')
    })
    empty.hidden = visible !== 0
    if (shortcuts) shortcuts.hidden = terms.length > 0
    clear.disabled = input.value.length === 0
    status.textContent = terms.length ? `${visible} of ${topics.length} topics match.` : `${topics.length} reference topics. Filter by name, keyword, or syntax.`
    const url = new URL(window.location.href)
    if (input.value.trim()) url.searchParams.set('q', input.value.trim())
    else url.searchParams.delete('q')
    window.history.replaceState(null, '', url)
  }
  input.value = new URL(window.location.href).searchParams.get('q') ?? ''
  input.addEventListener('input', filter)
  clear.addEventListener('click', () => {
    input.value = ''
    filter()
    input.focus()
  })
  filter()
  finder.hidden = false
}

const initializeRavenSite = () => {
  initializeReferenceFinder()
  initializeCarousels()
  initializePlaygroundSamples()
}

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', initializeRavenSite)
} else {
  initializeRavenSite()
}

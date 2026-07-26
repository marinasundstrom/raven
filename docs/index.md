---
_layout: landing
---

<section class="raven-hero">
  <div class="raven-hero-copy">
    <p class="raven-eyebrow">A modern language for .NET</p>
    <h1>Write expressive programs.<br><span>Keep the whole platform.</span></h1>
    <p class="raven-hero-lead">Raven brings typed, expression-oriented programming,
    algebraic modeling, and familiar object-oriented design to the .NET ecosystem.</p>
    <div class="raven-hero-actions">
      <a class="raven-button raven-button-primary" href="getting-started.md">Get started <span aria-hidden="true">→</span></a>
      <a class="raven-button" href="https://marinasundstrom.github.io/raven/playground/">Try the Playground</a>
    </div>
    <p class="raven-preview-note">Raven is experimental and under active development.</p>
  </div>
  <div class="raven-hero-code" aria-label="Raven code example">
    <div class="raven-code-titlebar">
      <span>quote.rvn</span>
      <span class="raven-code-dots" aria-hidden="true">● ● ●</span>
    </div>
    <pre><code class="lang-raven">record Shipment(Id: int, Weight: decimal)

union QuoteResult {
    case Quoted(amount: decimal)
    case Rejected(reason: string)
}

func Quote(shipment: Shipment) → QuoteResult {
    if shipment.Weight &lt;= 0 {
        return .Rejected("Weight must be positive")
    }

    return .Quoted(12.50m + shipment.Weight * 1.75m)
}</code></pre>
  </div>
</section>

<section class="raven-home-intro">
  <p class="raven-eyebrow">One language, complementary tools</p>
  <h2>Use the right shape for the problem.</h2>
  <p>Build value-oriented models with records and unions, compose behavior with
  functions and expressions, and reach for classes when identity or open
  polymorphism matters—all with direct access to .NET libraries.</p>
</section>

<div class="raven-feature-grid">
  <a class="raven-feature-card" href="raven-in-60-seconds.md">
    <span class="raven-card-kicker">Tour</span>
    <strong>Raven in 60 seconds</strong>
    <span>See the language's core ideas in one small program.</span>
  </a>
  <a class="raven-feature-card" href="learn.md">
    <span class="raven-card-kicker">Learn</span>
    <strong>Choose your path</strong>
    <span>Start from .NET experience or from programming fundamentals.</span>
  </a>
  <a class="raven-feature-card" href="lang/README.md">
    <span class="raven-card-kicker">Reference</span>
    <strong>Explore the language</strong>
    <span>Find precise syntax, semantics, and language guidance.</span>
  </a>
</div>

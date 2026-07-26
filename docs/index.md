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
  <div class="raven-hero-code raven-code-carousel" aria-label="Raven code examples" data-raven-carousel>
    <div class="raven-code-slide" id="raven-sample-modeling" role="tabpanel" aria-labelledby="raven-sample-modeling-tab">
      <div class="raven-code-titlebar">
        <span>modeling.rvn</span>
        <span class="raven-code-dots" aria-hidden="true">● ● ●</span>
      </div>
      <pre><code class="lang-raven">record Shipment(Id: int, Weight: decimal)&#10;&#10;union QuoteResult {&#10;&#32;&#32;&#32;&#32;case Quoted(amount: decimal)&#10;&#32;&#32;&#32;&#32;case Rejected(reason: string)&#10;}&#10;&#10;func Quote(shipment: Shipment) → QuoteResult {&#10;&#32;&#32;&#32;&#32;if shipment.Weight &lt;= 0 {&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;return .Rejected("Weight must be positive")&#10;&#32;&#32;&#32;&#32;}&#10;&#10;&#32;&#32;&#32;&#32;return .Quoted(12.50m + shipment.Weight * 1.75m)&#10;}</code></pre>
    </div>
    <div class="raven-code-slide" id="raven-sample-expressions" role="tabpanel" aria-labelledby="raven-sample-expressions-tab" hidden>
      <div class="raven-code-titlebar">
        <span>expressions.rvn</span>
        <span class="raven-code-dots" aria-hidden="true">● ● ●</span>
      </div>
      <pre><code class="lang-raven">func Describe(result: QuoteResult) → string {&#10;&#32;&#32;&#32;&#32;return match result {&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;.Quoted(let amount) when amount &gt; 100m&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;=&gt; "Large quote: $amount"&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;.Quoted(let amount)&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;=&gt; "Quote: $amount"&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;.Rejected(let reason)&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;=&gt; "Cannot quote: $reason"&#10;&#32;&#32;&#32;&#32;}&#10;}&#10;&#10;let message = Describe(Quote(Shipment(42, 3.5m)))</code></pre>
    </div>
    <div class="raven-code-slide" id="raven-sample-dotnet" role="tabpanel" aria-labelledby="raven-sample-dotnet-tab" hidden>
      <div class="raven-code-titlebar">
        <span>dotnet.rvn</span>
        <span class="raven-code-dots" aria-hidden="true">● ● ●</span>
      </div>
      <pre><code class="lang-raven">import System.Console.*&#10;import System.Text.Json.JsonSerializer&#10;&#10;record Package(Name: string, Version: string)&#10;&#10;let package = Package(&#10;&#32;&#32;&#32;&#32;Name: "Raven.Core",&#10;&#32;&#32;&#32;&#32;Version: "0.1.0"&#10;)&#10;&#10;let json = JsonSerializer.Serialize(package)&#10;WriteLine(json)</code></pre>
    </div>
    <div class="raven-carousel-controls" role="tablist" aria-label="Choose a Raven example">
      <button id="raven-sample-modeling-tab" type="button" role="tab" aria-controls="raven-sample-modeling" aria-selected="true">
        Modeling
      </button>
      <button id="raven-sample-expressions-tab" type="button" role="tab" aria-controls="raven-sample-expressions" aria-selected="false" tabindex="-1">
        Expressions
      </button>
      <button id="raven-sample-dotnet-tab" type="button" role="tab" aria-controls="raven-sample-dotnet" aria-selected="false" tabindex="-1">
        .NET
      </button>
    </div>
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

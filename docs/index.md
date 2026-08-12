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
      <a class="raven-button raven-button-primary" href="raven-in-60-seconds.md">Learn Raven <span aria-hidden="true">→</span></a>
      <a class="raven-button" href="https://marinasundstrom.github.io/raven/playground/">Try it online</a>
    </div>
    <p class="raven-preview-note">Raven is under active development. Evolving areas are marked in the documentation.</p>
  </div>
  <div class="raven-hero-code raven-code-carousel" aria-label="Raven code examples" data-raven-carousel>
    <div class="raven-code-slide" id="raven-sample-modeling" role="tabpanel" aria-labelledby="raven-sample-modeling-tab">
      <div class="raven-code-titlebar"><span>Domain modeling · quote.rvn</span><span class="raven-code-dots" aria-hidden="true">● ● ●</span></div>
      <pre><code class="lang-raven">record Shipment(Id: int, Weight: decimal)&#10;&#10;union QuoteResult {&#10;&#32;&#32;&#32;&#32;case Quoted(amount: decimal)&#10;&#32;&#32;&#32;&#32;case Rejected(reason: string)&#10;}&#10;&#10;func Quote(shipment: Shipment) -&gt; QuoteResult {&#10;&#32;&#32;&#32;&#32;if shipment.Weight &lt;= 0 {&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;return .Rejected("Weight must be positive")&#10;&#32;&#32;&#32;&#32;}&#10;&#10;&#32;&#32;&#32;&#32;return .Quoted(12.50m + shipment.Weight * 1.75m)&#10;}</code></pre>
      <div class="raven-code-learn"><span>Records, unions, functions, and explicit states</span><a href="lang/domain-modeling.md">Learn more <span aria-hidden="true">→</span></a></div>
    </div>
    <div class="raven-code-slide" id="raven-workload-web" role="tabpanel" aria-labelledby="raven-workload-web-tab" hidden>
      <div class="raven-code-titlebar"><span>Web API · Program.rvn</span><span class="raven-code-dots" aria-hidden="true">● ● ●</span></div>
      <pre><code class="lang-raven">import AspNetMinimalApi.Domain.*&#10;import Microsoft.AspNetCore.Builder.*&#10;&#10;let builder = WebApplication.CreateBuilder(args)&#10;builder.Services.AddOpenApi()&#10;&#10;use app = builder.Build()&#10;app.MapGet("/pets/{id}", FindPet)&#10;app.MapGet("/pets", StreamPets)&#10;app.MapPost("/pets/find", LookupPet)&#10;&#10;app.Run()</code></pre>
      <div class="raven-code-learn"><span>ASP.NET Core, handlers, records, and unions</span><a href="workloads/web-api.md">Learn more <span aria-hidden="true">→</span></a></div>
    </div>
    <div class="raven-code-slide" id="raven-workload-embedded" role="tabpanel" aria-labelledby="raven-workload-embedded-tab" hidden>
      <div class="raven-code-titlebar"><span>Embedded IoT · temperature.rvn</span><span class="raven-code-dots" aria-hidden="true">● ● ●</span></div>
      <pre><code class="lang-raven">union TemperatureState {&#10;&#32;&#32;&#32;&#32;case SensorUnavailable&#10;&#32;&#32;&#32;&#32;case Comfortable(celsius: double)&#10;&#32;&#32;&#32;&#32;case TooHot(celsius: double)&#10;}&#10;&#10;func ActOn(state: TemperatureState, alarm: GpioPin) {&#10;&#32;&#32;&#32;&#32;match state {&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;.SensorUnavailable =&gt; alarm.Write(PinValue.High)&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;.Comfortable(_) =&gt; alarm.Write(PinValue.Low)&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;.TooHot(_) =&gt; alarm.Write(PinValue.High)&#10;&#32;&#32;&#32;&#32;}&#10;}</code></pre>
      <div class="raven-code-learn"><span>.NET nanoFramework, sensor states, and GPIO</span><a href="workloads/embedded-iot.md">Learn more <span aria-hidden="true">→</span></a></div>
    </div>
    <div class="raven-carousel-controls" role="tablist" aria-label="Choose a Raven example">
      <button id="raven-sample-modeling-tab" type="button" role="tab" aria-controls="raven-sample-modeling" aria-selected="true">Modeling</button>
      <button id="raven-workload-web-tab" type="button" role="tab" aria-controls="raven-workload-web" aria-selected="false" tabindex="-1">Web API</button>
      <button id="raven-workload-embedded-tab" type="button" role="tab" aria-controls="raven-workload-embedded" aria-selected="false" tabindex="-1">Embedded IoT</button>
    </div>
  </div>
</section>

<section class="raven-learning-path">
  <div class="raven-section-heading">
    <p class="raven-eyebrow">Start here</p>
    <h2>One clear path into Raven.</h2>
    <p>Begin with the language itself. Install the compiler when you are ready
    to run a program locally.</p>
  </div>
  <ol class="raven-path-steps">
    <li>
      <span class="raven-step-number">1</span>
      <a href="raven-in-60-seconds.md">Raven in 60 seconds</a>
      <p>Read one small program and learn the ideas that shape the language.</p>
    </li>
    <li>
      <span class="raven-step-number">2</span>
      <a href="introduction.md">Take the language tour</a>
      <p>See functions, data modeling, patterns, failure handling, objects, and .NET interop.</p>
    </li>
    <li>
      <span class="raven-step-number">3</span>
      <a href="getting-started.md">Build and run Raven</a>
      <p>Set up the source-built toolchain and create your first project.</p>
    </li>
  </ol>
  <p class="raven-path-aside">Coming from .NET? Use the default path above, or keep the
  <a href="raven-for-csharp-developers.md">Raven for C# developers</a> guide beside it.</p>
</section>

<section class="raven-web-showcase">
  <div>
    <p class="raven-eyebrow">A real .NET workload</p>
    <h2>Build an ASP.NET Core API.</h2>
    <p>The pet-shelter sample uses ordinary ASP.NET Core routing, OpenAPI, async
    handlers, and streaming responses. Raven records and unions model the API
    domain without giving up the framework you already know.</p>
    <a class="raven-button raven-button-primary" href="workloads/web-api.md">Build the web API <span aria-hidden="true">→</span></a>
  </div>
  <div class="raven-workload-points" aria-label="Web API sample capabilities">
    <div><strong>ASP.NET Core</strong><span>Minimal APIs and dependency injection</span></div>
    <div><strong>Typed domains</strong><span>Records, unions, and pattern matching</span></div>
    <div><strong>Production shapes</strong><span>OpenAPI, async handlers, and streams</span></div>
  </div>
</section>

<section class="raven-reference-callout">
  <p><strong>Already learning Raven?</strong> Go directly to the
  <a href="lang/README.md">language reference</a> or the
  <a href="compiler/index.md">tooling documentation</a>.</p>
</section>

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
  <div class="raven-hero-code raven-code-carousel" aria-label="Raven application examples" data-raven-carousel>
    <div class="raven-code-slide" id="raven-workload-web" role="tabpanel" aria-labelledby="raven-workload-web-tab">
      <div class="raven-code-titlebar"><span>Web API · Program.rvn</span><span class="raven-code-dots" aria-hidden="true">● ● ●</span></div>
      <pre><code class="lang-raven">import AspNetMinimalApi.Domain.*&#10;import Microsoft.AspNetCore.Builder.*&#10;&#10;let builder = WebApplication.CreateBuilder(args)&#10;builder.Services.AddOpenApi()&#10;&#10;use app = builder.Build()&#10;app.MapGet("/pets/{id}", FindPet)&#10;app.MapGet("/pets", StreamPets)&#10;app.MapPost("/pets/find", LookupPet)&#10;&#10;app.Run()</code></pre>
      <div class="raven-code-learn"><span>ASP.NET Core, handlers, records, and unions</span><a href="workloads/web-api.md">Learn more <span aria-hidden="true">→</span></a></div>
    </div>
    <div class="raven-code-slide" id="raven-workload-cli" role="tabpanel" aria-labelledby="raven-workload-cli-tab" hidden>
      <div class="raven-code-titlebar"><span>Command line · hello.rvn</span><span class="raven-code-dots" aria-hidden="true">● ● ●</span></div>
      <pre><code class="lang-raven">#!/usr/bin/env rvn&#10;&#10;import System.*&#10;&#10;func Main(args: string[]) {&#10;&#32;&#32;&#32;&#32;Console.WriteLine(&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;"Hello from a single Raven file!"&#10;&#32;&#32;&#32;&#32;)&#10;&#10;&#32;&#32;&#32;&#32;for argument in args {&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;Console.WriteLine(&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;"Argument: ${argument}"&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;)&#10;&#32;&#32;&#32;&#32;}&#10;}</code></pre>
      <div class="raven-code-learn"><span>One file, arguments, and direct execution</span><a href="workloads/command-line.md">Learn more <span aria-hidden="true">→</span></a></div>
    </div>
    <div class="raven-code-slide" id="raven-workload-iot" role="tabpanel" aria-labelledby="raven-workload-iot-tab" hidden>
      <div class="raven-code-titlebar"><span>IoT monitor · telemetry.rvn</span><span class="raven-code-dots" aria-hidden="true">● ● ●</span></div>
      <pre><code class="lang-raven">async func Main() -&gt; Task {&#10;&#32;&#32;&#32;&#32;let telemetry: ITelemetrySource =&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;SimulatedTelemetrySource()&#10;&#10;&#32;&#32;&#32;&#32;await for result in telemetry.Poll(&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;CancellationToken.None&#10;&#32;&#32;&#32;&#32;) {&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;match result {&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;Ok(let readings) =&gt; PrintReport(readings)&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;Error(let error) =&gt; PrintError(error)&#10;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;}&#10;&#32;&#32;&#32;&#32;}&#10;}</code></pre>
      <div class="raven-code-learn"><span>Async streams, typed errors, and Native AOT</span><a href="workloads/iot-monitor.md">Learn more <span aria-hidden="true">→</span></a></div>
    </div>
    <div class="raven-carousel-controls" role="tablist" aria-label="Choose an application example">
      <button id="raven-workload-web-tab" type="button" role="tab" aria-controls="raven-workload-web" aria-selected="true">Web API</button>
      <button id="raven-workload-cli-tab" type="button" role="tab" aria-controls="raven-workload-cli" aria-selected="false" tabindex="-1">Command line</button>
      <button id="raven-workload-iot-tab" type="button" role="tab" aria-controls="raven-workload-iot" aria-selected="false" tabindex="-1">IoT &amp; AOT</button>
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

import { createReadStream, existsSync, statSync } from "node:fs";
import { createServer } from "node:http";
import { extname, join, normalize, resolve } from "node:path";
import { chromium } from "playwright";

const siteRoot = resolve(process.argv[2] ?? "");
const basePath = "/playground/";

if (!process.argv[2] || !existsSync(join(siteRoot, "index.html"))) {
  throw new Error("Pass the published playground wwwroot directory as the first argument.");
}

const contentTypes = new Map([
  [".css", "text/css; charset=utf-8"],
  [".dat", "application/octet-stream"],
  [".dll", "application/octet-stream"],
  [".html", "text/html; charset=utf-8"],
  [".ico", "image/x-icon"],
  [".js", "text/javascript; charset=utf-8"],
  [".json", "application/json; charset=utf-8"],
  [".png", "image/png"],
  [".wasm", "application/wasm"],
]);

const server = createServer((request, response) => {
  const requestPath = decodeURIComponent(new URL(request.url, "http://localhost").pathname);
  if (!requestPath.startsWith(basePath)) {
    response.writeHead(404);
    response.end();
    return;
  }

  const relativePath = normalize(requestPath.slice(basePath.length))
    .replace(/^(\.\.(\/|\\|$))+/, "")
    .replace(/^[/\\]+/, "");
  let filePath = join(siteRoot, relativePath || "index.html");

  if (!filePath.startsWith(`${siteRoot}/`) || !existsSync(filePath) || statSync(filePath).isDirectory()) {
    filePath = join(siteRoot, "index.html");
  }

  response.writeHead(200, {
    "Cache-Control": "no-store",
    "Content-Type": contentTypes.get(extname(filePath)) ?? "application/octet-stream",
  });
  createReadStream(filePath).pipe(response);
});

await new Promise((resolveListen, reject) => {
  server.once("error", reject);
  server.listen(0, "127.0.0.1", resolveListen);
});

const address = server.address();
const url = `http://127.0.0.1:${address.port}${basePath}`;
const browser = await chromium.launch({ headless: true });
const page = await browser.newPage();
const browserErrors = [];

page.on("console", message => {
  if (message.type() === "error") browserErrors.push(message.text());
});
page.on("pageerror", error => browserErrors.push(error.stack ?? error.message));

try {
  await page.goto(url);
  await page.getByText("Ready", { exact: true }).waitFor({ timeout: 30_000 });

  const themeResponse = await page.request.get(`${url}css/raven-theme.css`);
  const themeContentType = themeResponse.headers()["content-type"] ?? "";
  const themeSource = await themeResponse.text();
  if (!themeResponse.ok() ||
      !themeContentType.startsWith("text/css") ||
      !themeSource.includes("--raven-bg")) {
    throw new Error(
      `Expected the standalone Raven theme stylesheet, got ` +
      `${themeResponse.status()} ${themeContentType}.`,
    );
  }
  const themeBackground = await page.evaluate(() =>
    getComputedStyle(document.documentElement).getPropertyValue("--raven-bg").trim());
  if (!themeBackground) {
    throw new Error("Expected the Raven theme variables to apply to the Playground.");
  }
  if (await page.locator(".raven-brand-copy").count() !== 0) {
    throw new Error("Expected the compact Playground header to omit the brand copy.");
  }
  const mastheadAlignment = await page.evaluate(() => {
    const mark = document.querySelector(".hero-copy .raven-brand-mark")?.getBoundingClientRect();
    const heading = document.querySelector(".hero-copy h1")?.getBoundingClientRect();
    return mark && heading
      ? { markRight: mark.right, headingLeft: heading.left }
      : null;
  });
  if (!mastheadAlignment || mastheadAlignment.markRight > mastheadAlignment.headingLeft) {
    throw new Error("Expected the Raven mark to sit before the Playground heading.");
  }

  const editor = page.locator(".monaco-editor");
  await editor.waitFor();
  const themePicker = page.getByLabel("Theme");
  await themePicker.selectOption("dark");
  await page.waitForFunction(
    () => document.documentElement.dataset.theme === "dark" &&
      document.querySelector(".monaco-editor")?.classList.contains("vs-dark"),
  );
  await themePicker.selectOption("light");
  await page.waitForFunction(
    () => document.documentElement.dataset.theme === "light" &&
      document.querySelector(".monaco-editor")?.classList.contains("vs"),
  );
  await themePicker.selectOption("system");
  const initialWorkspaceHeight = await page.locator(".workspace").evaluate(
    element => element.getBoundingClientRect().height,
  );
  await page.waitForTimeout(1_500);
  const settledWorkspaceHeight = await page.locator(".workspace").evaluate(
    element => element.getBoundingClientRect().height,
  );
  if (Math.abs(settledWorkspaceHeight - initialWorkspaceHeight) > 1) {
    throw new Error(
      `Expected the workspace height to remain stable, but it grew from ` +
      `${initialWorkspaceHeight}px to ${settledWorkspaceHeight}px.`,
    );
  }
  if (settledWorkspaceHeight > 800) {
    throw new Error(`Expected a bounded desktop workspace, got ${settledWorkspaceHeight}px.`);
  }
  const pageDimensions = await page.evaluate(() => ({
    viewportHeight: window.innerHeight,
    documentHeight: Math.max(
      document.body.scrollHeight,
      document.documentElement.scrollHeight,
    ),
  }));
  if (pageDimensions.documentHeight > pageDimensions.viewportHeight + 1) {
    throw new Error(
      `Expected the desktop playground to fit without page scrolling, but its document is ` +
      `${pageDimensions.documentHeight}px tall in a ${pageDimensions.viewportHeight}px viewport.`,
    );
  }

  const initialSource = (await editor.locator(".view-lines").textContent()).replaceAll("\u00a0", " ");
  if (!initialSource.includes("Hello from $language in WebAssembly")) {
    throw new Error(`Expected Hello World to load on startup, got ${initialSource}.`);
  }

  await editor.click({ force: true });
  await page.keyboard.press(process.platform === "darwin" ? "Meta+A" : "Control+A");
  await page.keyboard.insertText('let shared = "Raven link"\nSystem.Console.WriteLine(shared)');
  await page.getByRole("button", { name: "Share", exact: true }).click();
  await page.waitForURL(url => url.searchParams.has("source"));
  const sharedUrl = page.url();
  const sharedPage = await browser.newPage();
  await sharedPage.goto(sharedUrl);
  await sharedPage.getByText("Ready", { exact: true }).waitFor({ timeout: 30_000 });
  const sharedEditor = sharedPage.locator(".monaco-editor");
  await sharedEditor.waitFor();
  const sharedSource = (await sharedEditor.locator(".view-lines").textContent())
    .replaceAll("\u00a0", " ");
  if (!sharedSource.includes("Raven link")) {
    throw new Error(`Expected shared source to load from the URL, got ${sharedSource}.`);
  }
  const sharedSelection = (await sharedPage.locator(".example-picker-trigger").textContent()).trim();
  if (!sharedSelection.includes("Shared program")) {
    throw new Error(`Expected the shared program selector state, got '${sharedSelection}'.`);
  }
  await sharedPage.close();

  const invalidSharedPage = await browser.newPage();
  await invalidSharedPage.goto(`${url}?source=invalid!`);
  await invalidSharedPage.getByText("Ready", { exact: true }).waitFor({ timeout: 30_000 });
  const fallbackSource = (await invalidSharedPage.locator(".monaco-editor .view-lines").textContent())
    .replaceAll("\u00a0", " ");
  if (!fallbackSource.includes("Hello from $language in WebAssembly")) {
    throw new Error(`Expected an invalid share URL to load Hello World, got ${fallbackSource}.`);
  }
  await invalidSharedPage.close();

  const tokenClasses = await editor.locator(".view-lines span[class]").evaluateAll(elements =>
    [...new Set(elements.map(element => element.className).filter(className => /^mtk\d+$/.test(className)))],
  );
  if (tokenClasses.length < 2) {
    throw new Error(`Expected TextMate highlighting to produce multiple token classes, got ${tokenClasses}.`);
  }

  await editor.click({ force: true });
  await page.keyboard.press(process.platform === "darwin" ? "Meta+A" : "Control+A");
  await page.keyboard.type("System.Console.Wri");
  const writeLineSuggestion = page.locator(".suggest-widget .monaco-list-row", {
    hasText: "WriteLine",
  });
  try {
    await writeLineSuggestion.first().waitFor({ timeout: 30_000 });
  } catch (error) {
    throw new Error(
      `WriteLine completion did not appear.\nBrowser errors:\n${browserErrors.join("\n") || "<none>"}`,
      { cause: error },
    );
  }
  await writeLineSuggestion.first().dblclick();
  await page.waitForTimeout(100);
  const completedSource = await editor.locator(".view-lines").textContent();
  if (!completedSource.includes("WriteLine")) {
    throw new Error(`Expected accepting completion to insert WriteLine, got ${completedSource}.`);
  }

  await page.keyboard.press("Escape");
  await editor.click({ force: true });
  await page.keyboard.press(process.platform === "darwin" ? "Meta+A" : "Control+A");
  await page.keyboard.type("ret");
  await page.keyboard.press("Control+Space");
  await page.locator(".suggest-widget .monaco-list-row", { hasText: "return" })
    .first()
    .waitFor({ timeout: 30_000 });

  const examplePicker = page.locator(".example-picker-trigger");
  const examples = await (await fetch(`${url}examples/index.json`)).json();
  if (examples.some(example => !example.category)) {
    throw new Error("Expected every example to belong to a category.");
  }
  await examplePicker.click();
  const exampleDialog = page.getByRole("dialog", { name: "Choose an example" });
  await exampleDialog.getByRole("heading", { name: "Basics" }).waitFor();
  await exampleDialog.getByRole("heading", { name: "Unions and patterns" }).waitFor();
  const exampleSearch = exampleDialog.getByRole("searchbox", { name: "Search examples" });
  await exampleSearch.fill("typestate");
  const filteredOptions = exampleDialog.getByRole("option");
  if (await filteredOptions.count() !== 1 ||
      !(await filteredOptions.first().textContent()).includes("State-safe connections")) {
    throw new Error("Expected example search to filter the grouped list.");
  }
  await exampleDialog.getByRole("button", { name: "Clear example search" }).click();
  if (await exampleDialog.getByRole("option").count() !== examples.length) {
    throw new Error("Expected clearing example search to restore every example.");
  }
  await page.keyboard.press("Escape");
  await exampleDialog.waitFor({ state: "hidden" });

  await page.getByRole("link", { name: "Documentation" }).waitFor();
  await page.getByRole("link", { name: "GitHub" }).waitFor();
  for (const example of examples) {
    await examplePicker.click();
    await page.getByRole("dialog", { name: "Choose an example" })
      .getByRole("option", { name: example.title, exact: true })
      .click();
    await page.waitForFunction(
      () => !new URL(window.location.href).searchParams.has("source"),
      { timeout: 30_000 },
    );
    await page.getByRole("button", { name: /^Run/ }).click();
    await page.waitForFunction(
      () => ["Complete", "Compile error", "Runtime error"].includes(
        document.querySelector(".status-pill")?.textContent?.trim(),
      ),
      { timeout: 30_000 },
    );
    const exampleStatus = (await page.locator(".status-pill").textContent())?.trim();
    if (exampleStatus !== "Complete") {
      const output = await page.locator(".output-panel").textContent();
      throw new Error(
        `Expected example '${example.id}' to run, got ${exampleStatus}: ${output}`,
      );
    }
  }

  await page.keyboard.press("Escape");
  await editor.click({ force: true });
  await page.keyboard.press(process.platform === "darwin" ? "Meta+A" : "Control+A");
  await page.keyboard.type(
    'import System.*\n\nlet greeting = "Hello from Raven in WebAssembly"\nConsole.WriteLine(greeting)',
  );
  await page.getByRole("button", { name: /Compile/ }).click();
  await page.waitForFunction(
    () => document.querySelector(".status-pill")?.textContent?.trim() !== "Compiling",
    { timeout: 30_000 },
  );
  const resultStatus = (await page.locator(".status-pill").textContent())?.trim();
  if (resultStatus !== "Compiled") {
    const diagnostics = await page.locator(".diagnostics li").allTextContents();
    throw new Error(`Expected greeting source to compile, got ${resultStatus}: ${diagnostics.join("\n")}`);
  }
  await page.getByText(/Compiled successfully/).waitFor();

  await page.getByRole("button", { name: /^Run/ }).click();
  await page.getByText("Complete", { exact: true }).waitFor({ timeout: 30_000 });
  await page.getByText("Hello from Raven in WebAssembly", { exact: true }).waitFor();

  await editor.click({ force: true });
  await page.keyboard.press(process.platform === "darwin" ? "Meta+A" : "Control+A");
  await page.keyboard.type("let =");
  await page.getByRole("button", { name: /Compile/ }).click();
  await page.getByText("Compile error", { exact: true }).waitFor({ timeout: 30_000 });
  if (await page.locator(".diagnostics li").count() === 0) {
    throw new Error("Expected invalid Raven source to produce at least one diagnostic.");
  }

  await editor.click({ force: true });
  await page.keyboard.press(process.platform === "darwin" ? "Meta+A" : "Control+A");
  await page.keyboard.insertText(
    [
      "import System.*",
      "",
      "func test<T>(value: T) -> Result<T, CustomError> {",
      "    return Ok(value)",
      "}",
      "",
      "Console.WriteLine(test<int>(42))",
      "",
      "record class CustomError()",
    ].join("\n"),
  );
  await page.getByRole("button", { name: /^Run/ }).click();
  await page.waitForFunction(
    () => ["Complete", "Runtime error"].includes(
      document.querySelector(".status-pill")?.textContent?.trim(),
    ),
    { timeout: 30_000 },
  );
  const resultRecordStatus = (await page.locator(".status-pill").textContent())?.trim();
  if (resultRecordStatus !== "Complete") {
    const diagnostics = await page.locator(".diagnostics li").allTextContents();
    const output = await page.locator(".output-panel").textContent();
    throw new Error(
      `Expected Result/record source to run, got ${resultRecordStatus}: ${diagnostics.join("\n")}\n${output}`,
    );
  }
  await page.getByText("Result<Int32>.Ok(42)", { exact: true }).waitFor();

  await editor.click({ force: true });
  await page.keyboard.press(process.platform === "darwin" ? "Meta+A" : "Control+A");
  await page.keyboard.type(
    'let greeting = "Hello from Raven in WebAssembly"\nSystem.Console.WriteLine(greeting)',
  );
  await page.getByRole("button", { name: /^Run/ }).click();
  await page.getByText("Complete", { exact: true }).waitFor({ timeout: 30_000 });
  await page.getByText("Hello from Raven in WebAssembly", { exact: true }).waitFor();

  const finalWorkspaceHeight = await page.locator(".workspace").evaluate(
    element => element.getBoundingClientRect().height,
  );
  if (Math.abs(finalWorkspaceHeight - settledWorkspaceHeight) > 1) {
    throw new Error(
      `Expected editor activity not to resize the workspace, but it changed from ` +
      `${settledWorkspaceHeight}px to ${finalWorkspaceHeight}px.`,
    );
  }

  if (browserErrors.length > 0) {
    throw new Error(`Browser errors:\n${browserErrors.join("\n")}`);
  }

  console.log("Playground browser smoke test passed.");
} finally {
  await browser.close();
  await new Promise(resolveClose => server.close(resolveClose));
}

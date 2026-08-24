#!/usr/bin/env node

import { execFileSync } from "node:child_process";
import { readFileSync, readdirSync, statSync, writeFileSync } from "node:fs";
import { dirname, join, relative, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";
import { nextUnreleasedVersion } from "./site-provenance-version.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(scriptDirectory, "..");
const siteRoot = resolve(process.argv[2] ?? join(repositoryRoot, "_site"));

if (!statSync(siteRoot).isDirectory()) {
  throw new Error(`Site directory does not exist: ${siteRoot}`);
}

function git(...args) {
  return execFileSync("git", ["-C", repositoryRoot, ...args], { encoding: "utf8" }).trim();
}

function hasTag(version) {
  try {
    execFileSync("git", ["-C", repositoryRoot, "rev-parse", "--verify", "--quiet", `refs/tags/v${version}`], {
      stdio: "ignore",
    });
    return true;
  } catch {
    return false;
  }
}

function determineVersion() {
  const exactTags = git("tag", "--points-at", "HEAD", "--list", "v[0-9]*")
    .split("\n")
    .filter(Boolean)
    .sort((left, right) => left.localeCompare(right, undefined, { numeric: true }));

  if (exactTags.length > 0) {
    return { version: exactTags.at(-1).slice(1), status: "released" };
  }

  const configuredVersion = process.env.RAVEN_SITE_VERSION
    ?? JSON.parse(readFileSync(join(repositoryRoot, "global.json"), "utf8"))["msbuild-sdks"]?.["Raven.Sdk"];

  if (!configuredVersion) {
    throw new Error("Could not determine the Raven version from RAVEN_SITE_VERSION or global.json.");
  }

  let version = configuredVersion.replace(/-local\..*$/, "");
  if (hasTag(version)) {
    version = nextUnreleasedVersion(version);
  }

  if (!/^\d+\.\d+\.\d+(?:-[0-9A-Za-z.-]+)?$/.test(version)) {
    throw new Error(`Invalid Raven site version: ${version}`);
  }

  return { version, status: "unreleased" };
}

function allHtmlFiles(directory) {
  return readdirSync(directory, { withFileTypes: true }).flatMap(entry => {
    const path = join(directory, entry.name);
    return entry.isDirectory()
      ? allHtmlFiles(path)
      : entry.isFile() && entry.name.endsWith(".html")
        ? [path]
        : [];
  });
}

const commit = git("rev-parse", "HEAD");
const shortCommit = git("rev-parse", "--short=9", "HEAD");
const dirty = git("status", "--porcelain", "--untracked-files=no").length > 0;
const { version, status } = determineVersion();
const provenance = { version, status, commit, shortCommit, dirty };
const commitUrl = `https://github.com/marinasundstrom/raven/commit/${commit}`;

writeFileSync(join(siteRoot, "site-build.json"), `${JSON.stringify(provenance, null, 2)}\n`);

const clientScript = `(() => {
  const build = ${JSON.stringify(provenance)};
  const commitUrl = ${JSON.stringify(commitUrl)};

  function addProvenance() {
    const footers = document.querySelectorAll("footer");
    const footer = footers.item(footers.length - 1);
    if (!footer || footer.querySelector("[data-raven-build]")) return Boolean(footer);

    const container = footer.querySelector(".flex-fill") ?? footer;
    const label = document.createElement("span");
    label.dataset.ravenBuild = "";
    label.style.cssText = "display:inline-flex;flex-wrap:wrap;gap:.4em;align-items:center;margin-inline-start:auto;font:500 .75rem/1.4 ui-monospace,SFMono-Regular,Menlo,monospace;letter-spacing:normal;text-transform:none;opacity:.82";
    label.append("Raven " + build.version + (build.status === "unreleased" ? " (unreleased)" : "") + " · ");

    const link = document.createElement("a");
    link.href = commitUrl;
    link.textContent = build.shortCommit;
    link.title = "Source commit " + build.commit;
    link.style.cssText = "color:inherit;text-decoration:underline;text-underline-offset:.2em";
    label.append(link);
    if (build.dirty) label.append(" · uncommitted changes");
    container.append(label);
    return true;
  }

  if (!addProvenance()) {
    const observer = new MutationObserver(() => {
      if (addProvenance()) observer.disconnect();
    });
    observer.observe(document.body, { childList: true, subtree: true });
  }
})();
`;

writeFileSync(join(siteRoot, "site-build.js"), clientScript);

const markerPattern = /\n?\s*<script\s+defer\s+data-raven-site-provenance\s+src="[^"]+"><\/script>/g;
for (const htmlFile of allHtmlFiles(siteRoot)) {
  let html = readFileSync(htmlFile, "utf8").replace(markerPattern, "");
  let scriptPath = relative(dirname(htmlFile), join(siteRoot, "site-build.js")).split(sep).join("/");
  if (!scriptPath.startsWith(".")) scriptPath = `./${scriptPath}`;

  const scriptElement = `  <script defer data-raven-site-provenance src="${scriptPath}"></script>\n`;
  if (!html.includes("</body>")) continue;

  html = html.replace("</body>", `${scriptElement}</body>`);
  writeFileSync(htmlFile, html);
}

console.log(`Added Raven ${version} (${status}) provenance for ${shortCommit}${dirty ? " with uncommitted changes" : ""}.`);

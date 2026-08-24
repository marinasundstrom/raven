#!/usr/bin/env node

import assert from "node:assert/strict";
import { nextUnreleasedVersion } from "./site-provenance-version.mjs";

assert.equal(nextUnreleasedVersion("0.1.0"), "0.1.1-preview.1");
assert.equal(nextUnreleasedVersion("0.1.0-preview.14"), "0.1.0-preview.15");
assert.throws(
  () => nextUnreleasedVersion("0.1.0-rc.1"),
  /next preview cannot be inferred/,
);

console.log("Site provenance version checks passed.");

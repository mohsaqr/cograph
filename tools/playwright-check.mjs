#!/usr/bin/env node
// Playwright driver for the cograph visual-sweep contact sheet.
//
// Two modes:
//   screenshot <url> [--out path]
//     Headless Chromium, waits for window.__sweepReady, takes full-page PNG.
//   verify <url>
//     Same boot, then reads data-ratio-ok from every cell. Exits non-zero
//     if any cell is red (|Δ| > 5%).
//
// Requires `npx playwright install chromium` to have been run once.
// No package.json, no node_modules needed at the repo root — npx handles it.
//
// Usage (from repo root):
//   node tools/playwright-check.mjs screenshot file://$PWD/tmp/visual-sweep/.../index.html
//   node tools/playwright-check.mjs screenshot file://$PWD/tmp/visual-sweep/.../index.html --out ./tmp/visual-sweep/contact-sheet.png
//   node tools/playwright-check.mjs verify     file://$PWD/tmp/visual-sweep/.../index.html

import { chromium } from 'playwright';
import { writeFile } from 'node:fs/promises';
import path from 'node:path';

const [, , mode, rawUrl, ...rest] = process.argv;
if (!mode || !rawUrl) {
  console.error("usage: playwright-check.mjs <screenshot|verify> <url> [--out path]");
  process.exit(2);
}

let outPath = null;
for (let i = 0; i < rest.length; i++) {
  if (rest[i] === '--out' && rest[i + 1]) {
    outPath = path.resolve(rest[++i]);
  }
}

// Local file:// URLs must be absolute. Accept a bare path for convenience.
let url = rawUrl;
if (!/^[a-z]+:\/\//i.test(url)) {
  url = 'file://' + path.resolve(url);
}

console.log(`[playwright] ${mode}  ${url}`);

const browser = await chromium.launch({ headless: true });
const context = await browser.newContext({
  viewport: { width: 1600, height: 1200 }
});
const page = await context.newPage();

// Surface client-side errors.
page.on('pageerror', (err) => console.error('[page]', err));
page.on('console', (msg) => {
  if (msg.type() === 'error') console.error('[console]', msg.text());
});

await page.goto(url, { waitUntil: 'load' });
await page.waitForFunction(() => window.__sweepReady === true, null,
                           { timeout: 30_000 });

if (mode === 'screenshot') {
  const out = outPath || path.resolve(path.dirname(url.replace(/^file:\/\//, '')),
                                      'contact-sheet.png');
  await page.screenshot({ path: out, fullPage: true });
  console.log(`[playwright] wrote ${out}`);
  await browser.close();
  process.exit(0);
}

if (mode === 'verify') {
  const cells = await page.$$eval('figure.cell', els => els.map(el => ({
    pf: el.dataset.pf,
    renderer: el.dataset.renderer,
    ok: el.dataset.ratioOk === 'true',
    delta: parseFloat(el.dataset.delta)
  })));
  let fail = 0;
  for (const c of cells) {
    const status = c.ok ? 'ok' : 'not ok';
    const delta = Number.isFinite(c.delta) ? `${c.delta.toFixed(2)}%` : 'n/a';
    console.log(`${status}  ${c.pf}  ${c.renderer}  delta=${delta}`);
    if (!c.ok) fail++;
  }
  console.log(`# ${cells.length - fail} / ${cells.length} passing`);
  await browser.close();
  process.exit(fail > 0 ? 1 : 0);
}

console.error(`unknown mode: ${mode}`);
await browser.close();
process.exit(2);

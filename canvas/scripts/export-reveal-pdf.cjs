const path = require("path");
const { pathToFileURL } = require("url");
const { chromium } = require("playwright-core");

async function main() {
  const [htmlPath, pdfPath, browserPath] = process.argv.slice(2);
  if (!htmlPath || !pdfPath || !browserPath) {
    throw new Error("Usage: export-reveal-pdf.cjs <html> <pdf> <browser>");
  }

  const browser = await chromium.launch({
    executablePath: browserPath,
    headless: true,
    args: ["--allow-file-access-from-files"],
  });
  try {
    const page = await browser.newPage({ viewport: { width: 1050, height: 700 } });
    const url = `${pathToFileURL(path.resolve(htmlPath)).href}?print-pdf`;
    await page.goto(url, { waitUntil: "load", timeout: 60000 });
    await page.waitForFunction(
      () =>
        window.Reveal &&
        (typeof window.Reveal.isReady !== "function" || window.Reveal.isReady()),
      null,
      { timeout: 60000 },
    );
    await page.waitForTimeout(1500);
    await page.pdf({
      path: path.resolve(pdfPath),
      format: "Letter",
      landscape: true,
      margin: { top: 0, right: 0, bottom: 0, left: 0 },
      printBackground: true,
      preferCSSPageSize: false,
      tagged: true,
      outline: true,
    });
  } finally {
    await browser.close();
  }
}

main().catch((error) => {
  console.error(error.stack || error.message || String(error));
  process.exitCode = 1;
});

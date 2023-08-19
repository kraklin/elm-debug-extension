import { defineConfig } from "vite";
import webExtension, { readJsonFile } from "vite-plugin-web-extension";
import elmPlugin from "vite-plugin-elm"
import zipPlugin from "vite-plugin-zip-pack"

const browser = process.env.TARGET || "chrome";
const pkg = readJsonFile("package.json");
const outputDir = `dist/${browser}`;
const bundleExtension = browser === 'firefox' ? '.xpi' : '.zip'
const bundleFileName = `${pkg.name}-${pkg.version}-${browser}${bundleExtension}`;

function generateManifest() {
  const manifest = readJsonFile("manifest.json");
  return {
    description: pkg.description,
    version: pkg.version,
    ...manifest,
  };
}

export default defineConfig({
  build: {
    minify: false,
    outDir: outputDir
  },
  plugins: [
    elmPlugin.plugin(),
    webExtension({
      manifest: generateManifest,
      browser: browser,
      watchFilePaths: ["package.json", "manifest.json"],
    }),
    zipPlugin({inDir: outputDir,
    outDir: 'dist/bundles',
    outFileName: bundleFileName})
  ],
});

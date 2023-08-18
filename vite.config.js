import { defineConfig } from "vite";
import webExtension, { readJsonFile } from "vite-plugin-web-extension";
import elmPlugin from "vite-plugin-elm"

const browser = process.env.TARGET || "chrome";

function generateManifest() {
  const manifest = readJsonFile("manifest.json");
  const pkg = readJsonFile("package.json");
  return {
    description: pkg.description,
    version: pkg.version,
    ...manifest,
  };
}

export default defineConfig({
  build: {
    minify: false
  },
  plugins: [
    elmPlugin.plugin(),
    webExtension({
      manifest: generateManifest,
      browser: browser,
      watchFilePaths: ["package.json", "manifest.json"],
    }),
  ],
});

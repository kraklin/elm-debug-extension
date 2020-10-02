<h1 align="center">Elm debug extension</h1>
<p align="center">Browser extension, that is making your elm Debug.log output much easier to work with.</p>
<div align="center">
  <a href="https://github.com/kraklin/elm-debug-extension/blob/master/LICENSE">
    <img src="https://img.shields.io/github/kraklin/elm-debug-extension.svg" alt="LICENSE" />
  </a>
</div>
<p align="center">❤️  Love it? ⭐️ Star it on <a href="https://github.com/kraklin/elm-debug-extension/stargazers">GitHub</a></p>
<hr />

## Features

- Transforms Debug.log string from Elm into collapsible object in browser console
- Optionally uses Custom Formatter for Chrome-based browsers for better visualisation

## Browser Support

| [![Chrome](https://raw.github.com/alrra/browser-logos/master/src/chrome/chrome_48x48.png)](/) | [![Firefox](https://raw.github.com/alrra/browser-logos/master/src/firefox/firefox_48x48.png)](/) | [![Opera](https://raw.github.com/alrra/browser-logos/master/src/opera/opera_48x48.png)](/) | [![Edge](https://raw.github.com/alrra/browser-logos/master/src/edge/edge_48x48.png)](/) | [![Yandex](https://raw.github.com/alrra/browser-logos/master/src/yandex/yandex_48x48.png)](/) | [![Brave](https://raw.github.com/alrra/browser-logos/master/src/brave/brave_48x48.png)](/) | [![vivaldi](https://raw.github.com/alrra/browser-logos/master/src/vivaldi/vivaldi_48x48.png)](/) |
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| 49 & later ✔ | 52 & later ✔ | 36 & later ✔ | 79 & later ✔ | Latest ✔ | Latest ✔ | Latest ✔

### Development

- `yarn install` to install dependencies.
- To watch file changes in developement

  - Chrome
    - `yarn run dev:chrome`
  - Firefox
    - `yarn run dev:firefox`
  - Opera
    - `yarn run dev:opera`

- **Load extension in browser**

- ### Chrome

  - Go to the browser address bar and type `chrome://extensions`
  - Check the `Developer Mode` button to enable it.
  - Click on the `Load Unpacked Extension…` button.
  - Select your extension’s extracted directory.

- ### Firefox

  - Load the Add-on via `about:debugging` as temporary Add-on.
  - Choose the `manifest.json` file in the extracted directory

- ### Opera

  - Load the extension via `opera:extensions`
  - Check the `Developer Mode` and load as unpacked from extension’s extracted directory.

## Bugs

Please file an issue [here](https://github.com/kraklin/elm-debug-extension/issues/new) for bugs, missing documentation, or unexpected behavior.

## Kudos

Built on top of awesome [web-extension-starter](https://github.com/abhijithvijayan/web-extension-starter/) by [Abhijith Vijayan](https://github.com/abhijithvijayan)

## License

MIT © [Tomas Latal](https://github.com/kraklin)

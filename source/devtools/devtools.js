import browser from 'webextension-polyfill';

browser.devtools.panels.create(
  "Elm Debug",                      // title
  "assets/icons/favicon-dark-32.png",                // icon
  "devtools.html"      // content
);

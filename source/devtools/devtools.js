import browser from 'webextension-polyfill';

//TODO: extract to some global constants file
const globalStorageKey = "globalOptions";

browser.storage.sync.get([globalStorageKey]).then((result) => {
  let globalOptions = result[globalStorageKey];

  if(globalOptions === undefined || !Boolean(globalOptions.devPanel)){
    return;
  }

  browser.devtools.panels.create(
    "Elm Debug",
    "assets/icons/favicon-dark-32.png",
    "devtools.html"
  ).then((newPanel) => {
    browser.runtime.sendMessage({
      action: "ELM_DEVTOOL_CREATED",
      data: String(browser.devtools.inspectedWindow.tabId)});
  });
})

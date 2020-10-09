/* eslint no-console: "off" */
import browser from 'webextension-polyfill';
import {urlToKey} from './helpers.js'

const globalStorageKey = "globalOptions";
const storageKey = urlToKey(location.href);

// try to get stored configuration for current tab
browser.storage.sync.get([globalStorageKey, storageKey]).then((result) => {
  const savedOptions = result[storageKey];
  let globalOptions = result[globalStorageKey];

  if (globalOptions === undefined) {
    globalOptions = {limit: 10000000, debug: false, simple_mode: true}
    browser.storage.sync.set({[globalStorageKey]: globalOptions});
  }

  // ---------- MESSAGE HANDLING ------------

  window.addEventListener(
    'message',
    (event) => {
      // We only accept messages from ourselves
      if (event.source !== window) return;

      if (event.data.type && event.data.type === 'ELM_LOG') {
        browser.runtime.sendMessage({ action: "ELM_LOG",  data: event.data.message});
      }
    },
    false
  );

  // handle messages for settings
  browser.runtime.onMessage.addListener((request) => {
    switch (request.action) {
      case 'TOGGLE_ACTIVE':
        options.active = !options.active;

        checkInjectAndRegister();
        setIcon(options.active);
        saveToStorage({active: options.active});

        break;
      default:
    }

    return new Promise((resolve) => {
      resolve({opts: options});
    });
  });
});

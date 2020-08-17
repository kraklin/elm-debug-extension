/* eslint no-console: "off" */
import {register} from 'elm-debug-transformer';
import browser from 'webextension-polyfill';
import {urlToKey} from './helpers.js'

const storageKey = urlToKey(location.href);

// try to get stored configuration for current tab 
browser.storage.sync.get(storageKey).then((result) => {

  // ------- SCRIPT INJECTION -------------

  const injectScript = (func) => {
    // eslint-disable-next-line prefer-template
    const actualCode = '(' + func + ')();';

    const script = document.createElement('script');
    script.textContent = actualCode;
    (document.head || document.documentElement).appendChild(script);
    script.remove();
  };

  injectScript(() => {
    if (window.console && console.log) {
      const old = console.log;
      console.log = (...args) => {
        if (!!args && args.length === 1) {
          window.postMessage({type: 'ELM_LOG', message: args[0]});
        } else {
          old.apply(console, args);
        }
      };
    }
  });

  // -------- ELM-DEBUG-TRANSFORM SETTINGS ------------

  const savedOptions = result[storageKey];
  let optionsValues = {active: false, limit: 10000000}

  if(savedOptions !== undefined) {
    optionsValues = {active: savedOptions.active}
  }

  const options = register(optionsValues);

  const saveToStorage = (optionsToSave) => {
    browser.storage.sync.set({[storageKey]: optionsToSave});
  }

  const setIcon = (isActive) => {
    browser.runtime.sendMessage({ action: "SET_ICON",  active: isActive});
  }

  // set initial icon
  setIcon(optionsValues.active);

  // ---------- MESSAGE HANDLING ------------

  window.addEventListener(
    'message',
    (event) => {
      // We only accept messages from ourselves
      if (event.source !== window) return;

      if (event.data.type && event.data.type === 'ELM_LOG') {
        console.log(event.data.message);
      }
    },
    false
  );

  // handle messages for settings
  browser.runtime.onMessage.addListener((request) => {
    switch (request.action) {
      case 'TOGGLE_ACTIVE':
        options.active = !options.active;
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

/* eslint no-console: "off" */
import {register} from 'elm-debug-transformer';
import browser from 'webextension-polyfill';
import {urlToKey} from './helpers.js'

const storageKey = urlToKey(location.href);

// try to get stored configuration for current tab 
browser.storage.sync.get(storageKey).then((result) => {

  const savedOptions = result[storageKey];

  // ------- SCRIPT INJECTION -------------

  let scriptInjected = false;
  const addScriptToPage = (func) => {
    // eslint-disable-next-line prefer-template
    const actualCode = '(' + func + ')();';

    const script = document.createElement('script');
    script.textContent = actualCode;
    (document.head || document.documentElement).appendChild(script);
    script.remove();
  };

  const injectScript = () => {
      addScriptToPage(() => {
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

      scriptInjected = true;
  }

  // -------- ELM-DEBUG-TRANSFORM SETTINGS ------------

  let options = {active: false, limit: 10000000}

  if(savedOptions !== undefined) {
    options = {active: savedOptions.active}
  }

  const checkInjectAndRegister = () => {
    if(options.active && !scriptInjected){
      injectScript();
      options = register(options);
    }
  }

  const saveToStorage = (optionsToSave) => {
    browser.storage.sync.set({[storageKey]: optionsToSave});
  }

  const setIcon = (isActive) => {
    browser.runtime.sendMessage({ action: "SET_ICON",  active: isActive});
  }

  // set initial icon
  setIcon(options.active);
  
  // check if you can inject the console.log catcher
  checkInjectAndRegister();

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

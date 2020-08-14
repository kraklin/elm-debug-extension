/* eslint no-console: "off" */
import {register} from 'elm-debug-transformer';
import browser from 'webextension-polyfill';

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

const options = register({active: false, limit: 1000000});

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
  console.log(request);

  switch (request.action) {
    case 'TOGGLE_ACTIVE':
      options.active = !options.active;
      break;
    default:
  }

  return new Promise((resolve) => {
    resolve({opts: options});
  });
});

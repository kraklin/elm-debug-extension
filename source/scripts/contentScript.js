/* eslint no-console: "off" */
import {register} from 'elm-debug-transformer';

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
        old.apply(this, args);
      }
    };
  }
});

register({limit: 1000000});

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

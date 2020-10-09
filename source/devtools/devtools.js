import browser from 'webextension-polyfill';

function handleShown() {
  console.log("panel is being shown");
}

function handleHidden() {
  console.log("panel is being hidden");
}


const f =
  `if (window.console && console.log && !window.__IS_CONSOLE_HIJACKED) {
          const old = console.log;
          console.log = (...args) => {
            if (!!args && args.length === 1) {
              window.postMessage({type: 'ELM_LOG', message: args[0]});
            }
            old.apply(console, args);
          };
        }

        window.__IS_CONSOLE_HIJACKED = true;
  `

const hijackConsole = () => {
    browser.devtools.inspectedWindow.eval(f)
        .then(v => {console.log('ok', v)})
        .catch(e => {console.log("err", e)});
}

browser.devtools.panels.create(
  "Elm Debug",                      // title
  "assets/icons/favicon-dark-32.png",                // icon
  "devtools.html"      // content
).then((newPanel) => {
  console.log("panel has been created", browser.devtools.inspectedWindow.tabId);
  newPanel.onShown.addListener(handleShown);
  newPanel.onHidden.addListener(handleHidden);

  hijackConsole();
});

browser.devtools.inspectedWindow.addEventListener();

const myPort = browser.runtime.connect({name: ''+browser.devtools.inspectedWindow.tabId});

myPort.onMessage.addListener((message) => {
  if(message === "reloaded"){
    hijackConsole();
  }
});


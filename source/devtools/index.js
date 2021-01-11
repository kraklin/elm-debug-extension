import browser from 'webextension-polyfill';
import {version} from '../../package.json';
import {Elm} from './Panel.elm';

// init
const theme = browser.devtools.panels.themeName || "light";

// elm part
const app = Elm.Panel.init({
  node: document.getElementById('debug-panel'),
  flags: { theme: theme }
});

const tabId = String(browser.devtools.inspectedWindow.tabId);
const myPort = browser.runtime.connect({name: tabId});

myPort.onMessage.addListener((message) => {
  if(message.action && message.action === "FLUSH"){
    app.ports.bulkLogReceived.send(message.data);
  }
  else {
    app.ports.logReceived.send([new Date().toISOString(), message]);
  }
});

window.addEventListener('beforeunload', function (e) {
  browser.runtime.sendMessage({action: "ELM_DEVTOOL_REMOVED", data: tabId});
});


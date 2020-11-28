import browser from 'webextension-polyfill';
import {version} from '../../package.json';
import {isChromeBased} from '../scripts/helpers.js';
import {Elm} from './Panel.elm';
import {parse} from 'elm-debug-transformer';

// init
const theme = window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';

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

app.ports.bulkParse.subscribe((valuesToParse) => {
  // List (message, count)
  const parsedValues = valuesToParse.map(([{log, hash, time}, count]) => {
    return {
      log: parse(log),
      hash: hash,
      timestamp: time,
      count: count
      };
  });

  app.ports.bulkParsedReceived.send(parsedValues);
});

app.ports.parse.subscribe((valueToParse) => {
  const hash = valueToParse[0];
  const message = valueToParse[1];

  const parsedLog = parse(message);
  app.ports.parsedReceived.send({
    hash: hash,
    log: parsedLog,
    timestamp: new Date().toISOString()
  });
});

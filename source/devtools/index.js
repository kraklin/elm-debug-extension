import browser from 'webextension-polyfill';
import {version} from '../../package.json';
import {isChromeBased} from '../scripts/helpers.js';
import {Elm} from './Panel.elm';
import {parse} from 'elm-debug-transformer';

// init

// elm part
const app = Elm.Panel.init({
  node: document.getElementById('debug-panel'),
  flags: { }
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
  const parsedValues = valuesToParse.map((value) => {
    return {
      log: parse(value[0].log),
      hash: value[0].hash,
      timestamp: value[0].time,
      count: value[1]
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

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

const myPort = browser.runtime.connect({name: ''+browser.devtools.inspectedWindow.tabId});

myPort.onMessage.addListener((message) => {
  if(message !== "reloaded"){
    app.ports.logReceived.send(message);
  }
});

app.ports.parse.subscribe((valueToParse) => {
  const hash = valueToParse[0];
  const message = valueToParse[1];

  const parsedLog = parse(message);
  console.log(parsedLog);
  app.ports.parsedReceived.send({hash: hash, log: parsedLog});
});

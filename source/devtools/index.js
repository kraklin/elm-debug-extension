import browser from 'webextension-polyfill';
import {version} from '../../package.json';
import {isChromeBased} from '../scripts/helpers.js';
import {Elm} from './Panel.elm';


const app = Elm.Panel.init({
  node: document.getElementById('debug-panel'),
  flags: { }
});

const myPort = browser.runtime.connect({name: ''+browser.devtools.inspectedWindow.tabId});

myPort.onMessage.addListener((message) => {
  app.ports.logReceived.send(message);
});


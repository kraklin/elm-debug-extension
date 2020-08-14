import browser from 'webextension-polyfill';
import {Elm} from './Main.elm';

const app = Elm.Main.init({
  node: document.getElementById('elm-popup'),
});

app.ports.sendRequest.subscribe((request) => {
  send(request);
}) 

function openWebPage(url) {
  return browser.tabs.create({url});
}

function handleError(error) {
  console.error('Error:', error.message);
}

const send = async (request) => {
  const tabs = await browser.tabs.query({
    active: true,
    currentWindow: true,
  });

  const sending = browser.tabs.sendMessage(tabs[0].id, request);

  sending.then((message) => {
    app.ports.receive.send(message.opts); 
  }, handleError);
}

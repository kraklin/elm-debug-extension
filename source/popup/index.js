import browser from 'webextension-polyfill';
import {version} from '../../package.json'
import {Elm} from './Popup.elm';

browser.tabs.query({active: true, currentWindow: true}).then((tabs) => {
  const currentTab = tabs[0];

  const app = Elm.Popup.init({
    node: document.getElementById('elm-popup'),
    flags: {version: version}
  });

  app.ports.sendRequest.subscribe((request) => {
    send(request);
  })

  app.ports.openOptionsPage.subscribe(() => {
    browser.runtime.openOptionsPage();
  });

  function handleError(error) {
    console.error('Error:', error.message);
  }

  const send = async (request) => {
    const sending = browser.tabs.sendMessage(currentTab.id, request);

    sending.then((message) => {
      if(message && message.opts){
        app.ports.receive.send(message.opts);
      }
    }, handleError);
  }
});

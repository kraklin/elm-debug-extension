import browser from 'webextension-polyfill';

function openWebPage(url) {
  return browser.tabs.create({url});
}

function handleError(error) {
  console.log(`Error: ${error}`);
}

const setStatus = (status) => {
  document.getElementById('status').innerHTML = status ? 'ON' : 'OFF'
};


const send = async (request) => {
  const tabs = await browser.tabs.query({
    active: true,
    currentWindow: true,
  });

  const sending = browser.tabs.sendMessage(tabs[0].id, request);

  sending.then((message) => {
    setStatus(message.response.opts.active);

  }, handleError);
}


document.addEventListener('DOMContentLoaded', async () => {
  send({action: 'GET_STATUS'})

  /*
  document.getElementById('packages_button').addEventListener('click', () => {
    return openWebPage('https://package.elm-lang.org');
  });
  */

  document.getElementById('toggle_button').addEventListener('click', () => {
    send({action: 'TOGGLE_ACTIVE'});
  });
});

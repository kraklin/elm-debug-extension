import browser from 'webextension-polyfill';

function openWebPage(url) {
  return browser.tabs.create({url});
}

document.addEventListener('DOMContentLoaded', async () => {
  const tabs = await browser.tabs.query({
    active: true,
    lastFocusedWindow: true,
  });

  /*
  const url = tabs.length && tabs[0].url;

  const response = await browser.runtime.sendMessage({
    msg: 'hello',
    url,
  });

  // eslint-disable-next-line no-console
  console.emoji('🦄', response);
  */

  document.getElementById('packages_button').addEventListener('click', () => {
    return openWebPage('https://package.elm-lang.org');
  });

  document.getElementById('ellie_button').addEventListener('click', () => {
    return openWebPage('https://ellie-app.com');
  });
});

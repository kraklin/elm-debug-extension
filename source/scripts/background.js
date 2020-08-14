import browser from 'webextension-polyfill';

browser.runtime.onInstalled.addListener(() => {
  // on installed action - e.g. how to turn on the Custom formatter extension
});

browser.runtime.onMessage.addListener((_request, _sender, _sendResponse) => {
  // Do something with the message!
  // alert(request.url);

  // And respond back to the sender.
  return Promise.resolve('got your message, thanks!');
});

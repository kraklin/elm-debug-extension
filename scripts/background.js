import browser from 'webextension-polyfill';

const setIcon = (tabId, isActive) => {
  const onIconPath = {
    "16": "/icon/favicon-16.png",
    "32": "/icon/favicon-32.png",
    "96": "/icon/favicon-96.png",
    "128": "/icon/favicon-128.png"
  };
  const offIconPath = {
    "16": "/icon/favicon-dark-16.png",
    "32": "/icon/favicon-dark-32.png",
    "96": "/icon/favicon-dark-96.png",
    "128": "/icon/favicon-dark-128.png"
  };

  let newPath = offIconPath;
  if (isActive) {
    newPath = onIconPath;
  }

  browser.browserAction.setIcon({
    path: newPath,
    tabId: tabId
  });
}

browser.runtime.onInstalled.addListener(() => {
  // on installed action - e.g. how to turn on the Custom formatter extension
});

browser.runtime.onMessage.addListener((_request, _sender, _sendResponse) => {
  // Do something with the message!
  // alert(request.url);

  // And respond back to the sender.
  if(_request.action === "SET_ICON"){
    setIcon(_sender.tab.id, _request.active)
  }

  return;// Promise.resolve('got your message, thanks!');
});

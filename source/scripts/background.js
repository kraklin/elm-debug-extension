import browser from 'webextension-polyfill';

const setIcon = (tabId, isActive) => {
  const onIconPath = {
    "16": "assets/icons/favicon-16.png",
    "32": "assets/icons/favicon-32.png",
    "96": "assets/icons/favicon-96.png",
    "128": "assets/icons/favicon-128.png"
  };
  const offIconPath = {
    "16": "assets/icons/favicon-dark-16.png",
    "32": "assets/icons/favicon-dark-32.png",
    "96": "assets/icons/favicon-dark-96.png",
    "128": "assets/icons/favicon-dark-128.png"
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

// handle on connect

let ports = {}

browser.runtime.onConnect.addListener((port)=>{
  if(!!port.name){
    ports[port.name] = port;
  }
});

browser.runtime.onInstalled.addListener(() => {
  // on installed action - e.g. how to turn on the Custom formatter extension
});

browser.runtime.onMessage.addListener((_request, _sender, _sendResponse) => {
  // Do something with the message!
  // alert(request.url);
  if(_request.action === "ELM_LOG"){
    const senderId = _sender.tab.id+'';
    if(ports[senderId]){
      ports[senderId].postMessage(_request.data);
    }
  }

  // And respond back to the sender.
  if(_request.action === "SET_ICON"){
    setIcon(_sender.tab.id, _request.active)
  }

  return;// Promise.resolve('got your message, thanks!');
});

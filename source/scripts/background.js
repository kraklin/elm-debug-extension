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

const bufferLimit = 30;

// maybe handle this in one object per page id would be better
let ports = {}
let comms = {}
let buffer = {}

browser.runtime.onConnect.addListener((port)=>{
  if(!!port.name){
    ports[port.name] = port;
    comms[port.name] = true;

    if(buffer[port.name]?.length > 0){
      port.postMessage({action: "FLUSH", data: buffer[port.name]});
      buffer[port.name] = new Array();
    }
  }
});

browser.runtime.onInstalled.addListener(() => {
  // on installed action - e.g. how to turn on the Custom formatter extension
});

browser.runtime.onMessage.addListener((_request, _sender, _sendResponse) => {
  console.log("request", _request);

  // Do something with the message!
  if(_request.action === "ELM_DEVTOOL_CREATED"){
    // is this useful somehow?
  }

  if(_request.action === "ELM_DEVTOOL_REMOVED"){
    comms[_request.data] = false;
    buffer[_request.data] = new Array();
  }

  if(_request.action === "ELM_LOG"){
    const senderId = String(_sender.tab.id);

    if(comms[senderId] === true){
      if(ports[senderId]){
        ports[senderId].postMessage(_request.data);
      }
    }
    else {
      if(buffer[senderId] === undefined){
        buffer[senderId] = new Array();
      }

      buffer[senderId].push({time: new Date().toISOString(), log: _request.data});

      // cap the buffer
      if (buffer[senderId].length > bufferLimit) {
        buffer[senderId].shift();
      }
    }
  }

  // And respond back to the sender.
  if(_request.action === "SET_ICON"){
    setIcon(_sender.tab.id, _request.active)
  }

  return;
});


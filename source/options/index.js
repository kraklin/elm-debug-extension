import browser from 'webextension-polyfill';
import {version} from '../../package.json'
import {Elm} from './Options.elm';
import {isChromeBased} from '../scripts/helpers.js'

//TODO: make some globals import to co-locate all these constants
const globalStorageKey = "globalOptions";

browser.tabs.query({active: true, currentWindow: true}).then((tabs) => {
  const currentTab = tabs[0];
  const hasCustomFormatters = isChromeBased()
  
  browser.storage.sync.get([globalStorageKey]).then((storedOptions) => {
    const storedGlobals = storedOptions.globalOptions;
    const defaultOptions = { simple_mode: true, limit: 1000000, debug: false }
    let initialOptions = {...defaultOptions, ...storedGlobals }

    const app = Elm.Options.init({
      node: document.getElementById('elm-options'),
      flags: {
        version: version, 
        hasCustomFormatters: hasCustomFormatters,
        initialOptions: initialOptions
      }
    });

    app.ports.saveGlobalOptions.subscribe((globalOptions) => {

      browser.storage.sync.set({[globalStorageKey]: globalOptions})
      .then(()=>{
        app.ports.globalsSavedResult.send(null);
      })
      .catch((err)=>{
        app.ports.globalsSavedResult.send(err.message);
        console.error(err);
      });
    }) 

  });
});

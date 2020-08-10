import {register} from 'elm-debug-transformer';

function injectScript(func) {
    var actualCode = '(' + func + ')();'

    var script = document.createElement('script');
    script.textContent = actualCode;
    (document.head||document.documentElement).appendChild(script);
    script.remove();
}

injectScript(function() {
    if(window.console && console.log){
        var old = console.log;
        console.log = function(){
            if (!!arguments && arguments.length === 1){
              window.postMessage({type: "ELM_LOG", message: arguments[0]});
            }
            else {
              old.apply(this, arguments)
            }
        }
    }  
});

register({limit: 1000000});

window.addEventListener(
  'message',
  (event) => {
    // We only accept messages from ourselves
    // if (event.source != window)
    // return;
    if (event.data.type && (event.data.type == "ELM_LOG")) {
      console.log(event.data.message);
    }
  },
  false
);

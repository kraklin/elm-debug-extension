export class AutoscrollDiv extends HTMLElement {
  static get observedAttributes() {
    return ['autoscroll'];
  }

  constructor() {
    // Always call super first in constructor
    super();

    const shadow = this.attachShadow({mode: 'open'});

    const div = document.createElement('div');
    div.appendChild(document.createElement('slot'));

    const myObserver = new ResizeObserver(entries => {
        if (this.autoscroll === false) {
          return;
        }

        entries.forEach(entry => {
            this.scrollTop = entry.contentRect.height;
        });
    });

    shadow.appendChild(div);
    myObserver.observe(div);
  }

  connectedCallback() {
    this.addEventListener('scroll', (e) => {
      if(e.target.scrollHeight > (e.target.clientHeight + e.target.scrollTop)){
        this.autoscroll = false;
        this.dispatchEvent(new CustomEvent("x-autoscroll-stopped",{
          bubbles: true,
          cancelable: false
        }));
      }
    });

  }

  disconnectedCallback() {
    this.removeEventListener('scroll');
  }

  attributeChangedCallback(name, oldValue, newValue) {
    if(name === "autoscroll"){
      this.autoscroll = (newValue === "true");
    }
  }
}


import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const __STATE__ = 'state'
const flags = {
  state: JSON.parse(window.localStorage.getItem(__STATE__))
}
const app = Main.embed(document.getElementById('root'), flags);

app.ports.save.subscribe(state => {
  try {
    window.localStorage.setItem(__STATE__, JSON.stringify(state))
  } catch (e) {
    console.warn('Could not save state locally...')
  }
})

registerServiceWorker();

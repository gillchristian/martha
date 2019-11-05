const toggle = document.querySelector('.history-toggle')
toggle.onclick = () => {
  const history = document.querySelector('.history')
  history.classList.toggle('history-show')
}

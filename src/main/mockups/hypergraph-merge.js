"use strict";
window.addEventListener('DOMContentLoaded', (e) => {
  document.getElementById('before').click();
  document.querySelectorAll('input').forEach(
  (elem) => {
    addEventListener('change', radioEventDispatch, false);
  })
});
function radioEventDispatch(e) {
  if (e.target.value == 'after') {
    performMerge()
  } else {
    
    reset()
  };
}
function performMerge() {
  console.log('clicked after');
}
function reset() {
  moveSlowly(1, 10);
}
function moveSlowly(e, n) {
  for (var i = 0; i < n; i++) {
    setTimeout(console.log, 500 * i, 'Iteration no.', i)
  }
}
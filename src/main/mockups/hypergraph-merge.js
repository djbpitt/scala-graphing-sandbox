"use strict";
window.addEventListener('DOMContentLoaded', () => {
  document.getElementById('before').click();
  document.querySelectorAll('input').forEach(
  () => {
    addEventListener('change', radioEventDispatch, false);
  })
})

function radioEventDispatch(e) {
  if (e.target.value === 'after') {
    performMerge();
  } else {
    reset();
  }
}

function performMerge() {
  console.log('clicked after');
  let ABline = document.getElementById('ABline');
  let CDline = document.getElementById('CDline');
  splitLine(ABline, 51);
  splitLine(CDline, 35);
}

function reset() {
  /* Reset is instantaneous (unlike performMerge)
   * The top-level <g> has a scale() that we need to retain;
   * All other instances of @transform on <g> elements involve
   *   movement, which we undo by stripping the @transform */
  console.log('clicked before');
  let gs = document.querySelectorAll('g g');
  let i = 0;
  const len = gs.length;
  for (; i < len; i++) {
    gs[i].setAttribute('transform', 'translate(0, 0)');
  }
}
function splitLine(e, n) {
  function timeoutLoop(i) {
    if (i < n) {
      setTimeout(function () {
        e.setAttribute('transform', 'translate(' + i + ',0)')
        timeoutLoop(i + 1);
      },
      40);
    }
  }
  timeoutLoop(0);
  // Start the loop
}
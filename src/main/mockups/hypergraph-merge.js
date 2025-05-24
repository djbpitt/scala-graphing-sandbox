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

async function performMerge() {
  let ABline = document.getElementById('ABline');
  let CDline = document.getElementById('CDline');
  let Bany = document.getElementById('Bany');
  let Ca = document.getElementById('Ca');
  let Dany = document.getElementById('Dany');
  let CDdistinct = document.getElementById('CDdistinct');
  const [result1, result2] = await Promise.all([
    splitLine(ABline, 51),
    splitLine(CDline, 35)
  ]);
  const [result3, result4] = await Promise.all([
    moveUp(Ca, 105, 0),
    moveUp(Dany, 90, 0),
    moveDown(Bany, 16)
  ]);
  const [result5, result6] = await Promise.all([
    moveUp(CDdistinct, 74, 0),
    moveUp(CDline, 90, 34)
  ]);
}

function reset() {
  /* Reset is instantaneous (unlike performMerge)
   * The top-level <g> has a scale() that we need to retain;
   * All other instances of @transform on <g> elements involve
   *   movement, which we undo by stripping the @transform */
  let gs = document.querySelectorAll('g g');
  let i = 0;
  const len = gs.length;
  for (; i < len; i++) {
    gs[i].setAttribute('transform', 'translate(0, 0)');
  }
}

function splitLine(e, n) {
  return new Promise(resolve => {
    function timeoutLoop(i) {
      if (i < n) {
        setTimeout(function () {
            e.setAttribute('transform', 'translate(' + i + ',0)')
            timeoutLoop(i + 1);
          },
          40);
      } else {
        resolve(i);
      }
    }

    timeoutLoop(0);
  })
}

function moveUp(e, n, h) {
  return new Promise(resolve => {
    function timeoutLoop(i) {
      if (i < n) {
        setTimeout(function () {
            e.setAttribute('transform', 'translate(' + h + ',-' + i + ')')
            timeoutLoop(i + 1);
          },
          40);
      } else {
        resolve(i);
      }
    }

    timeoutLoop(0);
  })
}

function moveDown(e, n) {
  return new Promise(resolve => {
    function timeoutLoop(i) {
      if (i < n) {
        setTimeout(function () {
            e.setAttribute('transform', 'translate(0,' + i + ')')
            timeoutLoop(i + 1);
          },
          40);
      } else {
        resolve(i);
      }
    }

    timeoutLoop(0);
  })
}

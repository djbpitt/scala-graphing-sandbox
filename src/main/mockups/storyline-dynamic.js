"use strict";
document.addEventListener("DOMContentLoaded", function () {
  const alignments = document.getElementsByClassName("alignment");
  for (var i = 0, len = alignments.length; i < len; i++) {
    alignments[i].addEventListener("click", toggleSize);
  }
})
function toggleSize() {
  var newWidth;
  if (this.getAttribute("width") == this.dataset.maxwidth) {
    newWidth = this.dataset.minwidth;
  } else {
    newWidth = this.dataset.maxwidth;
  }
  this.setAttribute("width", newWidth);
}
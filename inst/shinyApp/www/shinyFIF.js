$(document).ready(function () {
  Shiny.addCustomMessageHandler("closeButton", function (editor) {
    var a = document.createElement("A");
    a.style.textDecoration = "underline";
    a.style.position = "absolute";
    a.style.top = "2px";
    a.style.left = "2px";
    a.style.color = "red";
    a.style.fontWeight = "bold";
    a.setAttribute("href", "javascript:;");
    a.appendChild(document.createTextNode("X"));
    a.onclick = function (e) {
      e.stopPropagation();
      ace.edit(editor).container.remove();
      Shiny.setInputValue("closetab", editor, {
        priority: "event"
      });
    };
    setTimeout(function() {
      $("a.list-group-item.active").prepend(a);
    }, 1000);
  });
});

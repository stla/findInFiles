function Save(event, content, file) {
  console.log(event);
  var a = document.createElement("a");
  document.body.append(a);
  a.download = file;
  var b64 = btoa(unescape(encodeURIComponent(decodeURI(content))));
  a.href = "data:text/plain;base64," + b64;
  a.click();
  a.remove();
}

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
    setTimeout(function () {
      $("a.list-group-item.active").prepend(a);
    }, 1000);
  });

  Shiny.addCustomMessageHandler("goto", function (x) {
    setTimeout(function () {
      var editor = ace.edit(x.editor);
      editor.env.editor.gotoLine(x.line, 0, true);
      editor.focus();
    }, 1000);
  });
});

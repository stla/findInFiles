HTMLWidgets.widget({
  name: "findInFiles",

  type: "output",

  factory: function (el, width, height) {
    return {
      renderValue: function (x) {
        var convert = new ANSItoHTML({
          fg: "#FFF",
          bg: "#000",
          newline: true,
          escapeXML: true,
          stream: false
        });
        var html = convert.toHtml(x.ansi);
        el.innerHTML = html;

        if (HTMLWidgets.shinyMode) {
          var filespans = document.evaluate(
            "//span[@style=\"color:#A0A\"]",
            document,
            null,
            XPathResult.ORDERED_NODE_ITERATOR_TYPE,
            null
          );
          var spn = filespans.iterateNext();
          var spns = [];
          while (spn) {
            spns.push(spn);
            spn = filespans.iterateNext();
          }
          function createAnchor(i) {
            var filepath = spns[i].innerText;
            spns[i].innerText = "";
            var a = document.createElement("A");
            a.style.textDecoration = "underline";
            a.setAttribute("href", "javascript:;");
            a.appendChild(document.createTextNode(filepath));
            spns[i].appendChild(a);
            var linespan = spns[i].nextElementSibling.nextElementSibling;
            var line = parseInt(linespan.innerText);
            var shinyValue = { file: filepath, line: line };
            a.onclick = function () {
              Shiny.setInputValue("filewithline", shinyValue, {
                priority: "event"
              });
            };
          }
          for (var i = 0; i < spns.length; i++) {
            createAnchor(i);
          }
        }
      },

      resize: function (width, height) {
        // TODO: code to re-render the widget with a new size
      }
    };
  }
});

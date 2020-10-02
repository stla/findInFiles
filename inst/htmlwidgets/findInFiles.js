HTMLWidgets.widget({

  name: "findInFiles",

  type: "output",

  factory: function(el, width, height) {

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
      },

      resize: function(width, height) {
        // TODO: code to re-render the widget with a new size
      }

    };
  }
});

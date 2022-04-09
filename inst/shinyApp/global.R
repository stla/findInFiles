library(findInFiles)
library(shiny)
library(shinyAce)
library(shinyWidgets)
library(shinyjqui)

onKeyDown <- HTML(
  'function onKeyDown(event) {',
  '  var key = event.which || event.keyCode;',
  '  if(key === 13) {',
  '    Shiny.setInputValue(',
  '      "pattern", event.target.value, {priority: "event"}',
  '    );',
  '  }',
  '}'
)

css <- '
a.list-group-item>h4 {
  font-size: 12px;
  word-break: break-all;
}
'

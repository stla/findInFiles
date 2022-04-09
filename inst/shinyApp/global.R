library(findInFiles)
library(shiny)
library(shinyAce)
library(shinyWidgets)
library(shinyjqui)
library(shinyFiles)
library(shinyvalidate)

roots <- getVolumes()()

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

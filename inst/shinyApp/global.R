library(findInFiles)
library(shiny)
library(shinyAce)
library(shinyWidgets)
library(shinyjqui)
library(shinyFiles)
library(shinyvalidate)
library(fs)
library(htmltools)

roots <- c(wd = getOption("FIFWD"), getVolumes()())

smallInput <- function(Input) {
  tagQ <- tagQuery(Input)
  tagQ$find("input")$addClass("input-sm form-control-sm")$allTags()
}

isPositiveIntegerOrNA <- function(x) {
  if(is.na(x) || (x >= 0 && x %% 1 == 0)){
    NULL
  } else {
    "Must be an integer or empty."
  }
}


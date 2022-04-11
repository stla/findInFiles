library(findInFiles)
library(shiny)
library(shinyAce)
library(shinyWidgets)
library(shinyjqui)
library(shinyFiles)
library(shinyvalidate)
library(fs)

roots <- c(wd = getOption("FIFWD"), getVolumes()())


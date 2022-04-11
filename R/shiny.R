#' @title Shiny bindings for \code{findInFiles}
#'
#' @description Output and render functions for using \code{findInFiles} within
#'   Shiny applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height a valid CSS unit (like \code{"100\%"},
#'   \code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
#'   string and have \code{"px"} appended
#' @param expr an expression that generates a '\code{\link{findInFiles}}' widget
#' @param env the environment in which to evaluate \code{expr}
#' @param quoted logical, whether \code{expr} is a quoted expression (with
#'   \code{quote()})
#'
#' @return \code{FIFOutput} returns an output element that can be included in a
#'   Shiny UI definition, and \code{renderFIF} returns a
#'   \code{shiny.render.function} object that can be included in a Shiny server
#'   definition.
#'
#' @name findInFiles-shiny
#'
#' @importFrom htmlwidgets shinyWidgetOutput shinyRenderWidget
#' @export
#'
#' @examples library(findInFiles)
#' library(shiny)
#'
#' onKeyDown <- HTML(
#'   'function onKeyDown(event) {',
#'   '  var key = event.which || event.keyCode;',
#'   '  if(key === 13) {',
#'   '    Shiny.setInputValue(',
#'   '      "pattern", event.target.value, {priority: "event"}',
#'   '    );',
#'   '  }',
#'   '}'
#' )
#'
#' ui <- fluidPage(
#'   tags$head(tags$script(onKeyDown)),
#'   br(),
#'   sidebarLayout(
#'     sidebarPanel(
#'       selectInput(
#'         "ext", "Extension",
#'         choices = c("R", "js", "css")
#'       ),
#'       tags$div(
#'         class = "form-group shiny-input-container",
#'         tags$label(
#'           class = "control-label",
#'           "Pattern"
#'         ),
#'         tags$input(
#'           type = "text",
#'           class = "form-control",
#'           onkeydown = "onKeyDown(event);",
#'           placeholder = "Press Enter when ready"
#'         )
#'       ),
#'       numericInput(
#'         "depth", "Depth (set -1 for unlimited depth)",
#'         value = 0, min = -1, step = 1
#'       ),
#'       checkboxInput(
#'         "wholeWord", "Whole word"
#'       ),
#'       checkboxInput(
#'         "ignoreCase", "Ignore case"
#'       )
#'     ),
#'     mainPanel(
#'       FIFOutput("results")
#'     )
#'   )
#' )
#'
#'
#' server <- function(input, output){
#'
#'   output[["results"]] <- renderFIF({
#'     req(input[["pattern"]])
#'     findInFiles(
#'       ext = isolate(input[["ext"]]),
#'       pattern = input[["pattern"]],
#'       depth = isolate(input[["depth"]]),
#'       wholeWord = isolate(input[["wholeWord"]]),
#'       ignoreCase = isolate(input[["ignoreCase"]])
#'     )
#'   })
#'
#' }
#'
#' if(interactive()){
#'   shinyApp(ui, server)
#' }
FIFOutput <- function(outputId, width = "100%", height = "400px"){
  shinyWidgetOutput(
    outputId, "findInFiles", width, height, package = "findInFiles"
  )
}

#' @rdname findInFiles-shiny
#' @export
renderFIF <- function(expr, env = parent.frame(), quoted = FALSE){
  if(!quoted){ expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, FIFOutput, env, quoted = TRUE)
}


#' @title Shiny application 'Find in files'
#' @description Launches a Shiny application allowing to run
#'   \code{\link{findInFiles}} and to navigate in the results.
#'
#' @return No returned value, just launches the Shiny application.
#' @note The packages listed in the \strong{Suggests} field of the package
#'   description are required.
#' @importFrom shiny shinyAppDir
#' @export
shinyFIF <- function(){
  options(FIFWD = getwd())
  shinyAppDir(system.file("shinyApp", package = "findInFiles"))
}

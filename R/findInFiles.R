#' Find pattern in files
#' @description Find a pattern in some files.
#'
#' @param ext file extension, e.g. \code{"R"} or \code{"js"}
#' @param pattern pattern to search for, a regular expression, e.g.
#'   \code{"function"} or \code{"^function"}
#' @param depth depth of the search, \code{NULL} or a negative number for an
#'   entire recursive search (subdirectories, subdirectories of subdirectories,
#'   etc.), otherwise a positive integer: \code{0} to search in the root
#'   directory only, \code{1} to search in the root directory and its
#'   subdirectories, etc.
#' @param wholeWord logical, whether to match the whole pattern
#' @param ignoreCase logical, whether to ignore the case
#' @param perl logical, whether \code{pattern} is a Perl regular expression
#' @param excludePattern a pattern; exclude from search the files and folders
#'   which match this pattern
#' @param excludeFoldersPattern a pattern; exclude from search the folders
#'   which match this pattern
#' @param root path to the root directory to search from
#'
#' @import htmlwidgets
#' @export
#'
#' @examples library(findInFiles)
#'
#' folder <- system.file("example", package = "findInFiles")
#' findInFiles("R", "function", root = folder)
#'
#' folder <- system.file("www", "shared", package = "shiny")
#' findInFiles("css", "outline", excludePattern = "*.min.css", root = folder)
findInFiles <- function(
  ext, pattern, depth = NULL,
  wholeWord = FALSE, ignoreCase = FALSE, perl = FALSE,
  excludePattern = NULL, excludeFoldersPattern = NULL,
  root = "."
){

  if(grepl("sunos", tolower(Sys.info()["sysname"]))){ # skip on Solaris
    message("This package is currently not supported by 'Solaris' platforms.")
    return(invisible(NULL))
  }

  ansi <- paste0(grepInFiles(
    ext = ext, pattern = pattern, depth = depth,
    wholeWord = wholeWord, ignoreCase = ignoreCase, perl = perl,
    excludePattern = excludePattern,
    excludeFoldersPattern = excludeFoldersPattern,
    directory = root
  ), collapse = "\n")

  # forward options using x
  x = list(
    ansi = ansi
  )

  # create widget
  htmlwidgets::createWidget(
    name = "findInFiles",
    x,
    width = NULL,
    height = NULL,
    package = "findInFiles",
    elementId = NULL
  )
}

#' Shiny bindings for \code{findInFiles}
#'
#' Output and render functions for using \code{findInFiles} within Shiny
#' applications and interactive Rmd documents.
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
#' @name findInFiles-shiny
#'
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
  htmlwidgets::shinyWidgetOutput(
    outputId, "findInFiles", width, height, package = "findInFiles"
  )
}

#' @rdname findInFiles-shiny
#' @export
renderFIF <- function(expr, env = parent.frame(), quoted = FALSE){
  if(!quoted){ expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, FIFOutput, env, quoted = TRUE)
}

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
#' @param output one of \code{"viewer"}, \code{"dataframe"} or
#'   \code{"viewer+dataframe"}; see examples
#'
#' @return A dataframe if \code{output="dataframe"}, otherwise a
#'   \code{htmlwidget} object.
#'
#' @import htmlwidgets
#' @importFrom stringr str_split_fixed
#' @importFrom crayon strip_style
#' @export
#'
#' @examples library(findInFiles)
#' folder <- system.file("example", package = "findInFiles")
#' findInFiles("R", "function", root = folder)
#'
#' findInFiles("R", "function", root = folder, output = "dataframe")
#'
#' fif <- findInFiles("R", "function", root = folder, output = "viewer+dataframe")
#' FIF2dataframe(fif)
#' fif
#'
#' folder <- system.file("www", "shared", package = "shiny")
#' findInFiles("css", "outline", excludePattern = "*.min.css", root = folder)
findInFiles <- function(
  ext, pattern, depth = NULL,
  wholeWord = FALSE, ignoreCase = FALSE, perl = FALSE,
  excludePattern = NULL, excludeFoldersPattern = NULL,
  root = ".", output = "viewer"
){

  if(inSolaris() && Sys.which("ggrep") == ""){
    message("On Solaris, this package requires the 'ggrep' system command.")
    return(invisible(NULL))
  }

  output <- match.arg(output, c("viewer", "dataframe", "viewer+dataframe"))

  results <- grepInFiles(
    ext = ext, pattern = pattern, depth = depth,
    wholeWord = wholeWord, ignoreCase = ignoreCase, perl = perl,
    excludePattern = excludePattern,
    excludeFoldersPattern = excludeFoldersPattern,
    directory = root, output = output
  )

  if(output %in% c("dataframe", "viewer+dataframe")){
    if(output == "viewer+dataframe"){
      strippedResults <- strip_style(results)
    }else{
      strippedResults <- results
    }
    resultsMatrix <- stringr::str_split_fixed(strippedResults, ":", n = 3L)
    colnames(resultsMatrix) <- c("file", "line", "code")
    resultsDF <- as.data.frame(resultsMatrix)
    resultsDF[["line"]] <- as.integer(resultsDF[["line"]])
    class(resultsDF) <- c(oldClass(resultsDF), "findInFiles")
    if(output == "dataframe"){
      return(resultsDF)
    }
  }

  ansi <- paste0(results, collapse = "\n")

  # forward options using x
  if(output == "viewer"){
    x = list(
      ansi = ansi
    )
  }else{ # viewer+dataframe
    x = list(
      ansi = ansi,
      results = resultsDF
    )
  }

  # create widget
  htmlwidgets::createWidget(
    name = "findInFiles",
    x = x,
    width = NULL,
    height = NULL,
    package = "findInFiles",
    elementId = NULL
  )

}

#' Output of `findInFiles` as a dataframe
#'
#' Returns the results of \code{\link{findInFiles}} in a dataframe, when the
#'   option \code{output = "viewer+dataframe"} is used.
#'
#' @param fif the output of \code{\link{findInFiles}} used with the
#'   option \code{output = "viewer+dataframe"}
#'
#' @return The results of \code{\link{findInFiles}} in a dataframe.
#' @export
#'
#' @examples folder <- system.file("example", package = "findInFiles")
#' fif <- findInFiles("R", "function", root = folder, output = "viewer+dataframe")
#' FIF2dataframe(fif)
#' fif
FIF2dataframe <- function(fif){
  if(is.data.frame(fif) && inherits(fif, "findInFiles")){
    return(fif)
  }
  if(!inherits(fif, c("findInFiles", "htmlwidget"))){
    stop(
      "The `fif` argument is not a output of `findInFiles`.",
      call. = TRUE
    )
  }
  output <- fif[["x"]][["results"]]
  if(is.null(output)){
    message(
      'You did not set the option `output = "viewer+dataframe"`.'
    )
    return(invisible(NULL))
  }
  output
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

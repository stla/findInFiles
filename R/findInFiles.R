#' Find pattern in files
#' @description Find a pattern in some files.
#'
#' @param ext file extension, e.g. \code{"R"} or \code{"js"}
#' @param pattern pattern to search for, a regular expression, e.g.
#'   \code{"function"} or \code{"^function"}
#' @param depth depth of the search, \code{NULL} for an entire recursive
#'   search (subdirectories, subdirectories of subdirectories, etc.), or an
#'   integer: \code{0} to search in the root directory only, \code{1} to
#'   search in the root directory and its subdirectories, etc.
#' @param wholeword logical, whether to match the whole pattern
#' @param ignoreCase logical, whether to ignore the case
#' @param perl logical, whether \code{pattern} is a Perl regular expression
#' @param directory path to the root directory to search from
#'
#' @import htmlwidgets
#' @export
#'
#' @examples library(findInFiles)
#' folder <- system.file("example", package = "findInFiles")
#' findInFiles("R", "function", directory = folder)
findInFiles <- function(
  ext, pattern, depth = NULL,
  wholeword = FALSE, ignoreCase = FALSE, perl = FALSE,
  directory = "."
){

  ansi <- paste0(grepInFiles(
    ext = ext, pattern = pattern, depth = depth,
    wholeword = wholeword, ignoreCase = ignoreCase, perl = perl,
    directory = directory
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

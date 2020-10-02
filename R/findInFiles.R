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
#'
#' @export
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

#' Shiny bindings for findInFiles
#'
#' Output and render functions for using findInFiles within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a findInFiles
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name findInFiles-shiny
#'
#' @export
findInFilesOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'findInFiles', width, height, package = 'findInFiles')
}

#' @rdname findInFiles-shiny
#' @export
renderFindInFiles <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, findInFilesOutput, env, quoted = TRUE)
}

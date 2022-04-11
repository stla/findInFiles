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
#' @param output one of \code{"viewer"}, \code{"tibble"} or
#'   \code{"viewer+tibble"}; see examples
#'
#' @return A tibble if \code{output="tibble"}, otherwise a
#'   \code{htmlwidget} object.
#'
#' @importFrom htmlwidgets createWidget
#' @importFrom stringr str_split_fixed
#' @importFrom crayon strip_style
#' @importFrom tibble tibble
#' @export
#'
#' @examples library(findInFiles)
#' folder <- system.file("example", package = "findInFiles")
#' findInFiles("R", "function", root = folder)
#'
#' findInFiles("R", "function", root = folder, output = "tibble")
#'
#' fif <- findInFiles("R", "function", root = folder, output = "viewer+tibble")
#' FIF2tibble(fif)
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

  stopifnot(isString(ext))

  if(isBinaryExtension(ext)){
    stop(
      sprintf("This file extension ('%s') is not allowed.", ext)
    )
  }

  stopifnot(isString(output))
  if(output == "dataframe"){
    output <- "tibble"
    message(
      'The option `output="dataframe"` is deprecated; use `output="tibble"` ',
      'instead.'
    )
  }
  if(output == "viewer+dataframe"){
    output <- "viewer+tibble"
    message(
      'The option `output="viewer+dataframe"` is deprecated; ',
      'use `output="viewer+tibble"` instead.'
    )
  }

  output <- match.arg(
    output,
    c("viewer", "tibble", "viewer+tibble")
  )

  results <- grepInFiles(
    ext = ext, pattern = pattern, depth = depth,
    wholeWord = wholeWord, ignoreCase = ignoreCase, perl = perl,
    excludePattern = excludePattern,
    excludeFoldersPattern = excludeFoldersPattern,
    directory = root, output = output
  )

  if(output %in% c("tibble", "viewer+tibble")){
    if(output == "viewer+tibble"){
      strippedResults <- strip_style(results)
    }else{
      strippedResults <- results
    }
    resultsMatrix <- stringr::str_split_fixed(strippedResults, ":", n = 3L)
    colnames(resultsMatrix) <- c("file", "line", "code")
    resultsDF <- as.data.frame(resultsMatrix, stringsAsFactors = FALSE)
    resultsDF[["line"]] <- as.integer(resultsDF[["line"]])
    if(!is.null(results)){
      opts <- attr(results, "options")
      resultsDF[["code"]] <- do.call(
        function(...){redifyVector(resultsDF[["code"]], ...)}, opts
      )
    }
    resultsDF <- tibble(resultsDF)
    class(resultsDF) <- c(oldClass(resultsDF), "findInFiles")
    if(output == "tibble"){
      return(resultsDF)
    }
  }

  if(is.null(results)){
    ansi <- "No results."
  }else{
    ansi <- paste0(results, collapse = "\n")
  }

  # forward options using x
  if(output == "viewer"){
    x = list(
      ansi = ansi
    )
  }else{ # viewer+tibble
    x = list(
      ansi = ansi,
      results = resultsDF
    )
  }

  # create widget
  createWidget(
    name = "findInFiles",
    x = x,
    width = NULL,
    height = NULL,
    package = "findInFiles",
    elementId = NULL
  )

}

#' @title Output of `findInFiles` as a tibble
#'
#' @description Returns the results of \code{\link{findInFiles}} in a
#'   tibble, when the option \code{output = "viewer+tibble"} is used.
#'
#' @param fif the output of \code{\link{findInFiles}} used with the
#'   option \code{output = "viewer+tibble"}
#'
#' @return The results of \code{\link{findInFiles}} in a tibble.
#' @export
#'
#' @examples folder <- system.file("example", package = "findInFiles")
#' fif <- findInFiles("R", "function", root = folder, output = "viewer+tibble")
#' FIF2tibble(fif)
#' fif
FIF2tibble <- function(fif){
  if(is.null(fif)){
    return(NULL)
  }
  if(is.data.frame(fif) && inherits(fif, "findInFiles")){
    return(fif)
  }
  if(!inherits(fif, c("findInFiles", "htmlwidget"))){
    stop(
      "The `fif` argument is not an output of `findInFiles`.",
      call. = TRUE
    )
  }
  output <- fif[["x"]][["results"]]
  if(is.null(output)){
    message(
      'You did not set the option `output = "viewer+tibble"`.'
    )
    return(invisible(NULL))
  }
  output
}

#' @title Output of `findInFiles` as a dataframe
#'
#' @description Returns the results of \code{\link{findInFiles}} in a
#'   dataframe, when the option \code{output = "viewer+tibble"} or
#'   \code{output = "tibble"} is used.
#'
#' @param fif the output of \code{\link{findInFiles}} used with the
#'   option \code{output = "viewer+tibble"} or \code{output = "tibble"}
#'
#' @return The results of \code{\link{findInFiles}} in a dataframe.
#'
#' @importFrom crayon strip_style
#' @importFrom vctrs vec_data
#' @export
#'
#' @examples folder <- system.file("example", package = "findInFiles")
#' fif <- findInFiles("R", "function", root = folder, output = "viewer+tibble")
#' FIF2dataframe(fif)
#' fif
FIF2dataframe <- function(fif){
  tbl <- FIF2tibble(fif)
  if(is.null(tbl)){
    return(NULL)
  }
  tbl[["code"]] <- vec_data(strip_style(tbl[["code"]]))
  as.data.frame(tbl)
}


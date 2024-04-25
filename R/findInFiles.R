#' @title Find pattern in files
#' @description Find a pattern in some files. The functions \code{findInFiles}
#'   and \code{fif} are the same, and \code{fifR(...)} is the same as
#'   \code{findInFiles(extensions = "R", ...)}.
#'
#' @name findInFiles
#' @rdname findInFiles
#'
#' @param extensions extension(s) of the files to include in the search
#'   (case-sensitive), e.g. \code{"R"} or \code{c("R", "Rmd")}, or \code{"*"}
#'   to search in all files
#' @param pattern pattern to search for, a regular expression, e.g.
#'   \code{"function"} or \code{"^function"}, or a string if \code{fixed=TRUE};
#'   by default the pattern is considered as a basic regular expression, but
#'   this can be changed to an extended regular expression by setting
#'   \code{extended=TRUE} or to a Perl regular expression by setting
#'   \code{perl=TRUE}
#' @param depth depth of the search, \code{NULL} or a negative number for an
#'   entire recursive search (subdirectories, subdirectories of subdirectories,
#'   etc.), otherwise a positive integer: \code{0} to search in the root
#'   directory only, \code{1} to search in the root directory and its
#'   subdirectories, etc.
#' @param maxCountPerFile maximum number of results per file, \code{NULL} for
#'   an unlimited number, otherwise a positive integer; when an integer
#'   \code{m} is supplied, \code{grep} stops to search in each file after it
#'   finds \code{m} results
#' @param maxCount maximum number of results, \code{NULL} for an unlimited
#'   number, otherwise a positive integer; supplying an integer \code{m} just
#'   truncates the output, it does not stop \code{grep} after \code{m} results
#'   are found (so there is no gain of efficiency)
#' @param wholeWord logical, whether to match the whole pattern
#' @param ignoreCase logical, whether to ignore the case
#' @param extended logical, whether the pattern given in the \code{pattern}
#'   is an extended regular expression; if \code{TRUE}, you can search for
#'   multiple patterns by passing a string like \code{"(pattern1|pattern2|...)"}
#'   to the \code{pattern} argument
#' @param fixed logical, whether the pattern given in the \code{pattern}
#'   argument is a string to be matched as is, or, to search for multiple
#'   patterns, multiple strings separated by \code{"\n"}
#' @param perl logical, whether the pattern given in the \code{pattern}
#'   argument is a Perl regular expression; if \code{TRUE}, you can search for
#'   multiple patterns by passing a string like \code{"(pattern1|pattern2|...)"}
#'   to the \code{pattern} argument
#' @param includePattern this argument is ignored if \code{depth} is not a
#'   positive integer; it must be a pattern or a vector of patterns, and only
#'   the files whose name matches this pattern or one of these patterns will be
#'   included in the search
#' @param excludePattern a pattern or a vector of patterns; files and folders
#'   whose name matches this pattern or one of these patterns will be excluded
#'   from search
#' @param excludeFoldersPattern a pattern or a vector of patterns; folders
#'   whose name matches this pattern or one of these patterns will be excluded
#'   from search
#' @param moreOptions additional options passed to the \code{grep} command,
#'   for \code{grep} experts
#' @param root path to the root directory to search from
#' @param output one of \code{"viewer"}, \code{"tibble"} or
#'   \code{"viewer+tibble"}; set \code{"tibble"} to get a tibble,
#'   \code{"viewer"} to get a \code{htmlwidget}, and \code{"viewer+tibble"}
#'   to get a \code{htmlwidget} from which you can extract a tibble
#'   with the function \code{\link{FIF2tibble}}
#' @param elementId a HTML id, usually useless
#' @param ... arguments other than \code{extensions} passed to
#'   \code{findInFiles}
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
#' findInFiles(
#'   "css", "color", root = folder,
#'   excludePattern = c("*.min.css", "selectize*", "shiny*")
#' )
findInFiles <- function(
  extensions, pattern, depth = NULL, maxCountPerFile = NULL, maxCount = NULL,
  wholeWord = FALSE, ignoreCase = FALSE,
  extended = FALSE, fixed = FALSE, perl = FALSE,
  includePattern = NULL,
  excludePattern = NULL, excludeFoldersPattern = NULL,
  moreOptions = NULL,
  root = ".", output = "viewer", elementId = NULL
){

  if(inSolaris() && Sys.which("ggrep") == ""){
    message("On Solaris, this package requires the 'ggrep' system command.")
    return(invisible(NULL))
  }

  for(ext in extensions) {
    if(isBinaryExtension(ext)){
      stop(
        sprintf("This file extension ('%s') is not allowed.", ext)
      )
    }
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
    ext = extensions, pattern = pattern, depth = depth,
    maxCountPerFile = maxCountPerFile, maxCount = maxCount,
    wholeWord = wholeWord, ignoreCase = ignoreCase,
    extended = extended, fixed = fixed, perl = perl,
    includePattern = includePattern,
    excludePattern = excludePattern,
    excludeFoldersPattern = excludeFoldersPattern,
    moreOptions = moreOptions,
    directory = root, output = output
  )

  resultsDF <- NULL
  if(output %in% c("tibble", "viewer+tibble")){
    if(output == "viewer+tibble"){
      strippedResults <- strip_style(results)
    }else{
      strippedResults <- results
    }
    resultsMatrix <- str_split_fixed(strippedResults, ":", n = 3L)
    colnames(resultsMatrix) <- c("file", "line", "match")
    resultsDF <- as.data.frame(resultsMatrix, stringsAsFactors = FALSE)
    resultsDF[["line"]] <- as.integer(resultsDF[["line"]])
    if(!is.null(results)){
      opts <- attr(results, "options")
      resultsDF[["match"]] <- do.call(
        function(...){redifyVector(resultsDF[["match"]], ...)}, opts
      )
    }
    resultsDF <- tibble(resultsDF)
    class(resultsDF) <- c(oldClass(resultsDF), "findInFiles")
    if(output == "tibble"){
      return(resultsDF)
    }
  }

  maxCountExceeded <- isTRUE(attr(results, "maxCountExceeded"))
  numberOfResults <- attr(results, "numberOfResults")

  if(is.null(results)){
    ansi <- "No result."
  }else{
    ansi <- paste0(results, collapse = "\n")
  }

  # create widget
  widget <- createWidget(
    name = "findInFiles",
    x = list("ansi" = ansi),
    width = NULL,
    height = NULL,
    package = "findInFiles",
    elementId = elementId
  )
  attr(widget, "maxCountExceeded") <- maxCountExceeded
  attr(widget, "numberOfResults")  <- numberOfResults
  attr(widget, "tibble")           <- resultsDF
  widget

}

#' @rdname findInFiles
#' @export
fif <- findInFiles

#' @rdname findInFiles
#' @export
fifR <- function(...) {
  findInFiles(extensions = "R", ...)
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
  output <- attr(fif, "tibble")
  if(is.null(output)){
    message(
      'You did not set the option `output = "viewer+tibble"` ',
      ' in the call to `findInFiles`.'
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
  tbl[["match"]] <- vec_data(strip_style(tbl[["match"]]))
  as.data.frame(tbl)
}

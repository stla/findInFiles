getFiles <- function(extensions, depth){
  stopifnot(isPositiveInteger(depth))
  extensions <- do.call(c, lapply(extensions, function(ext) {
    paste0(c(".", "*."), ext)
  }))
  wildcards <- do.call(c, lapply(extensions, function(ext) {
    Reduce(
      file.path, x = rep("*", depth), init = ext,
      right = TRUE, accumulate = TRUE
    )
  }))
  Sys.glob(wildcards)
}

#' @importFrom utils head
#' @noRd
grepInFiles <- function(
    ext, pattern, depth, maxCountPerFile, maxCount,
    wholeWord, ignoreCase, extended, fixed, perl,
    includePattern, excludePattern, excludeFoldersPattern,
    moreOptions,
    directory, output
){
  if(inSolaris()){
    if(Sys.which("ggrep") == ""){
      stop("This package requires the 'ggrep' command-line utility.")
    }
  }else{
    if(Sys.which("grep") == ""){
      stop("This package requires the 'grep' command-line utility.")
    }
  }
  check <- all(vapply(ext, isString, logical(1L)))
  if(!check) {
    stop("Invalid file extension.")
  }
  stopifnot(isString(pattern))
  stopifnot(isBoolean(wholeWord))
  stopifnot(isBoolean(ignoreCase))
  stopifnot(isBoolean(perl))
  wd <- setwd(directory)
  on.exit(setwd(wd))
  opts <- c("-n", "-I", "--with-filename", moreOptions)
  if(output == "tibble"){
    opts <- c(opts, "--colour=never")
  }else{
    opts <- c(opts, "--colour=always")
  }
  if(!is.null(maxCountPerFile)) {
    stopifnot(isPositiveInteger(maxCountPerFile))
    opts <- c(opts, sprintf("-m %d", maxCountPerFile))
  }
  if(!is.null(maxCount)) {
    stopifnot(isPositiveInteger(maxCount))
  }
  if(wholeWord) opts <- c(opts, "-w")
  if(ignoreCase) opts <- c(opts, "-i")
  if(extended) opts <- c(opts, "-E")
  if(fixed) opts <- c(opts, "-F")
  if(perl) opts <- c(opts, "-P")
  if(!is.null(includePattern)){
    check <- all(vapply(includePattern, isString, logical(1L)))
    if(!check) {
      stop("Invalid argument `includePattern`.")
    }
    opts <- c(
      opts,
      vapply(includePattern, function(pattern) {
        paste0("--include=", shQuote(pattern))
      }, character(1L))
    )
  }
  if(!is.null(excludePattern)){
    check <- all(vapply(excludePattern, isString, logical(1L)))
    if(!check) {
      stop("Invalid argument `excludePattern`.")
    }
    opts <- c(
      opts,
      vapply(excludePattern, function(pattern) {
        paste0("--exclude=", shQuote(pattern))
      }, character(1L))
    )
  }
  excludeFoldersPattern <- c(".*", excludeFoldersPattern) # skip hidden folders
  check <- all(vapply(excludeFoldersPattern, isString, logical(1L)))
  if(!check) {
    stop("Invalid argument `excludeFoldersPattern`.")
  }
  opts <- c(
    opts,
    vapply(excludeFoldersPattern, function(pattern) {
      paste0("--exclude-dir=", shQuote(pattern))
    }, character(1L))
  )
  command <- ifelse(inSolaris(), "ggrep", "grep")
  results <- if(is.null(depth) || depth < 0){
    suppressWarnings(system2(
      command,
      args = c(
        paste0("--include=\\*\\.", ext),
        opts, "-r", "-e", shQuote(pattern)
      ),
      stdout = TRUE, stderr = TRUE
    ))
  }else{
    files <- getFiles(ext, depth)
    if(length(files) == 0L){
      message(
        sprintf("No file with extension '%s' has been found.", ext)
      )
      return(invisible(NULL))
    }
    suppressWarnings(system2(
      command,
      args = c(shQuote(pattern), shQuote(files), opts),
      stdout = TRUE, stderr = TRUE
    ))
  }
  if(!is.null(status <- attr(results, "status"))){
    if(status == 1L){
      message("No result.")
      return(invisible(NULL))
    }else{
      stop("An error occured. Possibly invalid 'grep' command.")
    }
  }
  nresults <- length(results)
  if(nresults == 1L) {
    message("One result.")
  } else {
    message(sprintf("%d results.", nresults))
  }
  if(!is.null(maxCount)) {
    results <- head(results, maxCount)
    if(maxCount < nresults) {
      attr(results, "maxCountExceeded") <- TRUE
      if(maxCount == 1L) {
        message(
          "Returning only one result ",
          "(reached the supplied maximum number of results)."
        )
      } else {
        message(
          sprintf("Returning only %d results ", maxCount),
          "(reached the supplied maximum number of results)."
        )
      }
    }
  }
  attr(results, "numberOfResults") <- nresults
  attr(results, "options") <- list(
    "pattern"    = pattern,
    "wholeWord"  = wholeWord,
    "ignoreCase" = ignoreCase,
    "perl"       = perl
  )
  results
}

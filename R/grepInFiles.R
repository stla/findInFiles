getFiles <- function(ext, depth){
  stopifnot(isPositiveInteger(depth))
  wildcards <- Reduce(
    file.path, x = rep("*", depth), init = paste0("*.", ext),
    right = TRUE, accumulate = TRUE
  )
  Sys.glob(wildcards)
}

grepInFiles <- function(
  ext, pattern, depth,
  wholeWord, ignoreCase, perl,
  excludePattern, excludeFoldersPattern,
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
  stopifnot(isString(ext))
  stopifnot(isString(pattern))
  stopifnot(isBoolean(wholeWord))
  stopifnot(isBoolean(ignoreCase))
  stopifnot(isBoolean(perl))
  wd <- setwd(directory)
  on.exit(setwd(wd))
  if(output == "tibble"){
    opts <- c("--colour=never", "-n", "--with-filename")
  }else{
    opts <- c("--colour=always", "-n", "--with-filename")
  }
  if(wholeWord) opts <- c(opts, "-w")
  if(ignoreCase) opts <- c(opts, "-i")
  if(perl) opts <- c(opts, "-P")
  if(!is.null(excludePattern)){
    stopifnot(isString(excludePattern))
    opts <- c(opts, paste0("--exclude=", shQuote(excludePattern)))
  } #TODO: multiple patterns - https://stackoverflow.com/questions/41702134/grep-exclude-from-how-to-include-multiple-files
  if(!is.null(excludeFoldersPattern)){
    stopifnot(isString(excludeFoldersPattern))
    opts <- c(opts, paste0("--exclude-dir=", shQuote(excludeFoldersPattern)))
  }
  # return(system2(
  #   "grep",
  #   args = c(
  #     paste0("--include=\\*\\.", ext), opts, "-r", "-e", shQuote(pattern)
  #   ),
  #   stdout = TRUE, stderr = TRUE
  # ))
  command <- ifelse(inSolaris(), "ggrep", "grep")
  results <- if(is.null(depth) || depth < 0){
    suppressWarnings(system2(
      command,
      args = c(
        paste0("--include=\\*\\.", ext), opts, "-r", "-e", shQuote(pattern)
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
      message("No results.")
      return(invisible(NULL))
    }else{
      stop("An error occured. Possibly invalid 'grep' command.")
    }
  }
  attr(results, "options") <- list(
    "pattern"    = pattern,
    "wholeWord"  = wholeWord,
    "ignoreCase" = ignoreCase,
    "perl"       = perl
  )
  results
}

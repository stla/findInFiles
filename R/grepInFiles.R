isPositiveInteger <- function(x){
  is.numeric(x) && (length(x) == 1L) && (floor(x) == x)
}

isString <- function(x){
  is.character(x) && (length(x) == 1L)
}

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
  wholeword, ignoreCase, perl,
  excludePattern, excludeFoldersPattern,
  directory
){
  if(Sys.which("grep") == ""){
    stop("This package requires the 'grep' command-line utility.")
  }
  stopifnot(isString(ext))
  stopifnot(isString(pattern))
  stopifnot(is.logical(wholeword))
  stopifnot(is.logical(ignoreCase))
  stopifnot(is.logical(perl))
  wd <- setwd(directory)
  on.exit(setwd(wd))
  opts <- c("--colour=always", "-n")
  if(wholeword) opts <- c(opts, "-w")
  if(ignoreCase) opts <- c(opts, "-i")
  if(perl) opts <- c(opts, "-P")
  if(!is.null(excludePattern)){
    stopifnot(isString(excludePattern))
    opts <- c(opts, paste0("--exclude=", shQuote(excludePattern)))
  }
  if(!is.null(excludeFoldersPattern)){
    stopifnot(isString(excludeFoldersPattern))
    opts <- c(opts, paste0("--exclude-dir=", shQuote(excludeFoldersPattern)))
  }
  results <- if(is.null(depth)){
    suppressWarnings(
      system2(
      "grep",
      args = c(
        paste0("--include=\\*.", ext), opts, "-r", "-e", shQuote(pattern)
      ),
      stdout = TRUE, stderr = TRUE
    ))
  }else{
    files <- getFiles(ext, depth)
    suppressWarnings(system2(
      "grep",
      args = c(shQuote(pattern), shQuote(files), opts),
      stdout = TRUE, stderr = TRUE
    ))
  }
  if(!is.null(attr(results, "status"))){
    stop("An error occured. Possibly invalid 'grep' command.")
  }
  results
}

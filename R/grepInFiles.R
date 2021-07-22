isPositiveInteger <- function(x){
  is.numeric(x) && (length(x) == 1L) && !is.na(x) && (floor(x) == x)
}

isString <- function(x){
  is.character(x) && (length(x) == 1L) && !is.na(x)
}

isBoolean <- function(x){
  is.logical(x) && (length(x) == 1L) && !is.na(x)
}

getFiles <- function(ext, depth){
  stopifnot(isPositiveInteger(depth))
  wildcards <- Reduce(
    file.path, x = rep("*", depth), init = paste0("*.", ext),
    right = TRUE, accumulate = TRUE
  )
  Sys.glob(wildcards)
}

inSolaris <- function(){
  grepl("sunos", tolower(Sys.info()["sysname"]))
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
  if(output == "dataframe"){
    opts <- c("--colour=never", "-n")
  }else{
    opts <- c("--colour=always", "-n")
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
    suppressWarnings(system2(
      command,
      args = c(shQuote(pattern), shQuote(files), opts),
      stdout = TRUE, stderr = TRUE
    ))
  }
  if(!is.null(status <- attr(results, "status"))){
    if(status == 1){
      message("No results.")
    }else{
      stop("An error occured. Possibly invalid 'grep' command.")
    }
  }
  results
}

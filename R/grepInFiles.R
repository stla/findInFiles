isPositiveInteger <- function(x){
  is.numeric(x) && (length(x) == 1L) && (floor(x) == x)
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
  directory
){
  if(Sys.which("grep") == ""){
    stop("This package requires the 'grep' command-line utility.")
  }
  wd <- setwd(directory)
  on.exit(setwd(wd))
  opts <- c("--colour=always", "-n")
  if(wholeword) opts <- c(opts, "-w")
  if(ignoreCase) opts <- c(opts, "-i")
  if(perl) opts <- c(opts, "-P")
  if(is.null(depth)){
    system2(
      "grep",
      args = c(
        paste0("--include=\\*.", ext), opts, "-r", "-e", shQuote(pattern)
      ),
      stdout = TRUE
    )
  }else{
    files <- getFiles(ext, depth)
    system2(
      "grep",
      args = c(shQuote(pattern), shQuote(files), opts),
      stdout = TRUE
    )
  }
}

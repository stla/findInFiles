#' @importFrom stringi stri_sub_replace_all
#' @importFrom crayon red
#' @noRd
redifyString <- function(x, pattern, perl, wholeWord, ignoreCase){
  if(wholeWord){
    pattern <- sprintf("\\b%s\\b", pattern)
  }
  starts <- gregexpr(pattern, x, perl = perl, ignore.case = ignoreCase)[[1L]]
  if(starts[1L] == -1L){
    return(x)
  }
  ends <- starts + attr(starts, "match.length") - 1L
  n <- length(starts)
  replacements <- character(n)
  for(i in seq_len(n)){
    subString <- substr(x, starts[i], ends[i])
    replacements[i] <- red(subString)
  }
  stri_sub_replace_all(
    x, starts, ends, replacement = replacements
  ) # add new_vctr to the column
}

#' @importFrom vctrs new_vctr
#' @noRd
redifyVector <- function(v, pattern, perl, wholeWord, ignoreCase){
  ansi <- vapply(
    v, redifyString, character(1L), pattern = pattern, perl = perl,
    wholeWord = wholeWord, ignoreCase = ignoreCase
  )
  new_vctr(ansi)
}

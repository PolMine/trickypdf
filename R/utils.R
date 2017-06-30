#' Restore paragraphs
#' 
#' Restore paragraphs in a character vector.
#' 
#' @param x a character vector
#' @param skipRegexCurrent a regex
#' @param skipRegexPrevious another regex
#' @export restore_paragraphs
#' @name restore_paragraphs
#' @rdname restore_paragraphs
#' @examples 
#' vec <- c(
#'   "This is a sample text. We freq-",
#'   "quently encounter issues with bro-",
#'   "ken lines."
#'   )
#' restore_paragraphs(vec)
restore_paragraphs <- function(x, skipRegexCurrent = "^\\s*[•A-Z(]", skipRegexPrevious = "[\\.?!)]\\s*$"){
  
  "Reconstruct paragraphs from a character vector with line breaks and word-wraps.
  The heuristic is as follows: If a line ends with a hyphenation and the next line
  starts with a small letter, remove hyphen and concatenate word."
  
  if (length(x) > 2){
    for (i in length(x):2){
      if (nchar(x[i-1]) < 40 && grepl(skipRegexPrevious, x[i-1]) == TRUE){
        # do nothing if preceding line ist short and ends with a satzzeichen
      } else {
        if (grepl("-\\s*$", x[i-1]) && grepl(skipRegexCurrent, x[i]) == FALSE){
          x[i-1] <- gsub("-\\s*$", "", x[i-1]) # remove hyphen
          x[i-1] <- paste(x[i-1], x[i], sep = "")
          x <- x[-i]
        } else {
          x[i-1] <- paste(x[i-1], x[i], sep = " ")
          x <- x[-i]
        }
      }
    }
  }
  x
}

#' Clean character vector.
#' 
#' @param x vector
#' @export broom
#' @rdname broom
#' @name broom
#' @examples
#' vec <- c(
#'   "This is    somewhat murky",
#'   "   text with  too much whitespace.  "
#' )
#' broom(vec)
broom <- function(x){
  x <- gsub("\uf038", "", x)
  x <- gsub("\\s+", " ", x)
  x <- gsub("^\\s*(.*?)\\s*$", "\\1", x)
  x
}

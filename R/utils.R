#' Parse pdf document as XML.
#' 
#' @param filename pdf file to process
#' @param first first page to process
#' @param last last page to process
#' @return an \code{xml_document} class object from package \code{xml2}
#' @importFrom xml2 read_xml
#' @importFrom Rpoppler PDF_info
#' @export pdf_to_xml
#' @examples
#' unmd_pdf <- system.file(package = "trickypdf", "extdata", "pdf", "UN_Millenium_Declaration.pdf")
#' unmd_xml <- pdf_to_xml(filename = unmd_pdf)
#' 
#' cdu_manifesto_pdf <- system.file(package = "trickypdf", "extdata", "pdf", "cdu.pdf")
#' cdu_manifesto_xml <- pdf_to_xml(filename = cdu_manifesto_pdf)
#' 
#' unga_pdf <- system.file(package = "trickypdf", "extdata", "pdf", "N9586353.pdf")
#' unga_xml <- pdf_to_xml(filename = unga_pdf)
pdf_to_xml = function(filename, first, last){
  
  # ensure that filename exists and is a file
  if (!file.exists(filename)) stop("file does not exist: ", filename)
  if (file.info(filename)[["isdir"]] == TRUE) stop("filename is not a file, but a directory")
  
  no_pages <- Rpoppler::PDF_info(filename)$Pages
  if (missing(first)) first <- 1L
  if (missing(last)) last <- no_pages
  if (last > no_pages) last <- no_pages
  stopifnot(first <= last)
  
  if (grepl("\\s+", filename)) filename <- sprintf('"%s"', filename)
  
  cmd <- c(
    "pdftohtml",
    "-xml", # output for XML post-processing
    "-hidden", # output hidden text
    "-f", first,
    "-l", last,
    "-q", # don't print any messages or errors
    "-stdout", # use standard output
    "-i", # ignore images
    filename
  )
  xml_char <- system(paste(cmd, collapse = " "), intern = TRUE)
  
  xml_parsing_error <- try(
    xml_doc <- xml2::read_xml(x = paste(xml_char, collapse = "\n")),
    silent = TRUE
  )
  if (class(xml_parsing_error)[1] == "try-error") {
    warning(
      "invalid XML - xml2::read_xml cannot parse XML generated from document ",
      filename
    )
    # running xml2::read_xml with options = "RECOVER" does (not yet?) work
    xmllint_present <- try(
      system("xmllint --version", intern = TRUE, ignore.stderr = TRUE),
      silent = TRUE
    )
    if (class(xmllint_present) == "try-error"){
      stop("command line tool 'xmllint' is not present to recover corrupt XML file (installation on Ubuntu: apt-get install libxml2-utils")
    } else {
      message("xmllint found - using command line tool to recover corrupt XML")
    }
    writeLines(text = xml_char, con = xmllint_input <- tempfile())
    xmllint_xml <- system(
      paste0(c("xmllint", "--recover", xmllint_input), collapse = " "),
      intern = TRUE
    )
    xml_parsing_error <- try(xml_doc <- xml2::read_xml(x = paste0(xmllint_xml, collapse = "\n")))
    if (class(xml_parsing_error)[1] == "try-error"){
      stop("tried to recover corrupt XML output from pdftohtml with xmllint --recover without success")
    }
  }
  xml_doc
}



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
restore_paragraphs <- function(x, skipRegexCurrent = "^\\s*[\u2022A-Z(]", skipRegexPrevious = "[\\.?!)]\\s*$"){
  
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
